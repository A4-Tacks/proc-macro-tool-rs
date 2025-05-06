#![doc = include_str!("../README.md")]

extern crate proc_macro;

use std::{
    collections::VecDeque,
    iter::{once, FusedIterator},
};

use proc_macro::{
    Delimiter, Group, Ident, Literal, Punct,
    Spacing::*,
    Span, TokenStream, TokenTree,
};

/// Generate a function, set input `TokenTree` span
pub fn span_setter<T>(span: Span) -> impl Fn(T) -> T
where T: SetSpan,
{
    move |tt| {
        tt.set_spaned(span)
    }
}

pub trait SetSpan: Sized {
    /// Call [`TokenTree::set_span`]
    fn set_span(&mut self, span: Span);

    fn set_spaned(mut self, span: Span) -> Self {
        self.set_span(span);
        self
    }
}
macro_rules! impl_set_span {
    ($ty:ty) => {
        impl SetSpan for $ty {
            fn set_span(&mut self, span: Span) {
                self.set_span(span);
            }
        }
    };
}
impl_set_span!(TokenTree);
impl_set_span!(Ident);
impl_set_span!(Punct);
impl_set_span!(Group);
impl_set_span!(Literal);

/// `<TokenStream as FromIterator<TokenTree>>::from_iter`
#[must_use]
pub fn stream<I>(iter: I) -> TokenStream
where I: IntoIterator<Item = TokenTree>,
{
    TokenStream::from_iter(iter)
}

/// `<TokenStream as FromIterator<TokenStream>>::from_iter`
#[must_use]
pub fn streams<I>(iter: I) -> TokenStream
where I: IntoIterator<Item = TokenStream>,
{
    TokenStream::from_iter(iter)
}

/// Make `compile_error! {"..."}`
#[must_use]
pub fn err(msg: &str, span: Span) -> TokenStream {
    let s = span_setter(span);
    stream([
        s(Punct::new(':', Joint).into()),
        s(Punct::new(':', Joint).into()),
        s(Ident::new("core", span).into()),
        s(Punct::new(':', Joint).into()),
        s(Punct::new(':', Joint).into()),
        s(Ident::new("compile_error", span).into()),
        s(Punct::new('!', Joint).into()),
        s(Group::new(Delimiter::Brace, stream([
            s(Literal::string(msg).into()),
        ])).into()),
    ])
}

/// Like [`err()`], but use [`Result`]
///
/// # Errors
/// - always return [`Err`]
pub fn rerr<T>(msg: &str, span: Span) -> Result<T, TokenStream> {
    Err(err(msg, span))
}

/// Make puncts, `spacing` is last punct spacing
///
/// - `"+-"` like `[Joint('+'), Joint('-')]`
/// - `"+- "` like `[Joint('+'), Alone('-')]`
/// - `"+ -"` like `[Alone('+'), Joint('-')]`
pub fn puncts(puncts: impl AsRef<[u8]>) -> TokenStream {
    puncts_spanned(puncts, Span::call_site())
}

/// Make puncts, `spacing` is last punct spacing
///
/// Like [`puncts`], but `.set_span(span)`
pub fn puncts_spanned(puncts: impl AsRef<[u8]>, span: Span) -> TokenStream {
    let puncts = puncts.as_ref().trim_ascii_start();
    let iter = &mut puncts.iter().copied().peekable();
    let mut result = TokenStream::new();

    while let Some(ch) = iter.next() {
        debug_assert!(! ch.is_ascii_whitespace());
        let mut s = None;
        while iter.next_if(u8::is_ascii_whitespace).is_some() {
            s = Some(Alone)
        }
        let spacing = s.or(iter.peek().map(|_| Joint))
            .unwrap_or(Joint);
        let p = Punct::new(ch.into(), spacing);
        result.push(p.set_spaned(span).into());
    }

    result
}

/// [`return err(msg [, span])`](err())
#[macro_export]
macro_rules! err {
    ($msg:expr $(,)?) => { $crate::err!($msg, $crate::Span::call_site()) };
    ($msg:expr , $($span:expr)? $(,)?) => {
        return $crate::err($msg, $span)
    };
}

/// [`return rerr(msg [, span])`](rerr())
#[macro_export]
macro_rules! rerr {
    ($msg:expr $(,)?) => { $crate::rerr!($msg, $crate::Span::call_site()) };
    ($msg:expr , $($span:expr)? $(,)?) => {
        return $crate::rerr($msg, $span)
    };
}

pub trait TokenStreamExt
    : Default
    + Extend<TokenTree>
    + Extend<TokenStream>
    + IntoIterator<Item = TokenTree>
    + Sized
{
    fn push(&mut self, tt: TokenTree) -> &mut Self {
        self.extend(once(tt));
        self
    }

    fn add(&mut self, stream: TokenStream) -> &mut Self {
        self.extend(once(stream));
        self
    }

    fn parse_iter(self) -> ParseIter<Self::IntoIter> {
        ParseIter { iter: self.into_iter(), buf: VecDeque::new() }
    }

    /// Split [`TokenStream`] to `predicate` false and true
    ///
    /// Like `"+-,-+".split_puncts(",")` -> `("+-", "-+")`
    fn split_puncts(self, puncts: impl AsRef<[u8]>) -> (
        Self,
        ParseIter<Self::IntoIter>,
    )
    {
        let mut left = Self::default();
        let puncts = puncts.as_ref();

        let mut iter = self.parse_iter();
        while iter.next_puncts(puncts).is_none() {
            left.push(iter.next().unwrap());
        }

        (left, iter)
    }
}
impl TokenStreamExt for TokenStream { }

pub trait TokenTreeExt: Sized {
    fn as_ident(&self) -> Option<&Ident>;
    fn as_punct(&self) -> Option<&Punct>;
    fn as_group(&self) -> Option<&Group>;
    fn as_literal(&self) -> Option<&Literal>;
    fn into_ident(self) -> Result<Ident, Self>;
    fn into_punct(self) -> Result<Punct, Self>;
    fn into_group(self) -> Result<Group, Self>;
    fn into_literal(self) -> Result<Literal, Self>;

    fn is_ident(&self) -> bool {
        self.as_ident().is_some()
    }

    fn is_punct(&self) -> bool {
        self.as_punct().is_some()
    }

    fn is_group(&self) -> bool {
        self.as_group().is_some()
    }

    fn is_literal(&self) -> bool {
        self.as_literal().is_some()
    }

    /// Ident content equal to `keyword` str
    ///
    /// Other return `false` when `self` is not [`Ident`]
    fn is_keyword(&self, keyword: &str) -> bool {
        self.as_ident().is_some_and(|i| i.to_string() == keyword)
    }

    /// Punct char equal to `ch`
    ///
    /// Other return `false` when `self` is not [`Punct`]
    fn is_punch(&self, ch: char) -> bool {
        self.as_punct().is_some_and(|p| p.as_char() == ch)
    }

    /// Group delimiter is not [`Delimiter::None`]
    ///
    /// Other return `false` when `self` is not [`Group`]
    fn is_solid_group(&self) -> bool {
        self.as_group().is_some_and(|g| g.is_solid_group())
    }

    /// Punct spacing is [`Joint`]
    ///
    /// Other return `false` when `self` is not [`Punct`]
    fn is_joint(&self) -> bool {
        self.as_punct().is_some_and(|p| p.spacing() == Joint)
    }

    fn as_punct_char(&self) -> Option<char> {
        self.as_punct().map(|p| p.as_char())
    }
}
impl TokenTreeExt for TokenTree {
    fn as_ident(&self) -> Option<&Ident> {
        match self {
            TokenTree::Ident(i) => Some(i),
            _ => None,
        }
    }

    fn as_punct(&self) -> Option<&Punct> {
        match self {
            TokenTree::Punct(i) => Some(i),
            _ => None,
        }
    }

    fn as_group(&self) -> Option<&Group> {
        match self {
            TokenTree::Group(i) => Some(i),
            _ => None,
        }
    }

    fn as_literal(&self) -> Option<&Literal> {
        match self {
            TokenTree::Literal(i) => Some(i),
            _ => None,
        }
    }

    fn into_ident(self) -> Result<Ident, Self> {
        match self {
            TokenTree::Ident(i) => Ok(i),
            _ => Err(self),
        }
    }

    fn into_punct(self) -> Result<Punct, Self> {
        match self {
            TokenTree::Punct(i) => Ok(i),
            _ => Err(self),
        }
    }

    fn into_group(self) -> Result<Group, Self> {
        match self {
            TokenTree::Group(i) => Ok(i),
            _ => Err(self),
        }
    }

    fn into_literal(self) -> Result<Literal, Self> {
        match self {
            TokenTree::Literal(i) => Ok(i),
            _ => Err(self),
        }
    }
}

pub trait GroupExt {
    /// Group delimiter is not [`Delimiter::None`]
    fn is_solid_group(&self) -> bool;
}
impl GroupExt for Group {
    fn is_solid_group(&self) -> bool {
        self.delimiter() != Delimiter::None
    }
}

#[derive(Debug, Clone)]
pub struct ParseIter<I: Iterator<Item = TokenTree>> {
    iter: I,
    buf: VecDeque<TokenTree>,
}

impl<I: Iterator<Item = TokenTree>> ParseIter<I> {
    pub fn peek(&mut self) -> Option<&TokenTree> {
        self.peek_i(0)
    }

    pub fn next_if<F>(&mut self, f: F) -> Option<TokenTree>
    where F: FnOnce(&TokenTree) -> bool,
    {
        let peek = self.peek()?;

        if f(peek) {
            self.next()
        } else {
            None
        }
    }

    pub fn peek_i(&mut self, i: usize) -> Option<&TokenTree> {
        for _ in self.buf.len()..=i {
            self.buf.push_back(self.iter.next()?);
        }
        Some(&self.buf[i])
    }

    pub fn peek_is<F>(&mut self, f: F) -> bool
    where F: FnOnce(&TokenTree) -> bool,
    {
        self.peek().is_some_and(f)
    }

    pub fn peek_i_is<F>(&mut self, i: usize, f: F) -> bool
    where F: FnOnce(&TokenTree) -> bool,
    {
        self.peek_i(i).is_some_and(f)
    }

    /// Peek jointed puncts
    pub fn peek_puncts(&mut self, puncts: impl AsRef<[u8]>) -> Option<
        impl Iterator<Item = &TokenTree>
    > {
        let mut prev = None;

        for (i, ch) in puncts.as_ref().iter()
            .copied().map(char::from).enumerate()
        {
            if let Some(prev) = prev {
                if prev == Alone { return None }
            }

            let tt = self.peek_i(i)?;
            let TokenTree::Punct(p) = tt else { return None };
            if p.as_char() != ch { return None }

            prev = Some(p.spacing());
        }
        Some(self.buf.iter())
    }

    /// Next jointed puncts
    pub fn next_puncts(&mut self, puncts: impl AsRef<[u8]>) -> Option<
        impl Iterator<Item = TokenTree> + '_
    > {
        let _ = self.peek_puncts(puncts.as_ref())?;
        Some(self.buf.drain(..puncts.as_ref().len()))
    }
}
pub trait ParseIterExt: Iterator<Item = TokenTree> + Sized {
    fn parse_iter(self) -> ParseIter<Self> {
        ParseIter { iter: self, buf: VecDeque::new() }
    }
}
impl<I: Iterator<Item = TokenTree>> ParseIterExt for I { }

impl<I: Iterator<Item = TokenTree>> Iterator for ParseIter<I> {
    type Item = TokenTree;

    fn next(&mut self) -> Option<Self::Item> {
        self.buf.pop_front()
            .or_else(|| self.iter.next())
    }

    fn count(self) -> usize
    where Self: Sized,
    {
        self.buf.len() + self.iter.count()
    }

    fn fold<B, F>(self, init: B, f: F) -> B
    where Self: Sized,
          F: FnMut(B, Self::Item) -> B,
    {
        self.buf.into_iter().chain(self.iter).fold(init, f)
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let (lo, hi) = self.iter.size_hint();
        let lo = lo.saturating_add(self.buf.len());
        let hi = hi.and_then(|hi| hi.checked_add(self.buf.len()));
        (lo, hi)
    }
}
impl<I: DoubleEndedIterator<Item = TokenTree>> DoubleEndedIterator for ParseIter<I> {
    fn next_back(&mut self) -> Option<Self::Item> {
        self.iter.next_back()
            .or_else(|| self.buf.pop_back())
    }

    fn rfold<B, F>(self, init: B, f: F) -> B
    where Self: Sized,
          F: FnMut(B, Self::Item) -> B,
    {
        self.buf.into_iter().chain(self.iter).rfold(init, f)
    }
}
impl<I: ExactSizeIterator<Item = TokenTree>> ExactSizeIterator for ParseIter<I> { }
impl<I: FusedIterator<Item = TokenTree>> FusedIterator for ParseIter<I> { }

fn pfunc_impl<F, R>(
    input: TokenStream,
    proc_input: bool,
    names: &[&str],
    f: &mut F,
) -> Result<TokenStream, R>
where F: FnMut(Ident, Group) -> Result<TokenStream, R>,
{
    let mut iter = input.into_iter().parse_iter();
    let mut result = TokenStream::new();

    while let Some(tt) = iter.next() {
        match tt {
            TokenTree::Punct(p)
                if p.as_char() == '#'
                && iter.peek_is(|i| i.as_ident()
                    .is_some_and(|i| names.contains(&&*i.to_string())))
                && iter.peek_i_is(1, |t| t.is_solid_group())
                =>
            {
                let ident = iter.next().unwrap().into_ident().unwrap();
                let mut group = iter.next().unwrap().into_group().unwrap();
                if proc_input {
                    let sub = pfunc_impl(
                        group.stream(), proc_input, names, f)?;
                    group = Group::new(group.delimiter(), sub)
                        .set_spaned(group.span());
                }
                result.add(f(ident, group)?);
            },
            TokenTree::Group(g) => {
                let sub = pfunc_impl(g.stream(), proc_input, names, f)?;
                let tt = Group::new(g.delimiter(), sub);
                result.push(tt.set_spaned(g.span()).into());
            },
            _ => _ = result.push(tt),
        }
    }

    Ok(result)
}

/// Call `f` on `#name(...)` `#name[...]` etc, exclude [`Delimiter::None`]
///
/// Apply pfunc for `(...)` when `proc_input` is `true`
pub fn pfunc<'a>(
    input: TokenStream,
    proc_input: bool,
    names: impl AsRef<[&'a str]>,
    mut f: impl FnMut(Ident, Group) -> TokenStream,
) -> TokenStream {
    let f = &mut |i, g| {
        Ok::<_, ()>(f(i, g))
    };
    pfunc_impl(input, proc_input, names.as_ref(), f).unwrap()
}

/// Call `f` on `#name(...)` `#name[...]` etc, exclude [`Delimiter::None`]
///
/// Apply pfunc for `(...)` when `proc_input` is `true`
pub fn try_pfunc<'a, R>(
    input: TokenStream,
    proc_input: bool,
    names: impl AsRef<[&'a str]>,
    mut f: impl FnMut(Ident, Group) -> Result<TokenStream, R>,
) -> Result<TokenStream, R> {
    pfunc_impl(input, proc_input, names.as_ref(), &mut f)
}

use proc_macro::{
    Delimiter, Group, Ident, Literal, Punct, Spacing::*, Span, TokenStream,
    TokenTree,
};

use crate::{
    GetSpan, ParseIter, ParseIterExt as _, SetSpan, TokenStreamExt as _,
    TokenTreeExt as _,
};

/// Create [`TokenStream`] from
/// [`IntoIterator<Item = TokenTree>`](IntoIterator)
#[must_use]
pub fn stream<I>(iter: I) -> TokenStream
where I: IntoIterator<Item = TokenTree>,
{
    TokenStream::from_iter(iter)
}

/// Create [`TokenStream`] from
/// [`IntoIterator<Item = TokenStream>`](IntoIterator)
#[must_use]
pub fn streams<I>(iter: I) -> TokenStream
where I: IntoIterator<Item = TokenStream>,
{
    TokenStream::from_iter(iter)
}

fn pfunc_predicate<I>(names: &[&str], p: &Punct, iter: &mut ParseIter<I>) -> bool
where I: Iterator<Item = TokenTree>,
{
    p.as_char() == '#'
        && iter.peek_is(|i| i.as_ident()
            .is_some_and(|i| names.contains(&&*i.to_string())))
        && iter.peek_i_is(1, |t| t.is_solid_group())
}

fn subtree_contain_pfunc(names: &[&str], stream: impl IntoIterator<Item = TokenTree>) -> bool {
    let iter = &mut stream.parse_iter();

    while let Some(tt) = iter.next() {
        match tt {
            TokenTree::Punct(p) if pfunc_predicate(names, &p, iter) => {
                return true;
            },
            TokenTree::Group(g)
                if subtree_contain_pfunc(names, g.stream()) =>
            {
                return true;
            },
            _ => {},
        }
    }

    false
}

fn pfunc_impl<F, R>(
    stream: impl IntoIterator<Item = TokenTree>,
    proc_input: bool,
    names: &[&str],
    lossless: bool,
    f: &mut F,
) -> Result<TokenStream, R>
where F: FnMut(Ident, Group) -> Result<TokenStream, R>,
{
    let iter = &mut stream.parse_iter();
    let mut result = TokenStream::new();

    while let Some(tt) = iter.next() {
        match tt {
            TokenTree::Punct(p) if pfunc_predicate(names, &p, iter) => {
                let ident = iter.next().unwrap().into_ident().unwrap();
                let mut group = iter.next().unwrap().into_group().unwrap();
                if proc_input {
                    let sub = pfunc_impl(
                        group.stream(),
                        proc_input,
                        names,
                        lossless,
                        f,
                    )?;
                    group = sub
                        .grouped(group.delimiter())
                        .set_spaned(group.span());
                }
                result.add(f(ident, group)?);
            },
            TokenTree::Group(g)
                if !lossless || subtree_contain_pfunc(names, g.stream()) =>
            {
                let sub = pfunc_impl(
                    g.stream(),
                    proc_input,
                    names,
                    lossless,
                    f,
                )?;
                result.push(sub
                    .grouped(g.delimiter())
                    .set_spaned(g.span())
                    .into());
            },
            _ => _ = result.push(tt),
        }
    }

    Ok(result)
}

/// Call `f` on `#name(...)` `#name[...]` etc, exclude [`Delimiter::None`]
///
/// Apply pfunc for `(...)` when `proc_input` is `true`
#[allow(clippy::missing_panics_doc)]
pub fn pfunc<'a>(
    stream: impl IntoIterator<Item = TokenTree>,
    proc_input: bool,
    names: impl AsRef<[&'a str]>,
    mut f: impl FnMut(Ident, Group) -> TokenStream,
) -> TokenStream {
    let f = &mut |i, g| {
        Ok::<_, ()>(f(i, g))
    };
    pfunc_impl(stream, proc_input, names.as_ref(), false, f).unwrap()
}

/// Call `f` on `#name(...)` `#name[...]` etc, exclude [`Delimiter::None`]
///
/// Apply pfunc for `(...)` when `proc_input` is `true`
pub fn try_pfunc<'a, R>(
    stream: impl IntoIterator<Item = TokenTree>,
    proc_input: bool,
    names: impl AsRef<[&'a str]>,
    mut f: impl FnMut(Ident, Group) -> Result<TokenStream, R>,
) -> Result<TokenStream, R> {
    pfunc_impl(stream, proc_input, names.as_ref(), false, &mut f)
}

/// Like [`pfunc`], but it's lossless when no changes are made
#[allow(clippy::missing_panics_doc)]
pub fn pfunc_lossless<'a>(
    stream: impl IntoIterator<Item = TokenTree>,
    proc_input: bool,
    names: impl AsRef<[&'a str]>,
    mut f: impl FnMut(Ident, Group) -> TokenStream,
) -> TokenStream {
    let f = &mut |i, g| {
        Ok::<_, ()>(f(i, g))
    };
    pfunc_impl(stream, proc_input, names.as_ref(), true, f).unwrap()
}

/// Like [`try_pfunc`], but it's lossless when no changes are made
pub fn try_pfunc_lossless<'a, R>(
    stream: impl IntoIterator<Item = TokenTree>,
    proc_input: bool,
    names: impl AsRef<[&'a str]>,
    mut f: impl FnMut(Ident, Group) -> Result<TokenStream, R>,
) -> Result<TokenStream, R> {
    pfunc_impl(stream, proc_input, names.as_ref(), true, &mut f)
}

/// Make `compile_error! {"..."}`
#[must_use]
pub fn err(msg: &str, span: impl GetSpan) -> TokenStream {
    let s = span_setter(span.span());

    stream([
        Punct::new(':', Joint).into(),
        Punct::new(':', Joint).into(),
        Ident::new("core", span.span()).into(),
        Punct::new(':', Joint).into(),
        Punct::new(':', Joint).into(),
        Ident::new("compile_error", span.span()).into(),
        Punct::new('!', Joint).into(),
        Group::new(Delimiter::Brace, stream([
            Literal::string(msg).into(),
        ].map(s))).into(),
    ].map(s))
}

/// Like [`err()`], but use [`Result`]
///
/// # Errors
/// - always return [`Err`]
pub fn rerr<T>(msg: &str, span: impl GetSpan) -> Result<T, TokenStream> {
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

/// Generate a function, set input `TokenTree` span
pub fn span_setter<T>(span: Span) -> impl Fn(T) -> T + Copy
where T: SetSpan,
{
    move |tt| {
        tt.set_spaned(span)
    }
}

/// Like [`Group::new(Delimiter::Parenthesis, iter)`](Group::new)
pub fn paren<I>(iter: I) -> Group
where I: IntoIterator,
      TokenStream: FromIterator<I::Item>,
{
    iter.into_iter()
        .collect::<TokenStream>()
        .grouped_paren()
}

/// Like [`Group::new(Delimiter::Brace, iter)`](Group::new)
pub fn brace<I>(iter: I) -> Group
where I: IntoIterator,
      TokenStream: FromIterator<I::Item>,
{
    iter.into_iter()
        .collect::<TokenStream>()
        .grouped_brace()
}

/// Like [`Group::new(Delimiter::Bracket, iter)`](Group::new)
pub fn bracket<I>(iter: I) -> Group
where I: IntoIterator,
      TokenStream: FromIterator<I::Item>,
{
    iter.into_iter()
        .collect::<TokenStream>()
        .grouped_bracket()
}

/// Like [`Group::new(Delimiter::None, iter)`](Group::new)
pub fn none<I>(iter: I) -> Group
where I: IntoIterator,
      TokenStream: FromIterator<I::Item>,
{
    iter.into_iter()
        .collect::<TokenStream>()
        .grouped_none()
}

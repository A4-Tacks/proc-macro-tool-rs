use core::{iter::once, mem::take};

use crate::{
    puncts, puncts_spanned, ParseIter, ParseIterExt as _, SetSpan as _,
    TokenKind,
};
use proc_macro::{
    Delimiter, Group, Ident, Literal, Punct,
    Spacing::{self, *},
    Span, TokenStream, TokenTree,
};

pub trait TokenStreamExt
    : Default
    + Extend<TokenTree>
    + Extend<TokenStream>
    + IntoIterator<Item = TokenTree>
    + Sized
{
    /// Extend a [`TokenTree`]
    fn push(&mut self, tt: TokenTree) -> &mut Self {
        self.extend(once(tt));
        self
    }

    /// Extend a [`TokenStream`]
    fn add(&mut self, stream: TokenStream) -> &mut Self {
        self.extend(once(stream));
        self
    }

    /// Call [`mem::take`](std::mem::take)
    #[must_use]
    fn take(&mut self) -> Self {
        take(self)
    }

    /// Call [`Group::new`]
    fn grouped(self, delimiter: Delimiter) -> Group;

    fn grouped_paren(self) -> Group {
        self.grouped(Delimiter::Parenthesis)
    }

    fn grouped_brace(self) -> Group {
        self.grouped(Delimiter::Brace)
    }

    fn grouped_bracket(self) -> Group {
        self.grouped(Delimiter::Bracket)
    }

    fn grouped_none(self) -> Group {
        self.grouped(Delimiter::None)
    }

    /// Split [`TokenStream`] to `predicate` false and true
    ///
    /// Like `"+-,-+".split_puncts(",")` -> `("+-", "-+")`
    fn split_puncts(self, puncts: impl AsRef<[u8]>) -> Option<(
        Self,
        ParseIter<Self::IntoIter>,
    )>;
}
impl TokenStreamExt for TokenStream {
    fn grouped(self, delimiter: Delimiter) -> Group {
        Group::new(delimiter, self)
    }

    fn split_puncts(self, puncts: impl AsRef<[u8]>) -> Option<(
        Self,
        ParseIter<Self::IntoIter>,
    )>
    {
        let mut iter = self.parse_iter();
        Some((iter.split_puncts(puncts)?, iter))
    }

}

/// Remake each subtree methods
pub trait WalkExt
    : IntoIterator<Item = TokenTree>
    + FromIterator<TokenTree>
{
    /// Remake each subtree
    ///
    /// `"(1+2)*3"` -> call `f` on `1`, `+`, `2`, `(f(1) f(+) f(2))`, `*`, `3`
    #[must_use]
    fn walk<F>(self, mut f: F) -> Self
    where F: FnMut(TokenTree) -> TokenTree
    {
        fn walk_impl<I, F>(this: I, f: &mut F) -> I
        where I: IntoIterator<Item = TokenTree> + FromIterator<TokenTree>,
              F: FnMut(TokenTree) -> TokenTree
        {
            this.into_iter()
                .map(|tt| {
                    let tt = match tt {
                        TokenTree::Group(g) => {
                            g.map(|this| walk_impl(this, f)).tt()
                        },
                        _ => tt,
                    };
                    f(tt)
                })
                .collect()
        }
        walk_impl(self, &mut f)
    }
}
impl<I: IntoIterator<Item = TokenTree> + FromIterator<TokenTree>> WalkExt for I { }

pub trait TokenTreeExt: Into<TokenTree> + Sized {
    fn as_ident(&self) -> Option<&Ident>     { None }
    fn as_punct(&self) -> Option<&Punct>     { None }
    fn as_group(&self) -> Option<&Group>     { None }
    fn as_literal(&self) -> Option<&Literal> { None }
    fn into_ident(self) -> Result<Ident, Self>     { Err(self) }
    fn into_punct(self) -> Result<Punct, Self>     { Err(self) }
    fn into_group(self) -> Result<Group, Self>     { Err(self) }
    fn into_literal(self) -> Result<Literal, Self> { Err(self) }

    fn to_ident(&self) -> Result<&Ident, &Self> {
        self.as_ident().ok_or(self)
    }

    fn to_punct(&self) -> Result<&Punct, &Self> {
        self.as_punct().ok_or(self)
    }

    fn to_group(&self) -> Result<&Group, &Self> {
        self.as_group().ok_or(self)
    }

    fn to_literal(&self) -> Result<&Literal, &Self> {
        self.as_literal().ok_or(self)
    }

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

    fn kind(&self) -> TokenKind {
        if self.is_literal() {
            TokenKind::Literal
        } else if self.is_punct() {
            TokenKind::Punct
        } else if self.is_group() {
            TokenKind::Group
        } else if self.is_ident() {
            TokenKind::Ident
        } else {
            unimplemented!()
        }
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

    /// Group delimiter equal to `delimiter`
    ///
    /// Other return `false` when `self` is not [`Group`]
    fn is_delimiter(&self, delimiter: Delimiter) -> bool {
        self.as_group().is_some_and(|g| g.is_delimiter(delimiter))
    }

    /// Like [`self.is_delimiter(Delimiter::Parenthesis)`](#method.is_delimiter)
    fn is_delimiter_paren(&self) -> bool {
        self.is_delimiter(Delimiter::Parenthesis)
    }

    /// Like [`self.is_delimiter(Delimiter::Brace)`](#method.is_delimiter)
    fn is_delimiter_brace(&self) -> bool {
        self.is_delimiter(Delimiter::Brace)
    }

    /// Like [`self.is_delimiter(Delimiter::Bracket)`](#method.is_delimiter)
    fn is_delimiter_bracket(&self) -> bool {
        self.is_delimiter(Delimiter::Bracket)
    }

    /// Like [`self.is_delimiter(Delimiter::None)`](#method.is_delimiter)
    fn is_delimiter_none(&self) -> bool {
        self.is_delimiter(Delimiter::None)
    }

    /// Call [`Group::stream`] when [`delimiter`] is [`Delimiter::Parenthesis`]
    ///
    /// Other return `false` when `self` is not [`Group`]
    ///
    /// [`delimiter`]: Group::delimiter
    fn to_paren_stream(&self) -> Result<TokenStream, &Self> {
        self.as_group()
            .and_then(|g| g.is_delimiter_paren().then(|| g.stream()))
            .ok_or(self)
    }

    /// Call [`Group::stream`] when [`delimiter`] is [`Delimiter::Brace`]
    ///
    /// Other return `false` when `self` is not [`Group`]
    ///
    /// [`delimiter`]: Group::delimiter
    fn to_brace_stream(&self) -> Result<TokenStream, &Self> {
        self.as_group()
            .and_then(|g| g.is_delimiter_brace().then(|| g.stream()))
            .ok_or(self)
    }

    /// Call [`Group::stream`] when [`delimiter`] is [`Delimiter::Bracket`]
    ///
    /// Other return `false` when `self` is not [`Group`]
    ///
    /// [`delimiter`]: Group::delimiter
    fn to_bracket_stream(&self) -> Result<TokenStream, &Self> {
        self.as_group()
            .and_then(|g| g.is_delimiter_bracket().then(|| g.stream()))
            .ok_or(self)
    }

    /// Call [`Group::stream`] when [`delimiter`] is [`Delimiter::None`]
    ///
    /// Other return `false` when `self` is not [`Group`]
    ///
    /// [`delimiter`]: Group::delimiter
    fn to_none_stream(&self) -> Result<TokenStream, &Self> {
        self.as_group()
            .and_then(|g| g.is_delimiter_none().then(|| g.stream()))
            .ok_or(self)
    }

    /// Like [`to_paren_stream`](#method.to_paren_stream),
    /// but using `Self` instead of `&Self`
    fn into_paren_stream(self) -> Result<TokenStream, Self> {
        self.to_paren_stream()
            .ok().ok_or(self)
    }

    /// Like [`to_brace_stream`](#method.to_brace_stream),
    /// but using `Self` instead of `&Self`
    fn into_brace_stream(self) -> Result<TokenStream, Self> {
        self.to_brace_stream()
            .ok().ok_or(self)
    }

    /// Like [`to_bracket_stream`](#method.to_bracket_stream),
    /// but using `Self` instead of `&Self`
    fn into_bracket_stream(self) -> Result<TokenStream, Self> {
        self.to_bracket_stream()
            .ok().ok_or(self)
    }

    /// Like [`to_none_stream`](#method.to_none_stream),
    /// but using `Self` instead of `&Self`
    fn into_none_stream(self) -> Result<TokenStream, Self> {
        self.to_none_stream()
            .ok().ok_or(self)
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

    /// [`Into`] [`TokenTree`], like [`TokenTree::from(self)`]
    ///
    /// [`TokenTree::from(self)`]: TokenTree::from
    fn tt(self) -> TokenTree {
        self.into()
    }

    /// [`TokenStream::from_iter(self.tt())`](TokenStream::from_iter)
    fn unit_stream(self) -> TokenStream {
        self.tt().into()
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

    fn kind(&self) -> TokenKind {
        self.into()
    }
}
macro_rules! impl_token_tree_ext {
    ($as:ident, $into:ident, $ty:ident) => {
        impl TokenTreeExt for $ty {
            fn $as(&self) -> Option<&$ty> {
                Some(self)
            }
            fn $into(self) -> Result<$ty, Self> {
                Ok(self)
            }
            fn kind(&self) -> TokenKind {
                TokenKind::$ty
            }
        }
    };
}
impl_token_tree_ext!(as_ident,   into_ident,   Ident);
impl_token_tree_ext!(as_punct,   into_punct,   Punct);
impl_token_tree_ext!(as_literal, into_literal, Literal);
impl TokenTreeExt for Group {
    fn as_group(&self) -> Option<&Group> {
        Some(self)
    }

    fn into_group(self) -> Result<Group, Self> {
        Ok(self)
    }

    fn kind(&self) -> TokenKind {
        TokenKind::Group
    }

    fn is_solid_group(&self) -> bool {
        self.delimiter() != Delimiter::None
    }

    fn is_delimiter(&self, delimiter: Delimiter) -> bool {
        self.delimiter() == delimiter
    }
}

/// Create unsuffixed [`Literal`]
pub trait Unsuffixed {
    fn unsuffixed(self) -> Literal;
}
/// Create suffixed [`Literal`]
pub trait Suffixed {
    fn suffixed(self) -> Literal;
}
macro_rules! impl_unsuffixes {
    ( $($ty:ty: $unsuffixed:ident $($suffixed:ident)?);+ $(;)? ) => {
        $(
            #[doc = concat!(
                "Call [`Literal::",
                stringify!($unsuffixed),
                "`]",
            )]
            impl Unsuffixed for $ty {
                fn unsuffixed(self) -> Literal {
                    Literal::$unsuffixed(self)
                }
            }

            $(
                #[doc = concat!(
                    "Call [`Literal::",
                    stringify!($suffixed),
                    "`]",
                )]
                impl Suffixed for $ty {
                    fn suffixed(self) -> Literal {
                        Literal::$suffixed(self)
                    }
                }
            )?
        )*
    };
}
impl_unsuffixes! {
    i8:     i8_unsuffixed       i8_suffixed;
    i16:    i16_unsuffixed      i16_suffixed;
    i32:    i32_unsuffixed      i32_suffixed;
    i64:    i64_unsuffixed      i64_suffixed;
    i128:   i128_unsuffixed     i128_suffixed;
    u8:     u8_unsuffixed       u8_suffixed;
    u16:    u16_unsuffixed      u16_suffixed;
    u32:    u32_unsuffixed      u32_suffixed;
    u64:    u64_unsuffixed      u64_suffixed;
    u128:   u128_unsuffixed     u128_suffixed;
    f32:    f32_unsuffixed      f32_suffixed;
    f64:    f64_unsuffixed      f64_suffixed;
    usize:  usize_unsuffixed    usize_suffixed;
    isize:  isize_unsuffixed    isize_suffixed;
    char:   character;
    &str:   string;
    &[u8]:  byte_string;
}

pub trait PunctsExt: AsRef<[u8]> {
    /// Call [`puncts`]
    fn puncts(&self) -> TokenStream {
        puncts(self)
    }

    /// Call [`puncts_spanned`]
    fn puncts_spanned(&self, span: Span) -> TokenStream {
        puncts_spanned(self, span)
    }
}
impl<T: AsRef<[u8]> + ?Sized> PunctsExt for T { }

pub trait PunctExt: Sized {
    fn punct(self, spacing: Spacing) -> Punct;

    /// Like [`.punct(Joint)`](#method.punct)
    fn joint(self) -> Punct {
        self.punct(Joint)
    }

    /// Like [`.punct(Alone)`](#method.punct)
    fn alone(self) -> Punct {
        self.punct(Alone)
    }
}
impl PunctExt for char {
    /// Call [`Punct::new`]
    fn punct(self, spacing: Spacing) -> Punct {
        Punct::new(self, spacing)
    }
}

pub trait StrExt {
    fn ident(&self, span: Span) -> Ident;
}
impl StrExt for str {
    /// Call [`Ident::new`]
    fn ident(&self, span: Span) -> Ident {
        Ident::new(self, span)
    }
}

pub trait GroupExt {
    #[must_use]
    fn map<F>(&self, f: F) -> Self
    where F: FnOnce(TokenStream) -> TokenStream;
}
impl GroupExt for Group {
    fn map<F>(&self, f: F) -> Self
    where F: FnOnce(TokenStream) -> TokenStream,
    {
        f(self.stream())
            .grouped(self.delimiter())
            .set_spaned(self.span())
    }
}

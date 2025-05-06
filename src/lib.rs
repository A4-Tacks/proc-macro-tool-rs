#![doc = include_str!("../README.md")]
#![warn(clippy::unused_trait_names, clippy::pedantic)]
#![allow(
    clippy::redundant_closure_for_method_calls,
    clippy::enum_glob_use,
    clippy::missing_errors_doc,
    clippy::semicolon_if_nothing_returned,
)]

extern crate proc_macro;

mod span;
mod parse_iter;
mod func_utils;

pub use span::*;
pub use parse_iter::*;
pub use func_utils::*;

use std::{iter::once, mem::take};

use proc_macro::{
    Delimiter, Group, Ident, Literal, Punct, Spacing::*, TokenStream,
    TokenTree,
};

/// [`return err(msg [, span])`](err())
///
/// `err!(@(...))` like `err!(&format!(...))`
#[macro_export]
macro_rules! err {
    ($msg:expr $(,)?) => { $crate::err!($msg, ::proc_macro::Span::call_site()) };
    ($msg:expr , $span:expr $(,)?) => {
        return $crate::err($msg, &$span)
    };
    (@($($f:tt)*) $($rest:tt)*) => {
        $crate::err!(&::std::format!($($f)*) $($rest)*)
    };
}

/// [`return rerr(msg [, span])`](rerr())
///
/// `err!(@(...))` like `err!(&format!(...))`
#[macro_export]
macro_rules! rerr {
    ($msg:expr $(,)?) => { $crate::rerr!($msg, ::proc_macro::Span::call_site()) };
    ($msg:expr , $span:expr $(,)?) => {
        return $crate::rerr($msg, &$span)
    };
    (@($($f:tt)*) $($rest:tt)*) => {
        $crate::rerr!(&::std::format!($($f)*) $($rest)*)
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

    fn take(&mut self) -> Self {
        take(self)
    }

    fn grouped(self, delimiter: Delimiter) -> Group;

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
                            walk_impl(g.stream(), &mut *f)
                                .grouped(g.delimiter())
                                .set_spaned(g.span())
                                .into()
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

pub trait TokenTreeExt: Sized {
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
macro_rules! impl_token_tree_ext {
    ($as:ident, $into:ident, $ty:ty) => {
        impl TokenTreeExt for $ty {
            fn $as(&self) -> Option<&$ty> {
                Some(self)
            }
            fn $into(self) -> Result<$ty, Self> {
                Ok(self)
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

    fn is_solid_group(&self) -> bool {
        self.delimiter() != Delimiter::None
    }

    fn is_delimiter(&self, delimiter: Delimiter) -> bool {
        self.delimiter() == delimiter
    }
}

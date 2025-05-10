use proc_macro::{Group, Ident, Literal, Punct, TokenTree};

use crate::TokenTreeExt as _;

/// Enum to [`TokenTree`] variants
///
/// # Examples
/// ```no_run
/// # extern crate proc_macro;
/// # use proc_macro_tool::{TokenKind, TokenTreeExt as _};
/// # use proc_macro::{Punct, TokenTree, Spacing::*};
/// let token = TokenTree::from(Punct::new('+', Alone));
/// assert_eq!(token.kind(), TokenKind::Punct);
/// ```
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TokenKind {
    /// [`TokenTree::Group`]
    Group,
    /// [`TokenTree::Ident`]
    Ident,
    /// [`TokenTree::Punct`]
    Punct,
    /// [`TokenTree::Literal`]
    Literal,
}

impl TokenKind {
    /// Returns `true` if the token kind is [`Group`].
    ///
    /// [`Group`]: TokenKind::Group
    #[must_use]
    pub fn is_group(&self) -> bool {
        matches!(self, Self::Group)
    }

    /// Returns `true` if the token kind is [`Ident`].
    ///
    /// [`Ident`]: TokenKind::Ident
    #[must_use]
    pub fn is_ident(&self) -> bool {
        matches!(self, Self::Ident)
    }

    /// Returns `true` if the token kind is [`Punct`].
    ///
    /// [`Punct`]: TokenKind::Punct
    #[must_use]
    pub fn is_punct(&self) -> bool {
        matches!(self, Self::Punct)
    }

    /// Returns `true` if the token kind is [`Literal`].
    ///
    /// [`Literal`]: TokenKind::Literal
    #[must_use]
    pub fn is_literal(&self) -> bool {
        matches!(self, Self::Literal)
    }
}

impl PartialEq<TokenTree> for TokenKind {
    fn eq(&self, other: &TokenTree) -> bool {
        *self == other.kind()
    }
}
impl PartialEq<TokenKind> for TokenTree {
    fn eq(&self, other: &TokenKind) -> bool {
        self.kind() == *other
    }
}

impl From<&TokenTree> for TokenKind {
    fn from(tt: &TokenTree) -> Self {
        match tt {
            TokenTree::Group(_) => Self::Group,
            TokenTree::Ident(_) => Self::Ident,
            TokenTree::Punct(_) => Self::Punct,
            TokenTree::Literal(_) => Self::Literal,
        }
    }
}
impl From<&mut TokenTree> for TokenKind {
    fn from(tt: &mut TokenTree) -> Self {
        match tt {
            TokenTree::Group(_) => Self::Group,
            TokenTree::Ident(_) => Self::Ident,
            TokenTree::Punct(_) => Self::Punct,
            TokenTree::Literal(_) => Self::Literal,
        }
    }
}
impl From<TokenTree> for TokenKind {
    fn from(tt: TokenTree) -> Self {
        Self::from(&tt)
    }
}
macro_rules! impl_kind {
    ($i:ident) => {
        impl From<$i> for TokenKind {
            fn from(_: $i) -> Self {
                Self::$i
            }
        }
        impl From<&$i> for TokenKind {
            fn from(_: &$i) -> Self {
                Self::$i
            }
        }
        impl From<&mut $i> for TokenKind {
            fn from(_: &mut $i) -> Self {
                Self::$i
            }
        }
    };
}
impl_kind!(Group);
impl_kind!(Ident);
impl_kind!(Punct);
impl_kind!(Literal);

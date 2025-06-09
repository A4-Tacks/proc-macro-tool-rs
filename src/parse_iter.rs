use crate::TokenStreamExt as _;
use proc_macro::{Spacing::*, TokenStream, TokenTree};
use std::{collections::VecDeque, iter::FusedIterator};

/// Create [`ParseIter`]
pub trait ParseIterExt: IntoIterator<Item = TokenTree> + Sized {
    /// Create [`ParseIter`]
    fn parse_iter(self) -> ParseIter<Self::IntoIter> {
        ParseIter { iter: self.into_iter(), buf: VecDeque::new() }
    }
}
impl<I: IntoIterator<Item = TokenTree>> ParseIterExt for I { }

/// Peek `n` iterator adapter
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

    pub fn next_i_if<F>(&mut self, i: usize, f: F) -> Option<TokenTree>
    where F: FnOnce(&TokenTree) -> bool,
    {
        let peek = self.peek_i(i)?;

        if f(peek) {
            self.nth(i)
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

    /// Split [`TokenStream`] to `predicate` false and true
    ///
    /// Like `"+-,-+".split_puncts(",")` -> `("+-", "-+")`
    pub fn split_puncts(&mut self, puncts: impl AsRef<[u8]>) -> Option<TokenStream> {
        let mut left = TokenStream::new();
        let puncts = puncts.as_ref();

        loop {
            if self.next_puncts(puncts).is_some() {
                break Some(left);
            }
            let Some(next) = self.next() else {
                break None;
            };
            left.push(next);
        }
    }
}
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

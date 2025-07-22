use crate::{stream, TokenTreeExt as _};
use proc_macro::{Spacing::*, Span, TokenStream, TokenTree};
use std::{array, collections::VecDeque, iter::{self, FusedIterator}};

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

    /// # Panics
    /// `self.count() < N`
    #[track_caller]
    pub fn next_tts<const N: usize>(&mut self) -> [TokenTree; N] {
        array::from_fn(|_| self.next()
            .expect("unexpected end of input"))
    }

    pub fn peek_i(&mut self, i: usize) -> Option<&TokenTree> {
        for _ in self.buf.len()..=i {
            self.buf.push_back(self.iter.next()?);
        }
        Some(&self.buf[i])
    }

    /// Get current token span. return [`Span::call_site`] when nothing token
    pub fn span(&mut self) -> Span {
        self.peek().map_or_else(Span::call_site, TokenTree::span)
    }

    /// Get `i`th token span. return [`Span::call_site`] when nothing token
    pub fn span_i(&mut self, i: usize) -> Span {
        self.peek_i(i).map_or_else(Span::call_site, TokenTree::span)
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
    pub fn peek_puncts(
        &mut self,
        puncts: impl AsRef<[u8]>,
    ) -> Option<impl Iterator<Item = &TokenTree>> {
        self.peek_i_puncts(0, puncts)
    }

    /// Peek jointed puncts
    pub fn peek_i_puncts(
        &mut self,
        i: usize,
        puncts: impl AsRef<[u8]>,
    ) -> Option<impl Iterator<Item = &TokenTree>> {
        let mut prev = None;

        let puncts = puncts.as_ref();

        for (j, ch) in puncts.iter().copied().map(char::from).enumerate() {
            if let Some(prev) = prev {
                if prev == Alone { return None }
            }

            let tt = self.peek_i(i + j)?;
            let TokenTree::Punct(p) = tt else { return None };
            if p.as_char() != ch { return None }

            prev = Some(p.spacing());
        }
        Some(self.buf.iter().skip(i).take(puncts.len()))
    }

    pub fn is_puncts(&mut self, puncts: impl AsRef<[u8]>) -> bool {
        self.peek_puncts(puncts).is_some()
    }

    pub fn is_puncts_at(&mut self, i: usize, puncts: impl AsRef<[u8]>) -> bool {
        self.peek_i_puncts(i, puncts).is_some()
    }

    /// Next jointed puncts
    pub fn next_puncts(
        &mut self,
        puncts: impl AsRef<[u8]>,
    ) -> Option<impl Iterator<Item = TokenTree> + '_> {
        let puncts = puncts.as_ref();
        let _ = self.peek_puncts(puncts)?;

        Some(self.buf.drain(..puncts.len()))
    }

    /// Split [`TokenStream`] with `puncts`
    ///
    /// Like `"+-,-+".split_puncts(",")` -> `"+-"`
    #[allow(clippy::missing_panics_doc)]
    pub fn split_puncts(&mut self, puncts: impl AsRef<[u8]>) -> Option<TokenStream> {
        let puncts = puncts.as_ref();
        let mut i = 0;

        loop {
            if self.peek_i_puncts(i, puncts).is_some() {
                let left = self.take(i).collect();
                let _ = self.next_puncts(puncts).unwrap();
                break Some(left);
            }
            self.peek_i(i)?;
            i += 1;
        }
    }

    /// Split all [`TokenStream`] with `puncts`
    ///
    /// Like `"+-,-+".split_puncts(",")` -> `"+-", "-+"`
    pub fn split_puncts_all<'a>(
        &'a mut self,
        puncts: impl AsRef<[u8]> + 'a,
    ) -> impl Iterator<Item = TokenStream> + 'a {
        iter::from_fn(move || {
            self.split_puncts(puncts.as_ref())
                .or_else(|| self.peek().is_some().then(|| self.collect()))
        })
    }

    /// # Note
    ///
    /// This method parse like `#[...]`
    pub fn next_attributes(&mut self) -> Vec<TokenStream> {
        let mut attributes = vec![];

        while self.peek_puncts("#").is_some()
            &&self.peek_i_is(1, |tt| tt.is_delimiter_bracket())
        {
            attributes.push(self.next_tts::<2>().into_iter().collect());
        }

        attributes
    }

    /// # Note
    ///
    /// This method parse like `#![...]`
    pub fn next_outer_attributes(&mut self) -> Vec<TokenStream> {
        let mut attributes = vec![];

        while self.peek_puncts("#").is_some()
            &&self.peek_i_is(1, |tt| tt.is_punch('!'))
            &&self.peek_i_is(2, |tt| tt.is_delimiter_bracket())
        {
            attributes.push(stream(self.next_tts::<3>()));
        }

        attributes
    }

    #[allow(clippy::missing_panics_doc)]
    pub fn next_vis(&mut self) -> Option<TokenStream> {
        if self.peek_is(|tt| tt.is_keyword("pub")) {
            if self.peek_i_is(1, |tt| tt.is_delimiter_paren()) {
                return Some(stream(self.next_tts::<2>()));
            }
            return Some(self.next().unwrap().into());
        }
        None
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

use proc_macro::{Group, Ident, Literal, Punct, Span, TokenTree};

pub trait GetSpan: Sized {
    /// Call [`TokenTree::span`]
    #[must_use]
    fn span(&self) -> Span;

    /// `*dst = self.span()`
    #[must_use]
    fn span_as(self, dst: &mut Span) -> Self {
        *dst = self.span();
        self
    }

    /// For [`Group`], it will return ([`span_open`], [`span_close`])
    ///
    /// [`span_open`]: Group::span_open
    /// [`span_close`]: Group::span_close
    #[must_use]
    fn span_region(&self) -> (Span, Span) {
        (self.span(), self.span())
    }
}
impl<T: GetSpan> GetSpan for &T {
    fn span(&self) -> Span {
        (**self).span()
    }

    fn span_region(&self) -> (Span, Span) {
        (**self).span_region()
    }
}
impl<T: GetSpan> GetSpan for &mut T {
    fn span(&self) -> Span {
        (**self).span()
    }

    fn span_region(&self) -> (Span, Span) {
        (**self).span_region()
    }
}
impl GetSpan for Group {
    fn span(&self) -> Span {
        self.span()
    }

    fn span_region(&self) -> (Span, Span) {
        (self.span_open(), self.span_close())
    }
}
impl GetSpan for Span {
    fn span(&self) -> Span {
        *self
    }
}

pub trait SetSpan: GetSpan + Sized {
    /// Call [`TokenTree::set_span`]
    fn set_span(&mut self, span: Span);

    #[must_use]
    fn set_spaned(mut self, span: Span) -> Self {
        self.set_span(span);
        self
    }
}

macro_rules! impl_set_span {
    ($ty:ty) => {
        impl GetSpan for $ty {
            fn span(&self) -> Span {
                self.span()
            }
        }
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
impl_set_span!(Literal);
impl SetSpan for Group {
    fn set_span(&mut self, span: Span) {
        self.set_span(span);
    }
}
impl<T: SetSpan> SetSpan for &mut T {
    fn set_span(&mut self, span: Span) {
        (*self).set_span(span);
    }
}
impl SetSpan for Span {
    fn set_span(&mut self, span: Span) {
        *self = span
    }
}

/// Generate a function, set input `TokenTree` span
pub fn span_setter<T>(span: Span) -> impl Fn(T) -> T
where T: SetSpan,
{
    move |tt| {
        tt.set_spaned(span)
    }
}

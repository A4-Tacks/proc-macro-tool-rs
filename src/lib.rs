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
mod exts;
mod kind;
mod parse_iter;
mod func_utils;

pub use span::*;
pub use exts::*;
pub use kind::*;
pub use parse_iter::*;
pub use func_utils::*;

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
    (@$f:literal $($rest:tt)*) => {
        $crate::err!(&::std::format!($f) $($rest)*)
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
    (@$f:literal $($rest:tt)*) => {
        $crate::rerr!(&::std::format!($f) $($rest)*)
    };
}

#[cfg(test)]
mod tests {
    use super::*;

    #[deny(unused)]
    fn _test_rerr() -> Result<(), proc_macro::TokenStream> {
        let msg = "abc";
        rerr!(@"{msg}");
    }
}

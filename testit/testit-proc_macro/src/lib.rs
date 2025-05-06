use proc_macro::TokenStream;

mod tests;

#[proc_macro]
pub fn test(stream: TokenStream) -> TokenStream {
    tests::__test();
    stream
}

use proc_macro::TokenStream;

#[proc_macro]
pub fn writeln(input: TokenStream) -> TokenStream {
    panic!("{:#?}", input);
}

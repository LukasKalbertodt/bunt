//! These are the docs for the crate `bunt-macros`. This is just implementation
//! detail, please see the crate `bunt` for the real docs.

use proc_macro::TokenStream as TokenStream1;
use proc_macro2::TokenStream;

#[macro_use]
mod err;
mod gen;
mod ir;
mod parse;

#[cfg(test)]
mod tests;

use crate::{
    err::Error,
    ir::{Style, WriteInput},
};


// Docs are in the `bunt` reexport.
#[proc_macro]
pub fn style(input: TokenStream1) -> TokenStream1 {
    run(input, |input| {
        let style = Style::parse_from_tokens(input)?;
        Ok(style.to_tokens())
    })
}

// Docs are in the `bunt` reexport.
#[proc_macro]
pub fn write(input: TokenStream1) -> TokenStream1 {
    run(input, |input| parse::parse(input, WriteInput::parse)?.gen_output())
}

// Docs are in the `bunt` reexport.
#[proc_macro]
pub fn writeln(input: TokenStream1) -> TokenStream1 {
    run(input, |input| {
        let mut input = parse::parse(input, WriteInput::parse)?;
        input.format_str.add_newline();
        input.gen_output()
    })
}

/// Performs the conversion from and to `proc_macro::TokenStream` and converts
/// `Error`s into `compile_error!` tokens.
fn run(
    input: TokenStream1,
    f: impl FnOnce(TokenStream) -> Result<TokenStream, Error>,
) -> TokenStream1 {
    f(input.into())
        .unwrap_or_else(|e| e.to_compile_error())
        .into()
}

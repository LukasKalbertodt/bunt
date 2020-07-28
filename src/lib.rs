use proc_macro::TokenStream as TokenStream1;
use proc_macro2::{Ident, Span, TokenStream};
use quote::quote;
use syn::{Error, LitStr};


fn run(
    input: TokenStream1,
    f: impl FnOnce(TokenStream) -> Result<TokenStream, Error>,
) -> TokenStream1 {
    f(input.into())
        .unwrap_or_else(|e| e.to_compile_error())
        .into()
}

macro_rules! err {
    ($span:expr, $($t:tt)+) => { syn::Error::new($span, format!($($t)+)) };
}


#[proc_macro]
pub fn style(input: TokenStream1) -> TokenStream1 {
    run(input, |input| {
        let literal = syn::parse2::<LitStr>(input)?;
        color_spec(&literal.value(), literal.span())
    })
}

fn color_spec(spec: &str, span: Span) -> Result<TokenStream, Error> {
    let ident = Ident::new("color_spec", Span::mixed_site());
    let styles = spec.split('+')
        .map(|s| {
            match s {
                "bold" => Ok(quote! { #ident .set_bold(true); }),
                other => Err(err!(span, "invalid color spec fragment '{}'", other)),
            }
        })
        .collect::<Result<TokenStream, _>>()?;

    Ok(quote! {
        {
            let mut #ident = termcolor::ColorSpec::new();
            #styles
            #ident
        }
    })
}

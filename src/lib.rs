use proc_macro::TokenStream as TokenStream1;
use proc_macro2::{Ident, Span, TokenStream};
use quote::quote;
use syn::{Error, LitStr};


/// Helper macro to easily create an error with a span.
macro_rules! err {
    ($span:expr, $($t:tt)+) => { syn::Error::new($span, format!($($t)+)) };
}

/// Takes one string literal containing a style specification and returns an
/// expression evaluating to the corresponding `termcolor::ColorSpec` value.
#[proc_macro]
pub fn spec(input: TokenStream1) -> TokenStream1 {
    run(input, |input| {
        let literal = syn::parse2::<LitStr>(input)?;
        color_spec(&literal.value(), literal.span())
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

/// Parses the style specification in `spec` (with `span`) and returns a token
/// stream representing an expression constructing the corresponding `ColorSpec`
/// value.
fn color_spec(spec: &str, span: Span) -> Result<TokenStream, Error> {
    let ident = Ident::new("color_spec", Span::mixed_site());

    let mut styles = TokenStream::new();
    let mut previous_fg_color = None;
    let mut previous_bg_color = None;
    for fragment in spec.split('+').map(str::trim).filter(|s| !s.is_empty()) {
        let (fragment, is_bg) = match fragment.strip_prefix("bg:") {
            Some(color) => (color, true),
            None => (fragment, false),
        };

        // Parse/obtain color if a color is specified.
        let color = match fragment {
            "black" => Some(quote! { Black }),
            "blue" => Some(quote! { Blue }),
            "green" => Some(quote! { Green }),
            "red" => Some(quote! { Red }),
            "cyan" => Some(quote! { Cyan }),
            "magenta" => Some(quote! { Magenta }),
            "yellow" => Some(quote! { Yellow }),
            "white" => Some(quote! { White }),

            hex if hex.starts_with('#') => {
                let hex = &hex[1..];

                if hex.len() != 6 {
                    let e = err!(
                        span,
                        "hex color code invalid: 6 digits expected, found {}",
                        hex.len(),
                    );
                    return Err(e);
                }

                let digits = hex.chars()
                    .map(|c| {
                        c.to_digit(16).ok_or_else(|| {
                            err!(span, "hex color code invalid: {} is not a valid hex digit", c)
                        })
                    })
                    .collect::<Result<Vec<_>, _>>()?;

                let r = (digits[0] * 16 + digits[1]) as u8;
                let g = (digits[2] * 16 + digits[3]) as u8;
                let b = (digits[4] * 16 + digits[5]) as u8;

                Some(quote! { Rgb(#r, #g, #b) })
            },

            // TODO: Ansi256 colors
            _ => None,
        };

        // Check for duplicate color definitions.
        let (previous_color, color_kind) = match is_bg {
            true => (&mut previous_bg_color, "background"),
            false => (&mut previous_fg_color, "foreground"),
        };
        match (&color, *previous_color) {
            (Some(_), Some(old)) => {
                let e = err!(
                    span,
                    "found '{}' but the {} color was already specified as '{}'",
                    fragment,
                    color_kind,
                    old,
                );
                return Err(e);
            }
            (Some(_), None) => *previous_color = Some(fragment),
            _ => {}
        }

        // Obtain the final token stream for method call.
        let tokens = match (is_bg, color, fragment) {
            (false, Some(color), _) => quote! { #ident .set_fg(Some(termcolor::Color:: #color ))},
            (true, Some(color), _) => quote! { #ident .set_bg(Some(termcolor::Color:: #color ))},
            (true, None, other) => {
                return Err(err!(span, "'{}' (following 'bg:') is not a valid color", other));
            }

            (false, None, "bold") => quote! { #ident .set_bold(true); },
            (false, None, "!bold") => quote! { #ident .set_bold(false); },
            (false, None, "italic") => quote! { #ident .set_italic(true); },
            (false, None, "!italic") => quote! { #ident .set_italic(false); },
            (false, None, "underline") => quote! { #ident .set_underline(true); },
            (false, None, "!underline") => quote! { #ident .set_underline(false); },
            (false, None, "intense") => quote! { #ident .set_intense(true); },
            (false, None, "!intense") => quote! { #ident .set_intense(false); },

            (false, None, other) => {
                return Err(err!(span, "invalid style spec fragment '{}'", other));
            }
        };

        styles.extend(quote! { # tokens ; });
    }

    Ok(quote! {
        {
            let mut #ident = termcolor::ColorSpec::new();
            #styles
            #ident
        }
    })
}

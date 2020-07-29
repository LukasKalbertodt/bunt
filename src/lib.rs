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
        let style = Style::parse(&literal.value(), literal.span())?;
        Ok(style.to_tokens())
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


#[derive(Debug, Default)]
struct Style {
    fg: Option<Color>,
    bg: Option<Color>,
    bold: Option<bool>,
    intense: Option<bool>,
    underline: Option<bool>,
    italic: Option<bool>,
    reset: Option<bool>,
}

#[derive(Debug, Clone, Copy)]
enum Color {
    Black,
    Blue,
    Green,
    Red,
    Cyan,
    Magenta,
    Yellow,
    White,
    //Ansi256(u8), // TODO: add
    Rgb(u8, u8, u8),
}

impl Color {
    fn to_tokens(&self) -> TokenStream {
        let variant = match self {
            Self::Black => Some(quote! { Black }),
            Self::Blue => Some(quote! { Blue }),
            Self::Green => Some(quote! { Green }),
            Self::Red => Some(quote! { Red }),
            Self::Cyan => Some(quote! { Cyan }),
            Self::Magenta => Some(quote! { Magenta }),
            Self::Yellow => Some(quote! { Yellow }),
            Self::White => Some(quote! { White }),
            Self::Rgb(r, g, b) => Some(quote! { Rgb(#r, #g, #b) }),
        };

        quote! { termcolor::Color:: #variant }
    }
}

impl Style {
    /// Parses the style specification in `spec` (with `span`) and returns a token
    /// stream representing an expression constructing the corresponding `ColorSpec`
    /// value.
    fn parse(spec: &str, span: Span) -> Result<Self, Error> {
        let mut out = Self::default();

        let mut previous_fg_color = None;
        let mut previous_bg_color = None;
        for fragment in spec.split('+').map(str::trim).filter(|s| !s.is_empty()) {
            let (fragment, is_bg) = match fragment.strip_prefix("bg:") {
                Some(color) => (color, true),
                None => (fragment, false),
            };

            // Parse/obtain color if a color is specified.
            let color = match fragment {
                "black" => Some(Color::Black),
                "blue" => Some(Color::Blue),
                "green" => Some(Color::Green),
                "red" => Some(Color::Red),
                "cyan" => Some(Color::Cyan),
                "magenta" => Some(Color::Magenta),
                "yellow" => Some(Color::Yellow),
                "white" => Some(Color::White),

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

                    Some(Color::Rgb(r, g, b))
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

            macro_rules! set_attr {
                ($field:ident, $value:expr) => {
                    if let Some(b) = out.$field {
                        let field_s = stringify!($field);
                        let old = if b { field_s.into() } else { format!("!{}", field_s) };
                        let new = if $value { field_s.into() } else { format!("!{}", field_s) };
                        let e = err!(
                            span,
                            "invalid style definition: found '{}', but '{}' was specified before",
                            new,
                            old,
                        );
                        return Err(e);
                    }
                };
            }

            // Obtain the final token stream for method call.
            match (is_bg, color, fragment) {
                (false, Some(color), _) => out.fg = Some(color),
                (true, Some(color), _) => out.bg = Some(color),
                (true, None, other) => {
                    return Err(err!(span, "'{}' (following 'bg:') is not a valid color", other));
                }

                (false, None, "bold") => set_attr!(bold, true),
                (false, None, "!bold") => set_attr!(bold, false),
                (false, None, "italic") => set_attr!(italic, true),
                (false, None, "!italic") => set_attr!(italic, false),
                (false, None, "underline") => set_attr!(underline, true),
                (false, None, "!underline") => set_attr!(underline, false),
                (false, None, "intense") => set_attr!(intense, true),
                (false, None, "!intense") => set_attr!(intense, false),

                (false, None, other) => {
                    return Err(err!(span, "invalid style spec fragment '{}'", other));
                }
            }
        }

        Ok(out)
    }

    /// Returns a token stream representing an expression constructing the
    /// `ColorSpec` value corresponding to `self`.
    fn to_tokens(&self) -> TokenStream {
        let ident = Ident::new("color_spec", Span::mixed_site());
        let mut method_calls = TokenStream::new();

        if let Some(fg) = self.fg {
            let fg = fg.to_tokens();
            method_calls.extend(quote! {
                #ident.set_fg(Some(#fg));
            })
        }
        if let Some(bg) = self.bg {
            let bg = bg.to_tokens();
            method_calls.extend(quote! {
                #ident.set_bg(Some(#bg));
            })
        }

        macro_rules! attr {
            ($field:ident, $method:ident) => {
                if let Some(b) = self.$field {
                    method_calls.extend(quote! {
                        #ident.$method(#b);
                    });
                }
            };
        }

        attr!(bold, set_bold);
        attr!(italic, set_italic);
        attr!(underline, set_underline);
        attr!(intense, set_intense);

        quote! {
            {
                let mut #ident = termcolor::ColorSpec::new();
                #method_calls
                #ident
            }
        }
    }
}

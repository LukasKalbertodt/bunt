use proc_macro::TokenStream as TokenStream1;
use proc_macro2::{Ident, Span, TokenStream};
use quote::quote;
use syn::{
    Error,
    LitStr,
    Token,
    parse::{Parse, ParseStream},
    spanned::Spanned,
};
use std::collections::HashMap;


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

#[proc_macro]
pub fn write(input: TokenStream1) -> TokenStream1 {
    run(input, |input| {
        let input: WriteInput = syn::parse2(input)?;
        dbg!(input);

        Ok(quote!(3))
    })
}

/// Input for the `write!` macro.
#[derive(Debug)]
struct WriteInput {
    target: syn::Expr,
    format_str: FormatStr,
    args: FormatArgs,
}

impl Parse for WriteInput {
    fn parse(input: ParseStream) -> Result<Self, Error> {
        let target = input.parse()?;
        input.parse::<Token![,]>()?;
        let format_str = input.parse()?;
        let args = input.parse()?;

        Ok(Self { target, format_str, args })
    }
}

/// One fragment of the format string.
#[derive(Debug)]
enum FormatStrFragment {
    /// A literal string without any arguments or style blocks.
    Str(String),

    /// An `{...}` argument.
    Arg {
        /// The argument that's referenced.
        arg: ArgRef,
        /// Formatting specifications (the thing after `:`).
        format_spec: Option<String>,
        /// An optional styling.
        style: Option<Style>,
    },

    /// A `{$...}` style start tag.
    StyleStart(Style),

    /// A `{$/}` style end tag.
    StyleEnd,
}

/// How a format argument is referred to.
#[derive(Debug)]
enum ArgRef {
    /// `{}`
    Next,
    /// `{2}`
    Position(u32),
    /// `{peter}`
    Name(String),
}

/// A parsed format string.
#[derive(Debug)]
struct FormatStr {
    fragments: Vec<FormatStrFragment>,
}

impl Parse for FormatStr {
    fn parse(input: ParseStream) -> Result<Self, Error> {
        let lit = input.parse::<syn::LitStr>()?;
        let raw = lit.value();
        let mut fragments = Vec::new();

        let mut s = &raw[..];
        while !s.is_empty() {
            if s.starts_with('{') && !s.starts_with("{{") {
                // ===== An argument or style tag =====

                // I *think* there can't be escaped closing braces inside the
                // fmt format.
                let end = s.find('}').ok_or(err!(lit.span(), "unclosed '{{' in format string"))?;
                let mut inner = &s[1..end];

                // Check if it's a style start tag, style end tag or argument.
                let fragment = if inner == "$/" {
                    FormatStrFragment::StyleEnd
                } else if inner.starts_with('$') {
                    let style = Style::parse(&inner[1..], lit.span())?;
                    FormatStrFragment::StyleStart(style)
                } else {
                    // Parse optional style information.
                    let style = if inner.starts_with('[') {
                        let style_end = inner.find(']')
                            .ok_or(err!(lit.span(), "unclosed '[' in format string argument"))?;
                        let style = Style::parse(&inner[1..style_end], lit.span())?;
                        inner = &inner[style_end + 1..];
                        Some(style)
                    } else {
                        None
                    };

                    // Split argument reference and format specs.
                    let arg_ref_end = inner.find(':').unwrap_or(inner.len());
                    let (arg_str, format) = inner.split_at(arg_ref_end);

                    // Check kind of argument reference.
                    let arg = if arg_str.is_empty() {
                        ArgRef::Next
                    } else if let Ok(pos) = arg_str.parse::<u32>() {
                        ArgRef::Position(pos)
                    } else {
                        syn::parse_str::<syn::Ident>(arg_str)?;
                        ArgRef::Name(arg_str.into())
                    };

                    FormatStrFragment::Arg {
                        arg,
                        format_spec: if format.is_empty() { None } else { Some(format.into()) },
                        style,
                    }
                };

                fragments.push(fragment);
                s = &s[end + 1..];
            } else {
                // ===== A literal string =====

                // Find the start of the next unescaped `{` or EOF.
                let end = s.match_indices('{')
                    .map(|(pos, _)| pos)
                    .find(|pos| !s[pos + 1..].starts_with('{'))
                    .unwrap_or(s.len());

                let (fragment, rest) = s.split_at(end);

                // Check for unmatched closing braces.
                let unmatched_closing = fragment.match_indices('}')
                    .map(|(pos, _)| pos)
                    .find(|pos| !fragment[pos + 1..].starts_with('}'));
                if let Some(pos) = unmatched_closing {
                    let e = err!(
                        lit.span(),
                        "unmatched '}}' in format string at position {}",
                        s.as_ptr() as usize - raw.as_ptr() as usize + pos,
                    );
                    return Err(e);
                }

                fragments.push(FormatStrFragment::Str(fragment.into()));
                s = rest;
            };
        }

        Ok(Self { fragments })
    }
}

/// Parsed formatting arguments.
#[derive(Debug)]
struct FormatArgs {
    positional: Vec<syn::Expr>,
    named: HashMap<String, syn::Expr>,
}

impl Parse for FormatArgs {
    fn parse(input: ParseStream) -> Result<Self, Error> {
        let mut positional = Vec::new();
        let mut named = HashMap::new();
        let mut saw_named = false;

        // We expect a comma here as this is always following the format string.
        if !input.peek(Token![,]) {
            return Ok(Self { positional, named })
        }
        input.parse::<Token![,]>()?;

        loop {
            if input.is_empty() {
                break;
            }

            // Parse the argument.
            match input.parse()? {
                FormatArg::Positional(e) => {
                    if saw_named {
                        let e = err!(
                            e.span(),
                            "positional argument after named arguments is not allowed",
                        );
                        return Err(e);
                    }

                    positional.push(e);
                },
                FormatArg::Named(name, e) => {
                    saw_named = true;
                    named.insert(name, e);
                }
            }

            // Consume comma or stop.
            if !input.peek(Token![,]) {
                break;
            }
            input.parse::<Token![,]>()?;
        }

        Ok(Self { positional, named })
    }
}

/// A single format argument.
#[derive(Debug)]
enum FormatArg {
    /// This argument is not named, e.g. just `27`.
    Positional(syn::Expr),
    /// A named argument, e.g. `value = 27`.
    Named(String, syn::Expr),
}

impl Parse for FormatArg {
    fn parse(input: ParseStream) -> Result<Self, Error> {
        match input.parse()? {
            syn::Expr::Assign(syn::ExprAssign { attrs, left, right, .. }) => {
                if let Some(attr) = attrs.get(0) {
                    return Err(err!(attr.span(), "attributes invalid in this context"));
                }

                // We only accept a single identifier on the left.
                match *left {
                    syn::Expr::Path(path) => {
                        let ident = path.path.get_ident();
                        if !path.attrs.is_empty() || path.qself.is_some() || ident.is_none() {
                            let e = err!(
                                path.span(),
                                "expected single identifier, found path on the left \
                                    side of the '=' in named parameter",
                            );
                            return Err(e);
                        }

                        Ok(Self::Named(ident.unwrap().to_string(), *right))
                    }
                    other => {
                        let e = err!(
                            other.span(),
                            "expected single identifier, found some expression on the left \
                                side of the '=' in named parameter",
                        );
                        return Err(e);
                    }
                }
            }

            // TODO: maybe disallow some expression types

            expr => Ok(Self::Positional(expr)),
        }
    }
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

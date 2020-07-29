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
use std::{
    collections::{BTreeSet, HashMap},
    fmt::Write,
};


/// Helper macro to easily create an error with a span.
macro_rules! err {
    ($fmt:literal $($t:tt)*) => { syn::Error::new(Span::call_site(), format!($fmt $($t)*)) };
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

        // Helper functions to create idents for argument bindings
        fn pos_arg_ident(id: u32) -> Ident {
            Ident::new(&format!("arg_pos_{}", id), Span::mixed_site())
        }
        fn name_arg_ident(id: &str) -> Ident {
            Ident::new(&format!("arg_name_{}", id), Span::mixed_site())
        }

        // Create a binding for each given argument. This is useful for two
        // reasons:
        // - The given expression could have side effects or be compuationally
        //   expensive. The formatting macros from std guarantee that the
        //   expression is evaluated only once, so we want to guarantee the
        //   same.
        // - We can then very easily refer to all arguments later. Without these
        //   bindings, we have to do lots of tricky logic to get the right
        //   arguments in each invidiual `write` call.
        let mut arg_bindings = TokenStream::new();
        for (i, arg) in input.args.positional.iter().enumerate() {
            let ident = pos_arg_ident(i as u32);
            arg_bindings.extend(quote! {
                let #ident = &{ #arg };
            })
        }
        for (name, arg) in input.args.named.iter() {
            let ident = name_arg_ident(name);
            arg_bindings.extend(quote! {
                let #ident = &{ #arg };
            })
        }

        // Prepare the actual process of writing to the target according to the
        // format string.
        let buf = Ident::new("buf", Span::mixed_site());
        let mut style_stack = Vec::new();
        let mut writes = TokenStream::new();
        let mut next_arg_index = 0;

        for segment in input.format_str.fragments {
            match segment {
                // A formatting fragment. This is the more tricky one. We have
                // to construct a `std::write!` invocation that has the right
                // fmt string, the right arguments (and no additional ones!) and
                // the correct argument references.
                FormatStrFragment::Fmt { fmt_str_parts, args } => {
                    let mut fmt_str = fmt_str_parts[0].clone();
                    let mut used_args = BTreeSet::new();

                    for (i, arg) in args.into_iter().enumerate() {
                        let ident = match arg.kind {
                            ArgRefKind::Next => {
                                let ident = pos_arg_ident(next_arg_index as u32);
                                if input.args.positional.get(next_arg_index).is_none() {
                                    return Err(
                                        err!("invalid '{{}}' argument reference \
                                            (too few actual arguments)")
                                    );
                                }

                                next_arg_index += 1;
                                ident
                            }
                            ArgRefKind::Position(pos) => {
                                let ident = pos_arg_ident(pos);
                                if input.args.positional.get(pos as usize).is_none() {
                                    return Err(err!(
                                        "invalid reference to positional argument {} (there are \
                                            not that many arguments)",
                                        pos,
                                    ));
                                }

                                ident
                            }
                            ArgRefKind::Name(name) => {
                                let ident = name_arg_ident(&name);
                                if input.args.named.get(&name).is_none() {
                                    return Err(err!("there is no argument named `{}`", name));
                                }

                                ident
                            }
                        };

                        std::write!(fmt_str, "{{{}:{}}}", ident, arg.format_spec).unwrap();
                        used_args.insert(ident);
                        fmt_str.push_str(&fmt_str_parts[i + 1]);
                    }


                    // Combine everything in `write!` invocation.
                    writes.extend(quote! {
                        std::write!(#buf, #fmt_str #(, #used_args = #used_args)* )?;
                    });
                }

                // A style start tag: we simply create the `ColorSpec` and call
                // `set_color`. The interesting part is how the styles stack and
                // merge.
                FormatStrFragment::StyleStart(style) => {
                    let last_style = style_stack.last().copied().unwrap_or(Style::default());
                    let new_style = style.or(last_style);
                    let style_def = new_style.to_tokens();
                    style_stack.push(new_style);
                    writes.extend(quote! {
                        termcolor::WriteColor::set_color(#buf, &#style_def)?;
                    });
                }

                // Revert the last style tag. This means that we pop the topmost
                // style from the stack and apply the *then* topmost style
                // again.
                FormatStrFragment::StyleEnd => {
                    style_stack.pop().ok_or(err!("unmatched closing style tag"))?;
                    let style = style_stack.last().copied().unwrap_or(Style::default());
                    let style_def = style.to_tokens();
                    writes.extend(quote! {
                        termcolor::WriteColor::set_color(#buf, &#style_def)?;
                    });
                }
            }
        }

        // Check if the style tags are balanced
        if !style_stack.is_empty() {
            return Err(err!("unclosed style tag"));
        }

        // Combine everything.
        let target = &input.target;
        Ok(quote! {
            (|| -> Result<(), ::std::io::Error> {
                use std::io::Write as _;

                #arg_bindings
                let #buf = &mut #target;
                #writes

                Ok(())
            })()
        })
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
    /// A format string without style tags, but potentially with arguments.
    ///
    /// `fmt_str_parts` always has exactly one element more than `args`.
    Fmt {
        /// The format string as parts between the arguments.
        fmt_str_parts: Vec<String>,

        /// Information about argument that are referenced.
        args: Vec<ArgRef>,
    },

    /// A `{$...}` style start tag.
    StyleStart(Style),

    /// A `{/$}` style end tag.
    StyleEnd,
}

#[derive(Debug)]
struct ArgRef {
    kind: ArgRefKind,
    format_spec: String,
}

/// How a format argument is referred to.
#[derive(Debug)]
enum ArgRefKind {
    /// `{}`
    Next,
    /// `{2}`
    Position(u32),
    /// `{peter}`
    Name(String),
}

impl ArgRef {
    /// (Partially) parses the inside of an format arg (`{...}`). The given
    /// string `s` must be the inside of the arg and must *not* contain the
    /// outer braces.
    fn parse(s: &str) -> Result<Self, Error> {
        // Split argument reference and format specs.
        let arg_ref_end = s.find(':').unwrap_or(s.len());
        let (arg_str, format_spec) = s.split_at(arg_ref_end);

        // Check kind of argument reference.
        let kind = if arg_str.is_empty() {
            ArgRefKind::Next
        } else if let Ok(pos) = arg_str.parse::<u32>() {
            ArgRefKind::Position(pos)
        } else {
            syn::parse_str::<syn::Ident>(arg_str)?;
            ArgRefKind::Name(arg_str.into())
        };

        Ok(Self { kind, format_spec: format_spec.into() })
    }
}

/// A parsed format string.
#[derive(Debug)]
struct FormatStr {
    fragments: Vec<FormatStrFragment>,
}

impl Parse for FormatStr {
    fn parse(input: ParseStream) -> Result<Self, Error> {
        /// Searches for the next closing `}`. Returns a pair of strings, the
        /// first starting like `s` and ending at the closing brace, the second
        /// starting at the brace and ending like `s`. Both strings exclude the
        /// brace itself. If a closing brace can't be found, an error is
        /// returned.
        fn split_at_closing_brace(s: &str, span: Span) -> Result<(&str, &str), Error> {
            // I *think* there can't be escaped closing braces inside the fmt
            // format, so we can simply search for a single closing one.
            let end = s.find("}")
                .ok_or(err!(span, "unclosed '{{' in format string"))?;
            Ok((&s[..end], &s[end + 1..]))
        }


        let lit = input.parse::<syn::LitStr>()?;
        let raw = lit.value();

        // Scan the whole string
        let mut fragments = Vec::new();
        let mut s = &raw[..];
        while !s.is_empty() {
            fn string_without<'a>(a: &'a str, b: &'a str) -> &'a str {
                let end = b.as_ptr() as usize - a.as_ptr() as usize;
                &a[..end]
            }

            // let start_string = s;
            let mut args = Vec::new();
            let mut fmt_str_parts = Vec::new();

            // Scan until we reach a style tag.
            let mut scanner = s;
            loop {
                match () {
                    // Reached EOF: stop searching!
                    () if scanner.is_empty() => break,

                    // Escaped brace: skip.
                    () if scanner.starts_with("{{") => scanner = &scanner[2..],

                    // Found a style tag: stop searching!
                    () if scanner.starts_with("{$") => break,
                    () if scanner.starts_with("{/$") => break,

                    // Found a styled argument: stop searching!
                    () if scanner.starts_with("{[") => break,

                    // An formatting argument. Gather some information about it
                    // and remember it for later.
                    () if scanner.starts_with("{") => {
                        let (inner, rest) = split_at_closing_brace(&scanner[1..], lit.span())?;
                        args.push(ArgRef::parse(inner)?);
                        fmt_str_parts.push(string_without(s, scanner).to_owned());
                        s = rest;
                        scanner = rest;
                    }

                    // Some other character: just continue searching.
                    _ => scanner = &scanner[1..],
                }
            }

            // Add the last string part and then push this fragment, unless it
            // is completely empty.
            fmt_str_parts.push(string_without(s, scanner).to_owned());
            s = scanner;
            if !args.is_empty() || fmt_str_parts.iter().any(|s| !s.is_empty()) {
                fragments.push(FormatStrFragment::Fmt { args, fmt_str_parts });
            }

            if s.is_empty() {
                break;
            }

            // At this point, `s` starts with either a styled argument or a
            // style tag.
            match () {
                // Closing style tag.
                () if s.starts_with("{/$}") => {
                    fragments.push(FormatStrFragment::StyleEnd);
                    s = &s[4..];
                }

                // Opening style tag.
                () if s.starts_with("{$") => {
                    let (inner, rest) = split_at_closing_brace(&s[2..], lit.span())?;
                    let style = Style::parse(inner, lit.span())?;
                    fragments.push(FormatStrFragment::StyleStart(style));
                    s = rest;
                }

                () if s.starts_with("{[") => {
                    let (inner, rest) = split_at_closing_brace(&s[1..], lit.span())?;

                    // Parse style information
                    let style_end = inner.find(']')
                        .ok_or(err!(lit.span(), "unclosed '[' in format string argument"))?;
                    let style = Style::parse(&inner[1..style_end], lit.span())?;
                    fragments.push(FormatStrFragment::StyleStart(style));

                    // Parse the standard part of this arg reference.
                    let standard_inner = inner[style_end + 1..].trim_start();
                    let arg = ArgRef::parse(standard_inner)?;
                    fragments.push(FormatStrFragment::Fmt {
                        args: vec![arg],
                        fmt_str_parts: vec!["".into(), "".into()],
                    });

                    fragments.push(FormatStrFragment::StyleEnd);

                    s = rest;
                }

                _ => panic!("bug: at this point, there should be a style tag or styled arg"),
            }
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

#[derive(Debug, Default, Clone, Copy)]
struct Style {
    fg: Option<Color>,
    bg: Option<Color>,
    bold: Option<bool>,
    intense: Option<bool>,
    underline: Option<bool>,
    italic: Option<bool>,
    reset: Option<bool>,
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
                ($field:ident, $value:expr) => {{
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

                    out.$field = Some($value);
                }};
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

    /// Like `Option::or`: all style values set in `self` are kept, all unset
    /// ones are overwritten with the values from `style_b`.
    fn or(&self, style_b: Self) -> Self {
        Self {
            fg: self.fg.or(style_b.fg),
            bg: self.bg.or(style_b.bg),
            bold: self.bold.or(style_b.bold),
            intense: self.intense.or(style_b.intense),
            underline: self.underline.or(style_b.underline),
            italic: self.italic.or(style_b.italic),
            reset: self.reset.or(style_b.reset),
        }
    }
}

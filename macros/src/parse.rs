//! Parse the macro input into an intermediate representation.

use proc_macro2::{
    Span, TokenStream, Delimiter, TokenTree, Spacing,
    token_stream::IntoIter as TokenIterator,
};
use std::collections::HashMap;
use crate::{
    ArgRefKind, ArgRef, Error, Expr, WriteInput, FormatStr, Style, Color,
    FormatStrFragment, FormatArgs,
};


/// Helper function to parse from a token stream. Makes sure the iterator is
/// empty after `f` returns.
pub(crate) fn parse<F, O>(tokens: TokenStream, f: F) -> Result<O, Error>
where
    F: FnOnce(&mut TokenIterator) -> Result<O, Error>,
{
    let mut it = tokens.into_iter();
    let out = f(&mut it)?;

    if let Some(tt) = it.next() {
        return Err(err!(tt.span(), "unexpected additional tokens"));
    }

    Ok(out)
}

/// Tries to parse a string literal.
fn parse_str_literal(it: &mut TokenIterator) -> Result<(String, Span), Error> {
    match it.next() {
        Some(TokenTree::Literal(lit)) => Ok((
            crate::literal::parse_str_literal(&lit)?,
            lit.span(),
        )),
        Some(tt) => {
            Err(err!(tt.span(), "expected string literal, found different token tree"))
        }
        None => Err(err!("expected string literal, found EOF")),
    }
}

/// Tries to parse a helper group (a group with `None` or `()` delimiter).
///
/// These groups are inserted by the declarative macro wrappers in `bunt` to
/// make parsing in `bunt-macros` easier. In particular, all expressions are
/// wrapped in these group, allowing us to skip over them without having a Rust
/// expression parser.
fn expect_helper_group(tt: Option<TokenTree>) -> Result<(TokenStream, Span), Error> {
    match tt {
        Some(TokenTree::Group(g))
            if g.delimiter() == Delimiter::None || g.delimiter() == Delimiter::Parenthesis =>
        {
            Ok((g.stream(), g.span()))
        }
        Some(TokenTree::Group(g)) => {
            Err(err!(
                g.span(),
                "expected none or () delimited group, but delimiter is {:?} (note: do not use \
                    the macros from `bunt-macros` directly, but only through `bunt`)",
                g.delimiter(),
            ))
        }
        Some(tt) => {
            Err(err!(
                tt.span(),
                "expected none or () delimited group, but found different token tree (note: do \
                    not use the macros from `bunt-macros` directly, but only through `bunt`)",
            ))
        }
        None => Err(err!("expected none or () delimited group, found EOF")),
    }
}

impl WriteInput {
    pub(crate) fn parse(it: &mut TokenIterator) -> Result<Self, Error> {
        let target = Expr::parse(it)?;
        let format_str = FormatStr::parse(it)?;
        let args = FormatArgs::parse(it)?;

        Ok(Self { target, format_str, args })
    }
}

impl Expr {
    pub(crate) fn parse(it: &mut TokenIterator) -> Result<Self, Error> {
        let (tokens, span) = expect_helper_group(it.next())?;
        Ok(Self { tokens, span })
    }
}

impl FormatStr {
    pub(crate) fn parse(it: &mut TokenIterator) -> Result<Self, Error> {
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

        let (inner, _) = expect_helper_group(it.next())?;
        let (raw, span) = parse(inner, parse_str_literal)?;

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
                match scanner.find('{') {
                    Some(brace_pos) => scanner = &scanner[brace_pos..],
                    None => {
                        // EOF reached: stop searching
                        scanner = &scanner[scanner.len()..];
                        break;
                    }
                }


                match () {
                    // Escaped brace: skip.
                    () if scanner.starts_with("{{") => scanner = &scanner[2..],

                    // Found a style tag: stop searching!
                    () if scanner.starts_with("{$") => break,
                    () if scanner.starts_with("{/$") => break,

                    // Found a styled argument: stop searching!
                    () if scanner.starts_with("{[") => break,

                    // An formatting argument. Gather some information about it
                    // and remember it for later.
                    _ => {
                        let (inner, rest) = split_at_closing_brace(&scanner[1..], span)?;
                        args.push(ArgRef::parse(inner)?);
                        fmt_str_parts.push(string_without(s, scanner).to_owned());
                        s = rest;
                        scanner = rest;
                    }
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
                    let (inner, rest) = split_at_closing_brace(&s[2..], span)?;
                    let style = Style::parse(inner, span)?;
                    fragments.push(FormatStrFragment::StyleStart(style));
                    s = rest;
                }

                () if s.starts_with("{[") => {
                    let (inner, rest) = split_at_closing_brace(&s[1..], span)?;

                    // Parse style information
                    let style_end = inner.find(']')
                        .ok_or(err!(span, "unclosed '[' in format string argument"))?;
                    let style = Style::parse(&inner[1..style_end], span)?;
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

impl ArgRef {
    /// (Partially) parses the inside of an format arg (`{...}`). The given
    /// string `s` must be the inside of the arg and must *not* contain the
    /// outer braces.
    pub(crate) fn parse(s: &str) -> Result<Self, Error> {
        // Split argument reference and format specs.
        let arg_ref_end = s.find(':').unwrap_or(s.len());
        let (arg_str, format_spec) = s.split_at(arg_ref_end);

        // Check kind of argument reference.
        let kind = if arg_str.is_empty() {
            ArgRefKind::Next
        } else if let Ok(pos) = arg_str.parse::<u32>() {
            ArgRefKind::Position(pos)
        } else {
            // TODO: make sure the string is a valid Rust identifier
            ArgRefKind::Name(arg_str.into())
        };

        Ok(Self { kind, format_spec: format_spec.into() })
    }
}

impl Style {
    /// Parses the style specifiction assuming the token stream contains a
    /// single string literal.
    pub(crate) fn parse_from_tokens(tokens: TokenStream) -> Result<Self, Error> {
        let (s, span) = parse(tokens, parse_str_literal)?;
        Self::parse(&s, span)
    }

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
}

impl FormatArgs {
    fn parse(it: &mut TokenIterator) -> Result<Self, Error> {
        /// Checks if the token stream starting with `tt0` and `tt1` is a named
        /// argument. If so, returns the name of the argument, otherwise
        /// (positional argument) returns `None`.
        fn get_name(tt0: &Option<TokenTree>, tt1: &Option<TokenTree>) -> Option<String> {
            if let (Some(TokenTree::Ident(name)), Some(TokenTree::Punct(punct))) = (tt0, tt1) {
                if punct.as_char() == '=' && punct.spacing() == Spacing::Alone {
                    return Some(name.to_string())
                }
            }

            None
        }

        let mut positional = Vec::new();
        let mut named = HashMap::new();
        let mut saw_named = false;

        // The remaining tokens should all be `None` delimited groups each
        // representing one argument.
        for arg_group in it {
            let (arg, span) = expect_helper_group(Some(arg_group))?;
            let mut it = arg.into_iter();
            let tt0 = it.next();
            let tt1 = it.next();

            if let Some(name) = get_name(&tt0, &tt1) {
                saw_named = true;

                let expr = Expr {
                    tokens: it.collect(),
                    span,
                };
                named.insert(name, expr);
            } else {
                if saw_named {
                    let e = err!(span, "positional argument after named arguments is not allowed");
                    return Err(e);
                }

                let expr = Expr {
                    tokens: vec![tt0, tt1].into_iter().filter_map(|tt| tt).chain(it).collect(),
                    span,
                };
                positional.push(expr);
            }

        }

        Ok(Self { positional, named })
    }
}

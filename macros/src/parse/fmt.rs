use proc_macro2::{
    Span,
    token_stream::IntoIter as TokenIterator, TokenTree, Delimiter,
};
use unicode_xid::UnicodeXID;
use std::str::Chars;
use crate::{
    err::Error,
    ir::{
        ArgRefKind, ArgRef, FormatStr, Style,
        FormatStrFragment, FormatSpec, Align, Sign, Width, Precision,
    },
};
use super::{parse, expect_helper_group, lit::expect_str_literal};


impl FormatStr {
    /// Parses `["foo"]`, `["foo" "bar"]`.
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

        // We expect a []-delimited group
        let (inner, span) = match it.next() {
            Some(TokenTree::Group(g)) if g.delimiter() == Delimiter::Bracket => {
                (g.stream(), g.span())
            }
            Some(TokenTree::Group(g)) => {
                return Err(err!(
                    g.span(),
                    "expected `[]` delimited group, but delimiter is {:?} (note: do not use \
                        the macros from `bunt-macros` directly, but only through `bunt`)",
                    g.delimiter(),
                ));
            }
            Some(tt) => {
                return Err(err!(
                    tt.span(),
                    "expected `[]` delimited group, but found different token tree (note: do \
                        not use the macros from `bunt-macros` directly, but only through `bunt`)",
                ))
            }
            None => return Err(err!("expected `[]` delimited group, found EOF")),
        };

        if inner.is_empty() {
            return Err(err!(
                span,
                "at least one format string has to be provided, but `[]` was passed (note: do not \
                    use the macros from `bunt-macros` directly, but only through `bunt`)"
            ));
        }

        // Concat all string literals
        let mut raw = String::new();
        for tt in inner {
            let (literal, _) = expect_helper_group(Some(tt))?;
            let (string_data, _) = parse(literal, expect_str_literal)?;
            raw += &string_data;
        }

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
        let (arg_str, format_spec) = match s.find(':') {
            None => (s, ""),
            Some(colon_pos) => (&s[..colon_pos], &s[colon_pos + 1..]),
        };

        // Check kind of argument reference.
        let kind = if arg_str.is_empty() {
            ArgRefKind::Next
        } else if let Ok(pos) = arg_str.parse::<usize>() {
            ArgRefKind::Position(pos)
        } else {
            // TODO: make sure the string is a valid Rust identifier
            ArgRefKind::Name(arg_str.into())
        };

        let format_spec = FormatSpec::parse(format_spec)?;

        Ok(Self { kind, format_spec })
    }
}

impl FormatSpec {
    /// Parses the format specification that comes after the `:` inside an
    /// `{...}` argument. The given string must not include the `:` but might be
    /// empty.
    pub(crate) fn parse(s: &str) -> Result<Self, Error> {
        /// Helper iterator for scanning the input
        struct Peek2<'a> {
            peek: Option<char>,
            peek2: Option<char>,
            rest: Chars<'a>,
        }

        impl<'a> Peek2<'a> {
            fn new(s: &'a str) -> Self {
                let mut rest = s.chars();
                let peek = rest.next();
                let peek2 = rest.next();

                Self { peek, peek2, rest }
            }

            fn next_if<O>(&mut self, f: impl FnOnce(char) -> Option<O>) -> Option<O> {
                let out = self.peek.and_then(f);
                if out.is_some() {
                    self.next();
                }
                out
            }

            fn next_if_eq(&mut self, expected: char) -> bool {
                if self.peek == Some(expected) {
                    self.next();
                    true
                } else {
                    false
                }
            }

            /// Parses one decimal number and returns whether or not it was
            /// terminated by `$`.
            fn parse_num(&mut self) -> Result<(usize, bool), Error> {
                let mut num: usize = 0;
                while matches!(self.peek, Some('0'..='9')) {
                    num = num.checked_mul(10)
                        .and_then(|num| {
                            num.checked_add((self.next().unwrap() as u32 - '0' as u32) as usize)
                        })
                        .ok_or(err!("width parameter value overflowed `usize`"))?;
                }

                let arg = self.next_if_eq('$');
                Ok((num, arg))
            }

            /// Parses a Rust identifier terminated by '$'. When calling this
            /// method, `self.peek.map_or(false, |c| c.is_xid_start())` must be
            /// true.
            fn parse_ident(&mut self) -> Result<String, Error> {
                let mut name = String::from(self.next().unwrap());
                while self.peek.map_or(false, |c| c.is_xid_continue()) {
                    name.push(self.next().unwrap());
                }

                if !self.next_if_eq('$') {
                    return Err(err!(
                        "invalid format string specification: width/precision named parameter \
                            does not end with '$'. Note: try the `std` macros to get much better \
                            error reporting."
                    ));
                }

                Ok(name)
            }
        }

        impl Iterator for Peek2<'_> {
            type Item = char;
            fn next(&mut self) -> Option<Self::Item> {
                let out = self.peek.take();
                if out.is_none() {
                    return None;
                }

                self.peek = self.peek2;
                self.peek2 = self.rest.next();

                out
            }
        }


        let mut it = Peek2::new(s);

        // Fill and align. The former can only exist if the latter also exists.
        let (fill, align) = if let Some(align) = it.next_if(Align::from_char) {
            (None, Some(align))
        } else if let Some(align) = it.peek2.and_then(Align::from_char) {
            let fill = it.next().unwrap();
            it.next().unwrap();
            (Some(fill), Some(align))
        } else {
            (None, None)
        };

        // Simple flags.
        let sign = it.next_if(Sign::from_char);
        let alternate = it.next_if_eq('#');
        let zero = if it.peek == Some('0') && it.peek2 != Some('$') {
            it.next().unwrap();
            true
        } else {
            false
        };

        // Width or early exit.
        let width = match it.peek {
            // Either a width constant (`8`) or referring to a positional
            // parameter (`2$`).
            Some('0'..='9') => {
                let (num, dollar) = it.parse_num()?;

                if dollar {
                    Some(Width::Position(num))
                } else {
                    Some(Width::Constant(num))
                }
            }

            // The "type" (e.g. `?`, `X`) determining the formatting trait. This
            // means we are done here.
            Some(c) if it.peek2.is_none() => {
                return Ok(Self {
                    fill,
                    align,
                    sign,
                    alternate,
                    zero,
                    width: None,
                    precision: None,
                    ty: Some(c),
                });
            }

            // The start of a `width` named parameter (`foo$`).
            Some(c) if c.is_xid_start() => Some(Width::Name(it.parse_ident()?)),
            _ => None,
        };

        // Precision starting with '.'.
        let precision = if it.next_if_eq('.') {
            if it.next_if_eq('*') {
                Some(Precision::Bundled)
            } else if matches!(it.peek, Some('0'..='9')) {
                let (num, dollar) = it.parse_num()?;

                if dollar {
                    Some(Precision::Position(num))
                } else {
                    Some(Precision::Constant(num))
                }
            } else {
                Some(Precision::Name(it.parse_ident()?))
            }
        } else {
            None
        };

        // Parse type char and make sure nothing else is left.
        let ty = it.next();
        if let Some(c) = it.next() {
            return Err(err!(
                "expected end of format specification, but found '{}'. Note: use the std \
                    macros to get much better error reporting.",
                c,
            ));
        }

        Ok(Self {
            fill,
            align,
            sign,
            alternate,
            zero,
            width,
            precision,
            ty,
        })
    }
}

impl Align {
    fn from_char(c: char) -> Option<Self> {
        match c {
            '<' => Some(Self::Left),
            '^' => Some(Self::Center),
            '>' => Some(Self::Right),
            _ => None,
        }
    }
}

impl Sign {
    fn from_char(c: char) -> Option<Self> {
        match c {
            '+' => Some(Self::Plus),
            '-' => Some(Self::Minus),
            _ => None,
        }
    }
}

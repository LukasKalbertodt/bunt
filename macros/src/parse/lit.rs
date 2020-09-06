use proc_macro2::{
    Span, TokenTree,
    token_stream::IntoIter as TokenIterator, Literal,
};
use crate::err::Error;


/// Tries to parse a string literal.
pub(super) fn expect_str_literal(it: &mut TokenIterator) -> Result<(String, Span), Error> {
    match it.next() {
        Some(TokenTree::Literal(lit)) => Ok((
            parse_str_literal(&lit)?,
            lit.span(),
        )),
        Some(tt) => {
            Err(err!(tt.span(), "expected string literal, found different token tree"))
        }
        None => Err(err!("expected string literal, found EOF")),
    }
}

/// Parses a string literal into the actual string data. Returns an error if the
/// literal is not a string or raw string literal.
fn parse_str_literal(lit: &Literal) -> Result<String, Error> {
    // In the parsing below, we make use of the fact that the string comes from
    // a `Literal`, i.e. we already know it represents a valid literal. There
    // are only some debug asserts in there to make sure our assumptions are not
    // wrong.
    let s = lit.to_string();
    if s.starts_with('r') {
        // A raw string literal. We don't have to unescape anything, but we just
        // have to strip the `r###` from the start and the `###` from the end.
        let bytes = s.as_bytes();
        let hash_count = bytes[1..].iter().take_while(|b| **b == b'#').count();

        debug_assert!(hash_count < s.len() / 2, "bug: raw string literal has too many #");
        debug_assert!(bytes[hash_count + 1] == b'"');
        debug_assert!(bytes[s.len() - hash_count - 1] == b'"');

        Ok(s[hash_count + 2..s.len() - hash_count - 1].into())
    } else if s.starts_with('"') {
        // A normal string literal. We have to unescape all escape sequences and
        // strip the `"`. A non-raw literal *always* starts and ends with `"`.
        debug_assert!(s.starts_with('"'));
        debug_assert!(s.ends_with('"'));
        let s = &s[1..s.len() - 1];

        let mut out = String::with_capacity(s.len());

        let mut i = 0;
        while let Some(pos) = s[i..].find('\\') {
            // Push the previous (that doesn't contain escapes) to the result.
            out.push_str(&s[i..][..pos]);

            // `pos` can't point to the last character, as that would escape the
            // last `"`.
            let escape = &s[i + pos..];
            let (c, len) = match escape.as_bytes()[1] {
                b'n' => (Some('\n'), 2),
                b'r' => (Some('\r'), 2),
                b't' => (Some('\t'), 2),
                b'0' => (Some('\0'), 2),
                b'\\' => (Some('\\'), 2),
                b'x' => {
                    let v = u8::from_str_radix(&escape[2..4], 16)
                        .expect("bug: invalid \\x escape");
                    (Some(v.into()), 4)
                }
                b'u' => {
                    let end = escape.find('}').expect("invalid \\u escape");

                    let v = u32::from_str_radix(&escape[3..end], 16)
                        .expect("invalid \\u escape");
                    let c = std::char::from_u32(v).expect("invalid value in \\u escape");
                    (Some(c), end + 1)
                }
                b'\n' => {
                    let whitespace_len = escape[2..]
                        .char_indices()
                        .find(|(_, c)| !c.is_whitespace())
                        .map(|(i, _)| i)
                        .unwrap_or(escape.len() - 2);
                    (None, whitespace_len + 2)
                }
                _ => panic!("bug: unknown escape code :/"),
            };

            if let Some(c) = c {
                out.push(c);
            }
            i += pos + len;
        }

        // Push the remaining string
        out.push_str(&s[i..]);

        Ok(out)
    } else {
        Err(Error {
            msg: format!("literal is not a string literal"),
            span: lit.span(),
        })
    }
}

#[cfg(test)]
mod tests {
    use proc_macro2::{TokenStream, TokenTree};
    use super::*;
    use super::parse_str_literal as parse;

    // Helper function to parse a string as literal. Note that we do it this way
    // to get other types of literal. `Literal::string` does not give us full
    // control and neither does using `quote`.
    fn lit(s: &str) -> Literal {
        let stream: TokenStream = s.parse()
            .expect("bug in test: string cannot be parse as tokenstream");
        let mut it = stream.into_iter();
        let tt = it.next().expect("bug in test: string is empty token stream");
        assert!(it.next().is_none(), "bug in test: string results in multiple token trees");

        match tt {
            TokenTree::Literal(lit) => lit,
            _ => panic!("bug in test: string didn't parse as literal"),
        }
    }

    #[test]
    fn empty() {
        assert_eq!(parse(&Literal::string("")).unwrap(), "");
        assert_eq!(parse(&lit(r###""""###)).unwrap(), "");
        assert_eq!(parse(&lit(r###"r"""###)).unwrap(), "");
    }

    #[test]
    fn raw() {
        // Yes, I am aware these strings are hard to read...
        assert_eq!(parse(&lit(r###"r"foo""###)).unwrap(), "foo");
        assert_eq!(parse(&lit(r###"r"foo\nbar""###)).unwrap(), "foo\\nbar");
        assert_eq!(parse(&lit(r###"r#"foo"#"###)).unwrap(), "foo");
        assert_eq!(parse(&lit(r###"r#"foo\nbar"#"###)).unwrap(), "foo\\nbar");
        assert_eq!(parse(&lit(r###"r##"foo"##"###)).unwrap(), "foo");
        assert_eq!(parse(&lit(r###"r##"foo\nbar"##"###)).unwrap(), "foo\\nbar");
    }

    #[test]
    fn normal() {
        assert_eq!(parse(&lit(r###""foo""###)).unwrap(), "foo");
        assert_eq!(parse(&lit(r###""foo\nbar""###)).unwrap(), "foo\nbar");
        assert_eq!(parse(&lit(r###""foo\rbar""###)).unwrap(), "foo\rbar");
        assert_eq!(parse(&lit(r###""foo\tbar""###)).unwrap(), "foo\tbar");
        assert_eq!(parse(&lit(r###""foo\0bar""###)).unwrap(), "foo\0bar");
        assert_eq!(parse(&lit(r###""foo\\bar""###)).unwrap(), "foo\\bar");

        assert_eq!(parse(&lit(r###""foo\nbar\rbaz\tbuz""###)).unwrap(), "foo\nbar\rbaz\tbuz");

        assert_eq!(parse(&lit(r###""foo\x50bar""###)).unwrap(), "foo\x50bar");
        assert_eq!(parse(&lit(r###""foo\u{50}bar""###)).unwrap(), "foo\u{50}bar");
        assert_eq!(parse(&lit(r###""foo\u{228}bar""###)).unwrap(), "foo\u{228}bar");
        assert_eq!(parse(&lit(r###""foo\u{fffe}bar""###)).unwrap(), "foo\u{fffe}bar");
        assert_eq!(parse(&lit(r###""foo\u{1F923}bar""###)).unwrap(), "foo\u{1F923}bar");
    }

    #[test]
    fn wrong_kinds() {
        assert!(parse(&Literal::u8_suffixed(27)).is_err());
        assert!(parse(&Literal::u16_suffixed(27)).is_err());
        assert!(parse(&Literal::u32_suffixed(27)).is_err());
        assert!(parse(&Literal::u64_suffixed(27)).is_err());
        assert!(parse(&Literal::u128_suffixed(27)).is_err());
        assert!(parse(&Literal::usize_suffixed(27)).is_err());
        assert!(parse(&Literal::i8_suffixed(-27)).is_err());
        assert!(parse(&Literal::i16_suffixed(-27)).is_err());
        assert!(parse(&Literal::i32_suffixed(-27)).is_err());
        assert!(parse(&Literal::i64_suffixed(-27)).is_err());
        assert!(parse(&Literal::i128_suffixed(-27)).is_err());
        assert!(parse(&Literal::isize_suffixed(-27)).is_err());

        assert!(parse(&Literal::u8_unsuffixed(27)).is_err());
        assert!(parse(&Literal::u16_unsuffixed(27)).is_err());
        assert!(parse(&Literal::u32_unsuffixed(27)).is_err());
        assert!(parse(&Literal::u64_unsuffixed(27)).is_err());
        assert!(parse(&Literal::u128_unsuffixed(27)).is_err());
        assert!(parse(&Literal::usize_unsuffixed(27)).is_err());
        assert!(parse(&Literal::i8_unsuffixed(-27)).is_err());
        assert!(parse(&Literal::i16_unsuffixed(-27)).is_err());
        assert!(parse(&Literal::i32_unsuffixed(-27)).is_err());
        assert!(parse(&Literal::i64_unsuffixed(-27)).is_err());
        assert!(parse(&Literal::i128_unsuffixed(-27)).is_err());
        assert!(parse(&Literal::isize_unsuffixed(-27)).is_err());

        assert!(parse(&Literal::f32_unsuffixed(3.14)).is_err());
        assert!(parse(&Literal::f32_suffixed(3.14)).is_err());
        assert!(parse(&Literal::f64_unsuffixed(3.14)).is_err());
        assert!(parse(&Literal::f64_suffixed(3.14)).is_err());
        assert!(parse(&Literal::f32_unsuffixed(-3.14)).is_err());
        assert!(parse(&Literal::f32_suffixed(-3.14)).is_err());
        assert!(parse(&Literal::f64_unsuffixed(-3.14)).is_err());
        assert!(parse(&Literal::f64_suffixed(-3.14)).is_err());

        assert!(parse(&Literal::character('a')).is_err());
        assert!(parse(&Literal::byte_string(b"peter")).is_err());
    }
}

//! Parse the macro input into an intermediate representation.

use proc_macro2::{
    Span, TokenStream, Delimiter, TokenTree, Spacing,
    token_stream::IntoIter as TokenIterator,
};
use std::{collections::HashMap, convert::TryFrom};
use crate::{
    err::Error,
    ir::{Expr, WriteInput, FormatStr, FormatArgs},
};

mod fmt;
mod style;


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

/// Tries to parse a string literal.
pub(super) fn expect_str_literal(it: &mut TokenIterator) -> Result<(String, Span), Error> {
    let tt = it.next().ok_or(err!("expected string literal, found EOF"))?;
    let lit = litrs::StringLit::try_from(&tt)
        .map_err(|e| err!(tt.span(), "{}", e))?;

    Ok((lit.into_value().into_owned(), tt.span()))
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

        let mut exprs = Vec::new();
        let mut name_indices = HashMap::new();
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
                let index = exprs.len();
                exprs.push(expr);
                name_indices.insert(name, index);
            } else {
                if saw_named {
                    let e = err!(span, "positional argument after named arguments is not allowed");
                    return Err(e);
                }

                let expr = Expr {
                    tokens: vec![tt0, tt1].into_iter().filter_map(|tt| tt).chain(it).collect(),
                    span,
                };
                exprs.push(expr);
            }

        }

        Ok(Self { exprs, name_indices })
    }
}

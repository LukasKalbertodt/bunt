//! These are the docs for the crate `bunt-macros`. This is just implementation
//! detail, please see the crate `bunt` for the real docs.

use proc_macro::TokenStream as TokenStream1;
use proc_macro2::{Span, TokenStream};
use std::collections::HashMap;

#[macro_use]
mod err;
mod gen;
mod literal;
mod parse;

use crate::err::Error;


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


/// Input for the `write!` and `writeln!` macro.
#[derive(Debug)]
struct WriteInput {
    target: Expr,
    format_str: FormatStr,
    args: FormatArgs,
}

/// Our own `expr` type. We use this instead of `syn` to avoid `syn`
/// alltogether. We don't need to introspect the expression, we just need to
/// skip over them and the emit them again.
#[derive(Debug)]
struct Expr {
    span: Span,
    tokens: TokenStream,
}

/// A parsed format string.
#[derive(Debug)]
struct FormatStr {
    fragments: Vec<FormatStrFragment>,
}

impl FormatStr {
    /// Adds `\n` to the end of the formatting string.
    fn add_newline(&mut self) {
        match self.fragments.last_mut() {
            // If the last fragment is an `fmt` one, we can easily add the
            // newline to its last part (which is guaranteed to exist).
            Some(FormatStrFragment::Fmt { fmt_str_parts, .. }) => {
                fmt_str_parts.last_mut()
                    .expect("bug: fmt_str_parts empty")
                    .push('\n');
            }

            // Otherwise (style closing tag is last fragment), we have to add a
            // new `Fmt` fragment.
            _ => {
                self.fragments.push(FormatStrFragment::Fmt {
                    fmt_str_parts: vec!["\n".into()],
                    args: vec![],
                });
            }
        }
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

/// Parsed formatting arguments.
#[derive(Debug)]
struct FormatArgs {
    positional: Vec<Expr>,
    named: HashMap<String, Expr>,
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

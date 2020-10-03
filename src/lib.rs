//! This crate offers a couple of macros to easily print colored and formatted
//! text to a terminal. It is basically just a convenience API on top of
//! [`termcolor`](https://crates.io/crates/termcolor). Thus, some understanding
//! of `termcolor` is useful to use `bunt`.
//!
//! Mini example:
//!
//! ```
//! let ty = "u32";
//! bunt::println!("{$bold+red}error:{/$} invalid value for type `{[blue]}`", ty);
//! ```
//!
//! # Format string syntax
//!
//! The macros in this crate have almost the same syntax as the corresponding
//! `std::fmt` macros: arguments are inserted with `{}` and braces are escaped
//! with `{{` and `}}`. `bunt` has two additions to that syntax:
//!
//! ## Style tags
//!
//! With `{$style_spec}...{/$}`, you can apply a style to a section of your
//! string (which can also contain arguments). The start tag `{$...}` contains
//! the style specification, while the end tag is always `{/$}`. These tags can
//! also be nested.
//!
//! ```
//! bunt::println!("normal color ... {$yellow}Yellow :){/$} ... normal color again");
//! bunt::println!("{$bold}This is bold. {$red}This is also red!{/$} Just bold again{/$}.");
//! ```
//!
//!Each opening tag needs a matching closing one and the other way around.
//!
//! ```compile_fail
//! bunt::println!("{$red}unclosed tag :o");
//! ```
//!
//! ```compile_fail
//! bunt::println!("{$red}close it once{/$} and close it another time 🙁 {/$}");
//! ```
//!
//! ## Styled arguments
//!
//! If you want to style an argument, you can use tags right before and after
//! that argument. However, there is also a shorthand syntax: `{[style_spec]
//! ...}`. You can still use the syntax for named arguments, positional
//! arguments, width, fill/alignmen, precision, formatting traits and everything
//! else from `std::fmt` after the `[...]`.
//!
//! ```
//! // Normal output via `Display`. Equivalent to `"value: {$green}{}{/$}"`
//! bunt::println!("value: {[green]}", 27);
//!
//! // Output via `Debug`. All argument formatting syntax from `fmt` works
//! // inside the braces, after the `[...]`.
//! bunt::println!("value: {[green]:?}", vec![1, 2, 3]);
//!
//! // Named argument + precision specified: works.
//! bunt::println!("value: {[green]foo:.5}", foo = 3.14);
//! ```
//!
//! ## Style specification
//!
//! `bunt` has the same capabilities as `termcolor`. See [`termcolor::Color`]
//! and [`termcolor::ColorSpec`] for more information. The syntax for style
//! specs in `bunt` is a simple list of fragments that are joined by `+`.
//! Examples:
//!
//! - `red`
//! - `#ff8030+bold`
//! - `yellow+italic+intense`
//! - `bg:white+blue+bold`
//!
//! Full list of allowed fragments:
//!
//! - Colors:
//!   - `black`, `blue`, `green`, `red`, `cyan`, `magenta`, `yellow`, `white`
//!   - RGB as hex string: `#rrggbb`, e.g. `#27ae60`
//!   - (yes, there is currently no syntax for `Color::Ansi256` because I
//!     couldn't come up with a nice syntax and I never used these ansi
//!     colors...)
//! - Background colors: same as colors but prefixed with `bg:`, e.g. `bg:blue`
//!   or `bg:#c0392b`
//! - Attributes:
//!   - `bold`
//!   - `italic`
//!   - `underline`
//!   - `intense`
//!
//! `bunt` macros make sure that your style spec makes sense (only one
//! foreground/background color is allowed, duplicated attributes are not
//! allowed). Invalid style specs result in a compile error.
//!
//! ```compile_fail
//! bunt::println!("{$red+blue}what{/$}");
//! ```
//!
//! ```compile_fail
//! bunt::println!("{$bold+red+bold}you don't have to say it twice buddy{/$}");
//! ```
//!
//!
//! [`termcolor::Color`]: https://docs.rs/termcolor/1.1.0/termcolor/enum.Color.html
//! [`termcolor::ColorSpec`]: https://docs.rs/termcolor/1.1.0/termcolor/struct.ColorSpec.html
//!
//!
//! # Available macros
//!
//! - [`write`] and [`writeln`]: print to a `termcolor::WriteColor` instance.
//! - [`print`] and [`println`]: print to stdout.
//! - [`eprint`] and [`eprintln`]: print to stderr.
//! - [`style`]: parses a format specification and returns the corresponding
//!   `termcolor::ColorSpec` value.
//!
//! In larger applications, you should probably use `write!` and `writeln!` to
//! have more control over how the stdout/stderr handle is created. You usually
//! want to give your users the choice of color usage, e.g. via a `--color` CLI
//! argument.
//!
//!
//! # Passing multiple format strings (`concat!` replacement)
//!
//! In many cases, users wish to call `concat!` and pass the result as format
//! string to bunt's macros, e.g. `bunt::println!(concat!("foo", "bar"))`. This
//! is mainly used if you want to write your own macro to wrap bunt's macros.
//! Unfortunately, this is not easily possible as macros are expaned lazily. See
//! [issue #15](https://github.com/LukasKalbertodt/bunt/issues/15) for more
//! information.
//!
//! As a workaround for this fairly common use case, bunt allows passing an
//! "array of format strings", like so:
//!
//! ```
//! bunt::println!(["foo ", "{[red]} bar"], 27);
//! ```
//!
//! All given strings will be concatenated by `bunt`. So the above code is
//! equivalent to `bunt::println!("foo {[red]} bar", 27)`.
//!
//! For most users this feature is irrelevant. If possible, pass the format
//! string as single string literal.
//!

#![deny(intra_doc_link_resolution_failure)]

// Reexport of `termcolor`. This is mostly to be used by the code generated by
// the macros.
pub extern crate termcolor;

// To consistently refer to the macros crate.
#[doc(hidden)]
pub extern crate bunt_macros;

/// Writes formatted data to a `termcolor::WriteColor` target.
///
/// This is a more general version of `print` as you can specify the destination
/// of the formatted data as first parameter. `write` also returns a `Result<(),
/// std::io::Error>` which is `Err` in case writing to the target or setting the
/// color fails. `print!` simply panics in that case.
///
/// ```
/// use bunt::termcolor::{ColorChoice, StandardStream};
///
/// // Choosing a different color choice, just to show something `println`
/// // can't do.
/// let mut stdout = StandardStream::stdout(ColorChoice::Always);
/// let result = bunt::write!(stdout, "{$red}bad error!{/$}");
///
/// if result.is_err() {
///     // Writing to stdout failed...
/// }
/// ```
///
/// See crate-level docs for more information.
// pub use bunt_macros::write;
#[macro_export]
macro_rules! write {
    ($target:expr, $format_str:literal $(, $arg:expr)* $(,)?) => {
        $crate::write!($target, [$format_str] $(, $arg )*)
    };
    ($target:expr, [$($format_str:literal),+ $(,)?] $(, $arg:expr)* $(,)?) => {
        $crate::bunt_macros::write!(
            $target [$($format_str)+] $( $arg )*
        )
    };
}

/// Writes formatted data with newline to a `termcolor::WriteColor` target.
///
/// Like [`write!`], but adds a newline (`\n`) at the end.
///
/// ```
/// use bunt::termcolor::{ColorChoice, StandardStream};
///
/// // Choosing a different color choice, just to show something `println`
/// // can't do.
/// let mut stdout = StandardStream::stdout(ColorChoice::Always);
/// let _ = bunt::writeln!(stdout, "{$red}bad error!{/$}");
/// ```
///
/// See crate-level docs for more information.
#[macro_export]
macro_rules! writeln {
    ($target:expr, $format_str:literal $(, $arg:expr)* $(,)?) => {
        $crate::writeln!($target, [$format_str] $(, $arg )*)
    };
    ($target:expr, [$($format_str:literal),+ $(,)?] $(, $arg:expr)* $(,)?) => {
        $crate::bunt_macros::writeln!(
            $target [$($format_str)+] $( $arg )*
        )
    };
}

/// Writes formatted data to stdout (with `ColorChoice::Auto`).
///
/// This is like `write`, but always writes to
/// `StandardStream::stdout(termcolor::ColorChoice::Auto)`. `print` also does
/// not return a result, but instead panics if an error occurs writing to
/// stdout.
///
/// ```
/// bunt::print!("{$magenta}foo {[bold]} bar{/$}", 27);
/// ```
///
/// See crate-level docs for more information.
#[macro_export]
macro_rules! print {
    ($format_str:literal $(, $arg:expr)* $(,)?) => {
        $crate::print!([$format_str] $(, $arg )*)
    };
    ([$($format_str:literal),+ $(,)?] $(, $arg:expr)* $(,)?) => {
        $crate::bunt_macros::write!(
            ($crate::termcolor::StandardStream::stdout($crate::termcolor::ColorChoice::Auto))
            [$($format_str)+] $( $arg )*
        ).expect("failed to write to stdout in `bunt::print`")
    };
}

/// Writes formatted data with newline to stdout (with `ColorChoice::Auto`).
///
/// Like [`print!`], but adds a newline (`\n`) at the end.
///
/// ```
/// bunt::println!("{$cyan}foo {[bold]} bar{/$}", true);
/// ```
///
/// See crate-level docs for more information.
#[macro_export]
macro_rules! println {
    ($format_str:literal $(, $arg:expr)* $(,)?) => {
        $crate::println!([$format_str] $(, $arg )*)
    };
    ([$($format_str:literal),+ $(,)?] $(, $arg:expr)* $(,)?) => {
        $crate::bunt_macros::writeln!(
            ($crate::termcolor::StandardStream::stdout($crate::termcolor::ColorChoice::Auto))
            [$($format_str)+] $( $arg )*
        ).expect("failed to write to stdout in `bunt::println`")
    };
}

/// Writes formatted data to stderr (with `ColorChoice::Auto`).
///
/// This is like `write`, but always writes to
/// `StandardStream::stderr(termcolor::ColorChoice::Auto)`. `eprint` also does
/// not return a result, but instead panics if an error occurs writing to
/// stderr.
///
/// ```
/// bunt::eprint!("{$magenta}foo {[bold]} bar{/$}", 27);
/// ```
///
/// See crate-level docs for more information.
#[macro_export]
macro_rules! eprint {
    ($format_str:literal $(, $arg:expr)* $(,)?) => {
        $crate::eprint!([$format_str] $(, $arg )*)
    };
    ([$($format_str:literal),+ $(,)?] $(, $arg:expr)* $(,)?) => {
        $crate::bunt_macros::write!(
            ($crate::termcolor::StandardStream::stderr($crate::termcolor::ColorChoice::Auto))
            [$($format_str)+] $( $arg )*
        ).expect("failed to write to stderr in `bunt::eprint`")
    };
}

/// Writes formatted data with newline to stderr (with `ColorChoice::Auto`).
///
/// Like [`eprint!`], but adds a newline (`\n`) at the end.
///
/// ```
/// bunt::eprintln!("{$cyan}foo {[bold]} bar{/$}", true);
/// ```
///
/// See crate-level docs for more information.
#[macro_export]
macro_rules! eprintln {
    ($format_str:literal $(, $arg:expr)* $(,)?) => {
        $crate::eprintln!([$format_str] $(, $arg )*)
    };
    ([$($format_str:literal),+ $(,)?] $(, $arg:expr)* $(,)?) => {
        $crate::bunt_macros::writeln!(
            ($crate::termcolor::StandardStream::stderr($crate::termcolor::ColorChoice::Auto))
            [$($format_str)+] $( $arg )*
        ).expect("failed to write to stderr in `bunt::eprintln`")
    };
}

/// Parses the given style specification string and returns the corresponding
/// `termcolor::ColorSpec` value.
///
/// ```
/// use bunt::termcolor::{Color, ColorChoice, StandardStream, WriteColor};
///
/// let style = bunt::style!("red+bold+bg:yellow");
/// let mut stdout = StandardStream::stdout(ColorChoice::Auto);
/// stdout.set_color(&style)?;
///
/// assert_eq!(style.fg(), Some(&Color::Red));
/// assert_eq!(style.bg(), Some(&Color::Yellow));
/// assert!(style.bold());
/// assert!(!style.italic());
/// assert!(!style.underline());
/// assert!(!style.intense());
/// # std::io::Result::Ok(())
/// ```
///
/// See crate-level docs for more information.
pub use bunt_macros::style;

//! Generating the output tokens from the parsed intermediate representation.

use proc_macro2::{Ident, Span, TokenStream};
use quote::quote;
use std::{
    cell::RefCell,
    collections::BTreeSet,
    fmt::{self, Write},
};
use crate::{
    err::Error,
    ir::{WriteInput, FormatStrFragment, ArgRefKind, Style, Color, Expr, FormatSpec, Align, Sign, Width, Precision},
};


impl WriteInput {
    pub(crate) fn gen_output(&self) -> Result<TokenStream, Error> {
        fn arg_ident(id: usize) -> Ident {
            Ident::new(&format!("arg{}", id), Span::mixed_site())
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
        for (i, arg) in self.args.exprs.iter().enumerate() {
            let ident = arg_ident(i);
            arg_bindings.extend(quote! {
                let #ident = &#arg;
            })
        }

        // Prepare the actual process of writing to the target according to the
        // format string.
        let buf = Ident::new("buf", Span::mixed_site());
        let mut style_stack = Vec::new();
        let mut writes = TokenStream::new();
        let mut next_arg_index = 0;

        // We want to keep track of all expressions that were used somehow to
        // emit an error if there are unused ones. As the easiest way to update
        // this is within `ident_for_pos` and `ident_for_name` and since two
        // closures cannot borrow the vector mutably at the same time, we use a
        // ref cell. It's totally fine here as the vector will only be borrowed
        // very briefly.
        let used_expressions = RefCell::new(vec![false; self.args.exprs.len()]);

        for segment in &self.format_str.fragments {
            match segment {
                // A formatting fragment. This is the more tricky one. We have
                // to construct a `std::write!` invocation that has the right
                // fmt string, the right arguments (and no additional ones!) and
                // the correct argument references.
                FormatStrFragment::Fmt { fmt_str_parts, args } => {
                    // Two helper functions to get the correct identifier for an
                    // argument.
                    let ident_for_pos = |pos| -> Result<Ident, Error> {
                        if self.args.exprs.get(pos).is_none() {
                            return Err(err!(
                                "invalid reference to positional argument {} (there are \
                                    not that many arguments)",
                                pos,
                            ));
                        }

                        used_expressions.borrow_mut()[pos] = true;
                        Ok(arg_ident(pos))
                    };
                    let ident_for_name = |name| -> Result<Ident, Error> {
                        let index = self.args.name_indices.get(name)
                            .ok_or(err!("there is no argument named `{}`", name))?;

                        used_expressions.borrow_mut()[*index] = true;
                        Ok(arg_ident(*index))
                    };

                    let mut fmt_str = fmt_str_parts[0].clone();
                    let mut used_args = BTreeSet::new();

                    for (i, arg) in args.into_iter().enumerate() {
                        // Check width and precision parameters. Those are the
                        // only two things in the format spec that can refer to
                        // arguments. If they do, we change them to always refer
                        // to a named parameter hat is inserted into
                        // `used_args`.
                        //
                        // We check those before the main argument because of
                        // the `.*` precision modifier which, like `{}`, refers
                        // to the next argument. BUT the `.*` comes first. So
                        // `println!("{:.*}", 2, 3.1415926)` prints `3.14` and
                        // swapping the arguments would result in an error.
                        let mut format_spec = arg.format_spec.clone();

                        match &arg.format_spec.width {
                            None | Some(Width::Constant(_)) => {}
                            Some(Width::Name(name)) => {
                                let ident = ident_for_name(name)?;
                                format_spec.width = Some(Width::Name(ident.to_string()));
                                used_args.insert(ident);
                            }
                            Some(Width::Position(pos)) => {
                                let ident = ident_for_pos(*pos)?;
                                format_spec.width = Some(Width::Name(ident.to_string()));
                                used_args.insert(ident);
                            }
                        }

                        match &arg.format_spec.precision {
                            None | Some(Precision::Constant(_)) => {}
                            Some(Precision::Name(name)) => {
                                let ident = ident_for_name(name)?;
                                format_spec.precision = Some(Precision::Name(ident.to_string()));
                                used_args.insert(ident);
                            }
                            Some(Precision::Position(pos)) => {
                                let ident = ident_for_pos(*pos)?;
                                format_spec.precision = Some(Precision::Name(ident.to_string()));
                                used_args.insert(ident);
                            }
                            Some(Precision::Bundled) => {
                                if self.args.exprs.get(next_arg_index).is_none() {
                                    return Err(err!(
                                        "invalid '.*' precision argument reference to \
                                            argument {} (too few actual arguments)",
                                        next_arg_index,
                                    ));
                                }

                                let ident = arg_ident(next_arg_index);
                                format_spec.precision = Some(Precision::Name(ident.to_string()));

                                used_expressions.borrow_mut()[next_arg_index] = true;
                                used_args.insert(ident);
                                next_arg_index += 1;
                            }
                        }

                        // Check the main argument.
                        let ident = match &arg.kind {
                            ArgRefKind::Next => {
                                if self.args.exprs.get(next_arg_index).is_none() {
                                    return Err(
                                        err!("invalid '{{}}' argument reference \
                                            (too few actual arguments)")
                                    );
                                }

                                used_expressions.borrow_mut()[next_arg_index] = true;
                                let ident = arg_ident(next_arg_index);
                                next_arg_index += 1;
                                ident
                            }
                            ArgRefKind::Position(pos) => ident_for_pos(*pos)?,
                            ArgRefKind::Name(name) => ident_for_name(name)?,
                        };

                        // Create the full fmt argument and also push the next
                        // string part.
                        std::write!(fmt_str, "{{{}:{}}}", ident, format_spec).unwrap();
                        used_args.insert(ident);
                        fmt_str.push_str(&fmt_str_parts[i + 1]);
                    }


                    // Combine everything in `write!` invocation.
                    writes.extend(quote! {
                        if let std::result::Result::Err(e)
                            = std::write!(#buf, #fmt_str #(, #used_args = #used_args)* )
                        {
                            break std::result::Result::Err(e);
                        }
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
                        if let std::result::Result::Err(e)
                            = ::bunt::termcolor::WriteColor::set_color(#buf, &#style_def)
                        {
                            break std::result::Result::Err(e);
                        }
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
                        if let std::result::Result::Err(e)
                            = ::bunt::termcolor::WriteColor::set_color(#buf, &#style_def)
                        {
                            break std::result::Result::Err(e);
                        }
                    });
                }
            }
        }

        // Check if the style tags are balanced
        if !style_stack.is_empty() {
            return Err(err!("unclosed style tag"));
        }

        if let Some(unused_pos) = used_expressions.into_inner().iter().position(|used| !used) {
            let expr = &self.args.exprs[unused_pos];
            return Err(err!(expr.span, "argument never used: `{}`", expr.tokens));
        }

        // Combine everything.
        let target = &self.target;
        Ok(quote! {
            loop {
                use std::io::Write as _;

                #arg_bindings
                let #buf = &mut #target;
                #writes

                break Ok(());
            }
        })
    }
}

impl quote::ToTokens for Expr {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        tokens.extend(self.tokens.clone())
    }
}

impl fmt::Display for FormatSpec {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(fill) = self.fill {
            f.write_char(fill)?;
        }
        if let Some(align) = self.align {
            let c = match align {
                Align::Left => '<',
                Align::Center => '^',
                Align::Right => '>',
            };
            f.write_char(c)?;
        }
        if let Some(sign) = self.sign {
            let c = match sign {
                Sign::Plus => '+',
                Sign::Minus => '-',
            };
            f.write_char(c)?;
        }
        if self.alternate {
            f.write_char('#')?;
        }
        if self.zero {
            f.write_char('0')?;
        }

        match &self.width {
            Some(Width::Constant(n)) => write!(f, "{}", n)?,
            Some(Width::Position(n)) => write!(f, "{}$", n)?,
            Some(Width::Name(s)) => write!(f, "{}$", s)?,
            None => {}
        }

        match &self.precision {
            Some(Precision::Constant(n)) => write!(f, ".{}", n)?,
            Some(Precision::Position(n)) => write!(f, ".{}$", n)?,
            Some(Precision::Name(s)) => write!(f, ".{}$", s)?,
            Some(Precision::Bundled) => write!(f, ".*")?,
            None => {}
        }

        if let Some(t) = self.ty {
            f.write_char(t)?;
        }

        Ok(())
    }
}

impl Style {
    /// Returns a token stream representing an expression constructing the
    /// `ColorSpec` value corresponding to `self`.
    pub(crate) fn to_tokens(&self) -> TokenStream {
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
        attr!(dimmed, set_dimmed);
        attr!(italic, set_italic);
        attr!(underline, set_underline);
        attr!(intense, set_intense);

        quote! {
            {
                let mut #ident = ::bunt::termcolor::ColorSpec::new();
                #method_calls
                #ident
            }
        }
    }
}

impl Color {
    /// Returns a token stream representing a value of type `termcolor::Color`.
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

        quote! { ::bunt::termcolor::Color:: #variant }
    }
}

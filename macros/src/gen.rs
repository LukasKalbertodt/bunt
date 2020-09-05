//! Generating the output tokens from the parsed intermediate representation.

use proc_macro2::{Ident, Span, TokenStream};
use quote::quote;
use std::{
    collections::BTreeSet,
    fmt::Write,
};
use crate::{Error, WriteInput, FormatStrFragment, ArgRefKind, Style, Color, Expr};


impl WriteInput {
    pub(crate) fn gen_output(&self) -> Result<TokenStream, Error> {
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
        for (i, arg) in self.args.positional.iter().enumerate() {
            let ident = pos_arg_ident(i as u32);
            arg_bindings.extend(quote! {
                let #ident = &#arg;
            })
        }
        for (name, arg) in self.args.named.iter() {
            let ident = name_arg_ident(name);
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

        for segment in &self.format_str.fragments {
            match segment {
                // A formatting fragment. This is the more tricky one. We have
                // to construct a `std::write!` invocation that has the right
                // fmt string, the right arguments (and no additional ones!) and
                // the correct argument references.
                FormatStrFragment::Fmt { fmt_str_parts, args } => {
                    let mut fmt_str = fmt_str_parts[0].clone();
                    let mut used_args = BTreeSet::new();

                    for (i, arg) in args.into_iter().enumerate() {
                        let ident = match &arg.kind {
                            ArgRefKind::Next => {
                                let ident = pos_arg_ident(next_arg_index as u32);
                                if self.args.positional.get(next_arg_index).is_none() {
                                    return Err(
                                        err!("invalid '{{}}' argument reference \
                                            (too few actual arguments)")
                                    );
                                }

                                next_arg_index += 1;
                                ident
                            }
                            ArgRefKind::Position(pos) => {
                                let ident = pos_arg_ident(*pos);
                                if self.args.positional.get(*pos as usize).is_none() {
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
                                if self.args.named.get(name).is_none() {
                                    return Err(err!("there is no argument named `{}`", name));
                                }

                                ident
                            }
                        };

                        std::write!(fmt_str, "{{{}{}}}", ident, arg.format_spec).unwrap();
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
                        ::bunt::termcolor::WriteColor::set_color(#buf, &#style_def)?;
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
                        ::bunt::termcolor::WriteColor::set_color(#buf, &#style_def)?;
                    });
                }
            }
        }

        // Check if the style tags are balanced
        if !style_stack.is_empty() {
            return Err(err!("unclosed style tag"));
        }

        // Combine everything.
        let target = &self.target;
        Ok(quote! {
            (|| -> Result<(), ::std::io::Error> {
                use std::io::Write as _;

                #arg_bindings
                let #buf = &mut #target;
                #writes

                Ok(())
            })()
        })
    }
}

impl quote::ToTokens for Expr {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        tokens.extend(self.tokens.clone())
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

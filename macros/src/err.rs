use proc_macro2::{Span, TokenStream};
use quote::quote_spanned;


/// Helper macro to easily create an error with a span.
macro_rules! err {
    ($fmt:literal $($t:tt)*) => { Error { span: Span::call_site(), msg: format!($fmt $($t)*) } };
    ($span:expr, $($t:tt)+) => { Error { span: $span, msg: format!($($t)+) } };
}

/// Simply contains a message and a span. Can be converted to a `compile_error!`
/// via `to_compile_error`.
#[derive(Debug)]
pub(crate) struct Error {
    pub(crate) msg: String,
    pub(crate) span: Span,
}

impl Error {
    pub(crate) fn to_compile_error(&self) -> TokenStream {
        let msg = &self.msg;
        quote_spanned! {self.span=>
            {
                compile_error!(#msg);
            }
        }
    }
}

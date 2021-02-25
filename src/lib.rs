pub extern crate bunt_macros;

#[macro_export]
macro_rules! foo {
    ($target:expr) => {
        $crate::bunt_macros::writeln!($target)
    };
}

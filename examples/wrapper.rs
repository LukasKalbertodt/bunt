//! Sometimes, you might want to wrap bunt's macros into your own
//! project-specific macro. Often, you want to print a prefix or something like
//! that.


macro_rules! log {
    ($fmt:literal $(, $arg:expr)* $(,)?) => {
        bunt::println!(
            // Bunt macros allow to pass an "array" of format strings which are
            // then concatenated by bunt.
            [
                // Our prefix
                "[{[magenta] log_module_path}] ",
                // What the user passed
                $fmt,
                // Our postfix
                " {$cyan}({log_file}:{log_line}){/$}",
            ],
            $($arg ,)*
            log_module_path = std::module_path!(),
            log_file = std::file!(),
            log_line = std::line!(),
        )

        // This solution is not optimal though. For one, it would be nice to
        // `stringify!(std::module_path!())` instead of passing it as runtime
        // string argument. That's not possible because macro expansions are
        // lazy.
        //
        // Futhermore, we pass the arguments with names like `log_module_path`.
        // In theory, the user could also use a named argument with that name.
        // This `log!` macro is very much an internal helper macro and not
        // something you would want to put into your public API.
    };
}


fn main() {
    log!("Hello {}", "peter");
    banana::do_something();
}

mod banana {
    pub fn do_something() {
        log!("I'm doing something with {[yellow]:?}", vec![1, 2, 4]);
    }
}

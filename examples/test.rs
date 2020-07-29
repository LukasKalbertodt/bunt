




fn main() {
    // let x = bunt::style!("bg:#ffffff+bold+italic");
    // let mut w = termcolor::StandardStream::stdout(termcolor::ColorChoice::Auto);
    let name = "peter";
    let _ = bunt::println!(
        "a {[magenta] 1} b {[red]} c {[yellow] foo} d {[green] 0} e {[cyan]} x",
        name,
        true,
        foo = 27,
    );
}

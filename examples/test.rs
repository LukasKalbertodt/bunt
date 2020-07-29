




fn main() {
    // let x = bunt::style!("bg:#ffffff+bold+italic");
    let w = termcolor::StandardStream::stdout(termcolor::ColorChoice::Auto);
    let name = "peter";
    // let _ = bunt::write!(
    //     w,
    //     "a {[magenta] 1} b {[red]} c {[yellow] foo} d {[green] 0} e {[cyan]}\n",
    //     name,
    //     true,
    //     foo = 27,
    // );

    let _ = bunt::write!(
        w,
        "{$yellow} a {1} b {} c {foo} d {0} e {}{$/}\n",
        name,
        true,
        foo = 27,
    );
}

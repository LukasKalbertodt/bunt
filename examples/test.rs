




fn main() {
    // let x = bunt::style!("bg:#ffffff+bold+italic");
    let w = termcolor::StandardStream::stdout(termcolor::ColorChoice::Auto);
    let _ = bunt::write!(w, "Hey you {$red}lovely {$bold}person{$/}{$/}, your name is {[blue]}\n", name);
}

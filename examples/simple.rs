fn main() {
    println!();
    bunt::println!("I really like {$yellow}lemons{/$}! Like, {$blue+italic}a lot{/$}.");

    let v = vec![1, 2, 3];
    bunt::println!(
        "Here is some data: {[green]:?}. {$bold}Length: {[cyan]}{/$}",
        v,
        v.len()
    );
    println!();

    let ty = "u32";
    bunt::println!(
        "{$bold+red}error:{/$} invalid value for type `{[blue]}`",
        ty
    );
    bunt::println!("");
    bunt::println!("{$italic}Just {$yellow}kidding{/$}{/$}, there is no {$magenta}error{/$} :)");
    println!();
}

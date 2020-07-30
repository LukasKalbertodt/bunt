fn main() {
    let ty = "u32";
    bunt::println!("{$bold+red}error:{/$} invalid value for type `{[blue]}`", ty);
    bunt::println!("");
    bunt::println!("{$italic}Just {$yellow}kidding{/$}{/$}, there is no {$magenta}error{/$} :)");
}

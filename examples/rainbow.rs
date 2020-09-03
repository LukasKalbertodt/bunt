fn main() {
    // ===== Foreground vs. style ===============================================================
    let dummy = "Bunt ♥";
    let sep = "     ";

    bunt::println!("{$bold+blue+intense}Foreground colors and styles:{/$}");
    println!();

    println!("         normal     bold       italic     underline  intense    bold+intense");
    bunt::println!(
        "black    {$black}\
            {0}{1}{[bold]0}{1}{[italic]0}{1}{[underline]0}{1}{[intense]0}{1}{[bold+intense]0}\
            {/$}",
        dummy,
        sep,
    );
    bunt::println!(
        "blue     {$blue}\
            {0}{1}{[bold]0}{1}{[italic]0}{1}{[underline]0}{1}{[intense]0}{1}{[bold+intense]0}\
            {/$}",
        dummy,
        sep,
    );
    bunt::println!(
        "green    {$green}\
            {0}{1}{[bold]0}{1}{[italic]0}{1}{[underline]0}{1}{[intense]0}{1}{[bold+intense]0}\
            {/$}",
        dummy,
        sep,
    );
    bunt::println!(
        "red      {$red}\
            {0}{1}{[bold]0}{1}{[italic]0}{1}{[underline]0}{1}{[intense]0}{1}{[bold+intense]0}\
            {/$}",
        dummy,
        sep,
    );
    bunt::println!(
        "cyan     {$cyan}\
            {0}{1}{[bold]0}{1}{[italic]0}{1}{[underline]0}{1}{[intense]0}{1}{[bold+intense]0}\
            {/$}",
        dummy,
        sep,
    );
    bunt::println!(
        "magenta  {$magenta}\
            {0}{1}{[bold]0}{1}{[italic]0}{1}{[underline]0}{1}{[intense]0}{1}{[bold+intense]0}\
            {/$}",
        dummy,
        sep,
    );
    bunt::println!(
        "yellow   {$yellow}\
            {0}{1}{[bold]0}{1}{[italic]0}{1}{[underline]0}{1}{[intense]0}{1}{[bold+intense]0}\
            {/$}",
        dummy,
        sep,
    );
    bunt::println!(
        "white    {$white}\
            {0}{1}{[bold]0}{1}{[italic]0}{1}{[underline]0}{1}{[intense]0}{1}{[bold+intense]0}\
            {/$}",
        dummy,
        sep,
    );

    // ===== Foreground vs. style ===============================================================
    println!();
    bunt::println!("{$bold+blue+intense}Foreground and background colors:{/$}");
    println!();

    let dummy = "  Bunt ♥   ";

    println!(
        "         bg:black   bg:blue    bg:green   bg:red     bg:cyan    \
        bg:magenta bg:yellow  bg:white"
    );
    bunt::println!(
        "black    {$black}\
            {[bg:black]0}{[bg:blue]0}{[bg:green]0}{[bg:red]0}{[bg:cyan]0}{[bg:magenta]0}{[bg:yellow]0}{[bg:white]0}\
            {/$}",
        dummy,
    );
    bunt::println!(
        "blue     {$blue}\
            {[bg:black]0}{[bg:blue]0}{[bg:green]0}{[bg:red]0}{[bg:cyan]0}{[bg:magenta]0}{[bg:yellow]0}{[bg:white]0}\
            {/$}",
        dummy,
    );
    bunt::println!(
        "green    {$green}\
            {[bg:black]0}{[bg:blue]0}{[bg:green]0}{[bg:red]0}{[bg:cyan]0}{[bg:magenta]0}{[bg:yellow]0}{[bg:white]0}\
            {/$}",
        dummy,
    );
    bunt::println!(
        "red      {$red}\
            {[bg:black]0}{[bg:blue]0}{[bg:green]0}{[bg:red]0}{[bg:cyan]0}{[bg:magenta]0}{[bg:yellow]0}{[bg:white]0}\
            {/$}",
        dummy,
    );
    bunt::println!(
        "cyan     {$cyan}\
            {[bg:black]0}{[bg:blue]0}{[bg:green]0}{[bg:red]0}{[bg:cyan]0}{[bg:magenta]0}{[bg:yellow]0}{[bg:white]0}\
            {/$}",
        dummy,
    );
    bunt::println!(
        "magenta  {$magenta}\
            {[bg:black]0}{[bg:blue]0}{[bg:green]0}{[bg:red]0}{[bg:cyan]0}{[bg:magenta]0}{[bg:yellow]0}{[bg:white]0}\
            {/$}",
        dummy,
    );
    bunt::println!(
        "yellow   {$yellow}\
            {[bg:black]0}{[bg:blue]0}{[bg:green]0}{[bg:red]0}{[bg:cyan]0}{[bg:magenta]0}{[bg:yellow]0}{[bg:white]0}\
            {/$}",
        dummy,
    );
    bunt::println!(
        "white    {$white}\
            {[bg:black]0}{[bg:blue]0}{[bg:green]0}{[bg:red]0}{[bg:cyan]0}{[bg:magenta]0}{[bg:yellow]0}{[bg:white]0}\
            {/$}",
        dummy,
    );

    println!();

    // TODO: add a real rainbow with RGB colors
}

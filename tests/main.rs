use std::fmt;
use bunt::{
    write, writeln, print, println, eprint, eprintln,
    termcolor::Buffer,
};


fn buf() -> Buffer {
    Buffer::ansi()
}
fn raw_str(buf: &Buffer) -> &str {
    std::str::from_utf8(buf.as_slice()).expect("test produced non-UTF8 string")
}

// Helper macro that checks the output of a `write!` invocation against an
// expected string.
macro_rules! check {
    ($expected:literal == $($t:tt)*) => {{
        let mut buf = buf();
        write!(buf, $($t)*).expect("failed to write to buffer in test");
        let actual = raw_str(&buf);
        if $expected != actual {
            panic!(
                "incorrect output for `write!({})`:\n   \
                    expected: {:?} ({})\n     \
                    actual: {:?} ({})\n",
                stringify!($($t)*),
                $expected,
                $expected,
                actual,
                actual,
            );
        }
    }};
}

#[test]
fn writeln() {
    let mut b = buf();
    writeln!(b, "").unwrap();
    assert_eq!(raw_str(&b), "\n");

    let mut b = buf();
    writeln!(b, "hello").unwrap();
    assert_eq!(raw_str(&b), "hello\n");

    let mut b = buf();
    writeln!(b, "a {$red}b{/$}").unwrap();
    assert_eq!(raw_str(&b), "a \x1b[0m\x1b[31mb\x1b[0m\n");
}

#[test]
fn no_move_buffer() {
    {
        let mut b = buf();
        write!(b, "hi").unwrap();
        drop(b);
    }
    {
        let mut b = buf();
        write!(&mut b, "hi").unwrap();
        drop(b);
    }

    write!(buf(), "hi").unwrap();
    write!(&mut buf(), "hi").unwrap();
}

#[test]
fn no_move_args() {
    #[derive(Debug)]
    struct NoCopy;

    impl fmt::Display for NoCopy {
        fn fmt(&self, _: &mut fmt::Formatter) -> fmt::Result {
            Ok(())
        }
    }

    {
        let string = "hi".to_string();
        let no_copy = NoCopy;
        let mut b = buf();
        write!(b, "a{[green]}b{:?}c", string, no_copy).unwrap();
        assert_eq!(raw_str(&b), "a\x1b[0m\x1b[32mhi\x1b[0mbNoCopyc");

        // We can still use the variables.
        drop(string);
        drop(no_copy);
    }
    {
        let s = "x".to_string();
        let mut b = buf();
        writeln!(b, "a{:?}b{}", s, s).unwrap();
        drop(s);
    }

    // The `print[ln]` use termcolor and thus circumvent the stdout capture by
    // the Rust test harness. To avoid ugly test output, we print basically no
    // text. Only `println` actually emits one newline.
    {
        let x = NoCopy;
        print!("{}", x);
        drop(x);
    }
    {
        let x = NoCopy;
        println!("{}", x);
        drop(x);
    }
}

#[test]
fn empty_ln() {
    // Just make sure they compile, we cannot easily capture and test their output.
    println!();
    eprintln!();


    let mut b = buf();
    writeln!(b).unwrap();
    writeln!(b,).unwrap();
    assert_eq!(raw_str(&b), "\n\n");
}

#[test]
fn arg_referal() {
    check!("27" == "{peter}", peter = 27);
    check!("27 27" == "{0} {0}", 27);

    check!(
        "a p c b a m" ==
        "{} {peter} {2} {} {0} {mary}", 'a', 'b', 'c', peter = 'p', mary = 'm'
    );

    check!("3 5" == "{} {}", foo = 3, bar = 5);
    check!("3 7 5 3 5 7" == "{} {baz} {} {0} {bar} {}", foo = 3, bar = 5, baz = 7);
}

#[test]
fn arg_referal_width() {
    check!("10|foo       |true" == "{}|{:0$}|{}", 10, "foo", true);
    check!("10|foo    |true" == "{}|{:3$}|{}", 10, "foo", true, 7);
    check!("10|foo    |true" == "{}|{:wid$}|{}", 10, "foo", true, wid = 7);
    check!("10|foo    |7" == "{}|{:wid$}|{wid}", 10, "foo", wid = 7);

    check!("bar|    5|5" == "{}|{1:1$}|{}", "bar", 5);
    check!("bar|anna |5" == "{}|{name:1$}|{}", "bar", 5, name = "anna");
    check!("bar|        5|5" == "{}|{1:wid$}|{}", "bar", 5, wid = 9);
    check!("bar|anna     |5" == "{}|{name:wid$}|{}", "bar", 5, wid = 9, name = "anna");
}

#[test]
fn arg_referal_precision() {
    check!("2|3.14|true" == "{}|{:.0$}|{}", 2, 3.1415926, true);
    check!("2|3.142|true" == "{}|{:.3$}|{}", 2, 3.1415926, true, 3);
    check!("2|3.142|true" == "{}|{:.prec$}|{}", 2, 3.1415926, true, prec = 3);
    check!("2|3.142|3" == "{}|{:.prec$}|{prec}", 2, 3.1415926, prec = 3);

    check!("bar|3.1|1" == "{}|{2:.1$}|{}", "bar", 1, 3.1415926);
    check!("bar|3.1|1" == "{}|{pi:.1$}|{}", "bar", 1, pi = 3.1415926);
    check!("bar|3.1|1" == "{}|{pi:.prec$}|{}", "bar", 1, pi = 3.1415926, prec = 1);
    check!("bar|3.1|1" == "{}|{2:.prec$}|{}", "bar", 1, 3.1415926, prec = 1);

    check!("foo|3.14|true" == "{}|{:.*}|{}", "foo", 2, 3.1415926, true);
    check!("3.1415926|3.14|foo" == "{}|{0:.*}|{}", 3.1415926, 2, "foo");
    check!("true|3.14|foo" == "{}|{pi:.*}|{}", true, 2, "foo", pi = 3.1415926);
}

#[test]
fn raw_strings() {
    check!("hello" == r"hello");
    check!(r"a\n" == r"a\n");
}

#[test]
fn no_style() {
    check!("hello" == "hello");
    check!("Foo 27 bar" == "Foo {} bar", 27);
    check!("x Foo 27 bar" == "{} Foo {} bar", 'x', 27);
    check!("Foo 8 barfren" == "Foo {} bar{}", 8, "fren");

    check!(
        "a\nb\tc\rd\0e\x48f\u{50}g\u{228}h\u{fffe}i\u{1F923}j" ==
        "a\nb\tc\rd\0e\x48f\u{50}g\u{228}h\u{fffe}i\u{1F923}j"
    );
    check!(
        "abc\
            def" ==
        "abcdef"
    );
}

#[test]
fn simple_tags() {
    check!("a\x1b[0m\x1b[31mb\x1b[0mc" == "a{$red}b{/$}c");
    check!("a\x1b[0m\x1b[33mbanana\x1b[0m" == "a{$yellow}banana{/$}");
    check!("\x1b[0m\x1b[34mocean\x1b[0m is wet" == "{$blue}ocean{/$} is wet");
    check!("\x1b[0m\x1b[1meverything\x1b[0m" == "{$bold}everything{/$}");

    check!("foo\x1b[0m\x1b[1m\x1b[32mbar\x1b[0mbaz" == "foo{$bold+green}bar{/$}baz");
    check!(
        "foo\x1b[0m\x1b[1m\x1b[32m\x1b[44mbar\x1b[0mbaz" ==
        "foo{$bold+green+bg:blue}bar{/$}baz"
    );
    check!(
        "foo\x1b[0m\x1b[1m\x1b[3m\x1b[38;5;10m\x1b[48;5;12mbar\x1b[0mbaz" ==
        "foo{$bold+green+bg:blue+intense+italic}bar{/$}baz"
    );
}

#[test]
fn attributes() {
    check!("a\x1b[0m\x1b[1mb\x1b[0mc" == "a{$bold}b{/$}c");
    check!("a\x1b[0m\x1b[2mb\x1b[0mc" == "a{$dimmed}b{/$}c");
    check!("a\x1b[0m\x1b[3mb\x1b[0mc" == "a{$italic}b{/$}c");
    check!("a\x1b[0m\x1b[4mb\x1b[0mc" == "a{$underline}b{/$}c");
}

#[test]
fn nested_tags() {
    check!(
        "a\x1b[0m\x1b[31mb\x1b[0m\x1b[1m\x1b[31mc\x1b[0m\x1b[31md\x1b[0me" ==
        "a{$red}b{$bold}c{/$}d{/$}e"
    );
    check!(
        "a\x1b[0m\x1b[31mb\x1b[0m\x1b[33mc\x1b[0m\x1b[31md\x1b[0me" ==
        "a{$red}b{$yellow}c{/$}d{/$}e"
    );
    check!(
        "a\x1b[0m\x1b[31mb\x1b[0m\x1b[1m\x1b[31mc\x1b[0m\x1b[1m\x1b[33md\x1b[0m\x1b[1m\x1b[31me\
            \x1b[0m\x1b[31mf\x1b[0mg" ==
        "a{$red}b{$bold}c{$yellow}d{/$}e{/$}f{/$}g"
    );
}

#[test]
fn colored_args() {
    check!("\x1b[0m\x1b[32m27\x1b[0m" == "{[green]}", 27);
    check!("a\x1b[0m\x1b[32m27\x1b[0m" == "a{[green]}", 27);
    check!("\x1b[0m\x1b[32m27\x1b[0mb" == "{[green]}b", 27);
    check!("a\x1b[0m\x1b[32m27\x1b[0mb" == "a{[green]}b", 27);

    check!("\x1b[0m\x1b[35mtrue\x1b[0m" == "{[magenta]:?}", true);
    check!("\x1b[0m\x1b[35m3f\x1b[0m" == "{[magenta]:x}", 0x3f);
    check!("\x1b[0m\x1b[35m3F\x1b[0m" == "{[magenta]:X}", 0x3f);
    check!("\x1b[0m\x1b[35m123\x1b[0m" == "{[magenta]:o}", 0o123);
    check!("\x1b[0m\x1b[35m101010\x1b[0m" == "{[magenta]:b}", 0b101010);
    check!("\x1b[0m\x1b[35m3.14e0\x1b[0m" == "{[magenta]:e}", 3.14);
    check!("\x1b[0m\x1b[35m3.14E0\x1b[0m" == "{[magenta]:E}", 3.14);

    check!(
        "a \x1b[0m\x1b[32m27\x1b[0m b \x1b[0m\x1b[31mtrue\x1b[0m c \x1b[0m\x1b[33mbanana\x1b[0m" ==
        "a {[green]} b {[red]} c {[yellow]}", 27, true, "banana"
    );
    check!(
        "a \x1b[0m\x1b[32m27\x1b[0m b \x1b[0m\x1b[1m3.14\x1b[0m c" ==
        "a {[green]} b {[bold]} c", 27, 3.14
    );
}

#[test]
fn mixed_tag_args() {
    check!(
        "a \x1b[0m\x1b[1mb\x1b[0m\x1b[1m\x1b[32m27\x1b[0m\x1b[1mc\x1b[0md" ==
        "a {$bold}b{[green]}c{/$}d", 27
    );

    check!(
        "a \x1b[0m\x1b[33m...\x1b[0m\x1b[38;5;11m27\x1b[0m\x1b[33m...\x1b[0m\x1b[1m\x1b\
            [33mb\x1b[0m\x1b[1m\x1b[32mtrue\x1b[0m\x1b[1m\x1b[33mc\x1b[0m\x1b[33m\x1b[0md" ==
        "a {$yellow}...{[intense]}...{$bold}b{[green]}c{/$}{/$}d", 27, true
    );
}

#[test]
fn questionmark_in_argument() {
    fn foo(s: &str) -> Result<bool, std::num::ParseIntError> {
        let mut b = buf();
        let _  = bunt::writeln!(b, "Hello {[green]}", s.parse::<u64>()?);
        Ok(true)
    }

    assert!(foo("hi").is_err());
    assert_eq!(foo("3"), Ok(true));
}

#[test]
fn io_error() {
    let mut empty = [];
    let res = bunt::write!(&mut empty[..], "hello");
    assert!(res.is_err());
}

#[test]
fn set_color_error() {
    use std::io;

    struct Dummy;

    impl io::Write for Dummy {
        fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
            Ok(buf.len())
        }
        fn flush(&mut self) -> io::Result<()> {
            Ok(())
        }
    }

    impl bunt::termcolor::WriteColor for Dummy {
        fn supports_color(&self) -> bool {
            true
        }
        fn set_color(&mut self, _: &bunt::termcolor::ColorSpec) -> io::Result<()> {
            Err(io::Error::new(io::ErrorKind::NotFound, "oops"))
        }
        fn reset(&mut self) -> io::Result<()> {
            Ok(())
        }
    }

    let mut b = Dummy;
    assert!(bunt::write!(b, "hello").is_ok());

    let res = bunt::write!(b, "{$green}colooor{/$}");
    assert!(res.is_err());
    assert_eq!(res.unwrap_err().kind(), io::ErrorKind::NotFound);
}

#[test]
fn concat_fmt_strings() {
    check!("hello" == ["hello"]);
    check!("hello" == ["hello",]);
    check!("foobar" == ["foo", "bar"]);
    check!("foobar" == ["foo", "bar",]);
    check!("foo 27 bar" == ["foo ", "{} bar"], 27);
    check!("abtruecd" == ["a", "b", "{}", "c", "d"], true);

    check!(
        "a\nb\tc\rd\0e\x48f\u{50}g\u{228}h\u{fffe}i\u{1F923}j" ==
        ["a\n", "b\tc\r", "d\0e\x48f\u{50}", "g\u{228}h\u{fffe}i", "\u{1F923}j"]
    );
}

#[test]
fn concat_fmt_strings_all_strings() {
    let mut b = buf();
    let _ = write!(b, ["a", "{} b"], 27);
    let _ = writeln!(b, ["a", "{} b"], 27);
    print!(["a", "{} \r"], 27);
    eprint!(["a", "{} \r"], 27);
    println!(["", "{}"], "");
    eprintln!(["", "{}"], "");
}

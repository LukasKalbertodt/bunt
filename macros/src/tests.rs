use crate::ir::{Align, Width, Sign, FormatSpec, Precision};


#[track_caller]
fn check_format_spec(expected_str: &str, expected_parsed: FormatSpec) {
    let actual_parsed = FormatSpec::parse(expected_str)
        .expect(&format!("failed to parse format spec '{}'", expected_str));

    if actual_parsed != expected_parsed {
        panic!(
            "result of parsing format spec '{}' unexpected.\
                \n  expected: {:?}\
                \n    actual: {:?}\n",
            expected_str,
            expected_parsed,
            actual_parsed,
        );
    }

    let actual_string = actual_parsed.to_string();
    if actual_string != expected_str {
        panic!(
            "result of stringifying '{:?}' unexpected.\
                \n  expected: {}\
                \n    actual: {}\n",
            actual_parsed,
            expected_str,
            actual_string,
        );
    }
}

#[test]
fn format_spec_simple() {
    check_format_spec("", FormatSpec::default());
    check_format_spec("x", FormatSpec {
        ty: Some('x'),
        .. FormatSpec::default()
    });
    check_format_spec("<", FormatSpec {
        align: Some(Align::Left),
        .. FormatSpec::default()
    });
    check_format_spec("x^", FormatSpec {
        fill: Some('x'),
        align: Some(Align::Center),
        .. FormatSpec::default()
    });
    check_format_spec("+", FormatSpec {
        sign: Some(Sign::Plus),
        .. FormatSpec::default()
    });
    check_format_spec("#", FormatSpec {
        alternate: true,
        .. FormatSpec::default()
    });
    check_format_spec("0", FormatSpec {
        zero: true,
        .. FormatSpec::default()
    });
}

#[test]
fn format_spec_width() {
    check_format_spec("5", FormatSpec {
        width: Some(Width::Constant(5)),
        .. FormatSpec::default()
    });
    check_format_spec("0$", FormatSpec {
        width: Some(Width::Position(0)),
        .. FormatSpec::default()
    });
    check_format_spec("2$", FormatSpec {
        width: Some(Width::Position(2)),
        .. FormatSpec::default()
    });
    check_format_spec("peter$", FormatSpec {
        width: Some(Width::Name("peter".into())),
        .. FormatSpec::default()
    });
}

#[test]
fn format_spec_precision() {
    check_format_spec(".5", FormatSpec {
        precision: Some(Precision::Constant(5)),
        .. FormatSpec::default()
    });
    check_format_spec(".2$", FormatSpec {
        precision: Some(Precision::Position(2)),
        .. FormatSpec::default()
    });
    check_format_spec(".peter$", FormatSpec {
        precision: Some(Precision::Name("peter".into())),
        .. FormatSpec::default()
    });
    check_format_spec(".*", FormatSpec {
        precision: Some(Precision::Bundled),
        .. FormatSpec::default()
    });
}

#[test]
fn format_spec_mixed() {
    check_format_spec("1.0", FormatSpec {
        width: Some(Width::Constant(1)),
        precision: Some(Precision::Constant(0)),
        .. FormatSpec::default()
    });
    check_format_spec("1$.peter$", FormatSpec {
        width: Some(Width::Position(1)),
        precision: Some(Precision::Name("peter".into())),
        .. FormatSpec::default()
    });
    check_format_spec("04", FormatSpec {
        zero: true,
        width: Some(Width::Constant(4)),
        .. FormatSpec::default()
    });
    check_format_spec("-<5", FormatSpec {
        fill: Some('-'),
        align: Some(Align::Left),
        width: Some(Width::Constant(5)),
        .. FormatSpec::default()
    });
    check_format_spec("00", FormatSpec {
        zero: true,
        width: Some(Width::Constant(0)),
        .. FormatSpec::default()
    });
    check_format_spec("#010x", FormatSpec {
        alternate: true,
        zero: true,
        width: Some(Width::Constant(10)),
        ty: Some('x'),
        .. FormatSpec::default()
    });

    check_format_spec("_>+#0x$.*?", FormatSpec {
        fill: Some('_'),
        align: Some(Align::Right),
        sign: Some(Sign::Plus),
        alternate: true,
        zero: true,
        width: Some(Width::Name("x".into())),
        precision: Some(Precision::Bundled),
        ty: Some('?'),
    });
}

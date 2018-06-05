use mat;
use parse;
use prelude::*;

#[test]
fn minimal() {
    let g = &[
        ("start", s(t("a"))),
    ];

    assert_eq!(
        parse(g, "start", "aaa").unwrap(),
        mat((0, 1, 1, 3, 1, 4), vec![])
    );
}

#[test]
fn optional() {
    let g = &[
        ("start", e(vec![
            q(t("-")),
            q(t("+")),
        ])),
    ];

    parse(g, "start", "-+").unwrap();
    parse(g, "start", "-").unwrap();
    parse(g, "start", "+").unwrap();
    parse(g, "start", "").unwrap();

    parse(g, "start", "+-").unwrap_err();
    parse(g, "start", " ").unwrap_err();
    parse(g, "start", "+0").unwrap_err();
    parse(g, "start", "0+").unwrap_err();
}

#[test]
fn decimal_integer() {
    let g = &[
        ("dec_nonzero_digit", c(vec![
            t("1"),
            t("2"),
            t("3"),
            t("4"),
            t("5"),
            t("6"),
            t("7"),
            t("8"),
            t("9"),
        ])),
        ("dec_digit", c(vec![
            t("0"),
            k("dec_nonzero_digit"),
        ])),
        ("dec_int", e(vec![
            q(t("-")),
            c(vec![
                t("0"),
                e(vec![
                    k("dec_nonzero_digit"),
                    s(k("dec_digit")),
                ]),
            ]),
        ])),
    ];

    parse(g, "dec_int", "0").unwrap();
    parse(g, "dec_int", "1").unwrap();
    parse(g, "dec_int", "9").unwrap();
    parse(g, "dec_int", "10").unwrap();
    parse(g, "dec_int", "19").unwrap();
    parse(g, "dec_int", "99").unwrap();
    parse(g, "dec_int", "-0").unwrap();
    parse(g, "dec_int", "-1").unwrap();
    parse(g, "dec_int", "-9").unwrap();
    parse(g, "dec_int", "-10").unwrap();
    parse(g, "dec_int", "-19").unwrap();
    parse(g, "dec_int", "-99").unwrap();

    parse(g, "dec_int", "y").unwrap_err();
    parse(g, "dec_int", "-").unwrap_err();
    parse(g, "dec_int", "0-").unwrap_err();
    parse(g, "dec_int", "1-").unwrap_err();
    parse(g, "dec_int", "01").unwrap_err();
}

#[test]
fn simple_expr() {
    let g = &[
        ("expr", e(vec![
            k("expr2"),
            s(e(vec![
                t("+"),
                k("expr2"),
            ])),
        ])),
        ("expr2", c(vec![
            t("1"),
            e(vec![
                t("("),
                k("expr"),
                t(")"),
            ]),
        ])),
    ];

    parse(g, "expr", "1").unwrap();
    parse(g, "expr", "1+1").unwrap();
    parse(g, "expr", "(1+1+1)+1").unwrap();
    parse(g, "expr", "1+(1+1+1)").unwrap();
}

#[test]
fn range() {
    let g = &[
        ("start", e(vec![
            r('a', 'd'),
            t("e"),
        ])),
    ];

    parse(g, "start", "ae").unwrap();
    parse(g, "start", "be").unwrap();
    parse(g, "start", "ce").unwrap();
    parse(g, "start", "de").unwrap();

    parse(g, "start", "a").unwrap_err();
    parse(g, "start", "e").unwrap_err();
    parse(g, "start", "ee").unwrap_err();
    parse(g, "start", "ea").unwrap_err();
}

#[test]
fn lookahead() {
    let g = &[
        ("start", e(vec![
            z(t("a")),
            r('a', 'c'),
            g(t("a")),
            r('a', 'c'),
        ])),
    ];

    parse(g, "start", "ab").unwrap();
    parse(g, "start", "ac").unwrap();

    parse(g, "start", "aa").unwrap_err();
    parse(g, "start", "ba").unwrap_err();
    parse(g, "start", "bc").unwrap_err();
    parse(g, "start", ".c").unwrap_err();
    parse(g, "start", "a").unwrap_err();
    parse(g, "start", "b").unwrap_err();
    parse(g, "start", "").unwrap_err();
}

#[test]
fn anything() {
    let g = &[
        ("x", e(vec![
            a(),
            a(),
        ])),
    ];

    assert_eq!(
        parse(g, "x", "ab").unwrap(),
        mat((0, 1, 1, 2, 1, 3), vec![])
    );
    assert_eq!(
        parse(g, "x", " \n").unwrap(),
        mat((0, 1, 1, 2, 2, 1), vec![])
    );

    parse(g, "x", "").unwrap_err();
    parse(g, "x", "a").unwrap_err();
    parse(g, "x", "abc").unwrap_err();
}

use constructors::*;
use parse::Parser;
use tests::mat;

#[test]
fn minimal() {
    let g = &[
        ("start", s(t("a"))),
    ];

    assert_eq!(
        Parser::parse(g, "start", "aaa").unwrap(),
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

    Parser::parse(g, "start", "-+").unwrap();
    Parser::parse(g, "start", "-").unwrap();
    Parser::parse(g, "start", "+").unwrap();
    Parser::parse(g, "start", "").unwrap();

    Parser::parse(g, "start", "+-").unwrap_err();
    Parser::parse(g, "start", " ").unwrap_err();
    Parser::parse(g, "start", "+0").unwrap_err();
    Parser::parse(g, "start", "0+").unwrap_err();
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

    Parser::parse(g, "dec_int", "0").unwrap();
    Parser::parse(g, "dec_int", "1").unwrap();
    Parser::parse(g, "dec_int", "9").unwrap();
    Parser::parse(g, "dec_int", "10").unwrap();
    Parser::parse(g, "dec_int", "19").unwrap();
    Parser::parse(g, "dec_int", "99").unwrap();
    Parser::parse(g, "dec_int", "-0").unwrap();
    Parser::parse(g, "dec_int", "-1").unwrap();
    Parser::parse(g, "dec_int", "-9").unwrap();
    Parser::parse(g, "dec_int", "-10").unwrap();
    Parser::parse(g, "dec_int", "-19").unwrap();
    Parser::parse(g, "dec_int", "-99").unwrap();

    Parser::parse(g, "dec_int", "y").unwrap_err();
    Parser::parse(g, "dec_int", "-").unwrap_err();
    Parser::parse(g, "dec_int", "0-").unwrap_err();
    Parser::parse(g, "dec_int", "1-").unwrap_err();
    Parser::parse(g, "dec_int", "01").unwrap_err();
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

    Parser::parse(g, "expr", "1").unwrap();
    Parser::parse(g, "expr", "1+1").unwrap();
    Parser::parse(g, "expr", "(1+1+1)+1").unwrap();
    Parser::parse(g, "expr", "1+(1+1+1)").unwrap();
}

#[test]
fn range() {
    let g = &[
        ("start", e(vec![
            r('a', 'd'),
            t("e"),
        ])),
    ];

    Parser::parse(g, "start", "ae").unwrap();
    Parser::parse(g, "start", "be").unwrap();
    Parser::parse(g, "start", "ce").unwrap();
    Parser::parse(g, "start", "de").unwrap();

    Parser::parse(g, "start", "a").unwrap_err();
    Parser::parse(g, "start", "e").unwrap_err();
    Parser::parse(g, "start", "ee").unwrap_err();
    Parser::parse(g, "start", "ea").unwrap_err();
}

#[test]
fn lookahead() {
    let g = &[
        ("start", e(vec![
            z(t("a")),
            r('a', 'c'),
            n(t("a")),
            r('a', 'c'),
        ])),
    ];

    Parser::parse(g, "start", "ab").unwrap();
    Parser::parse(g, "start", "ac").unwrap();

    Parser::parse(g, "start", "aa").unwrap_err();
    Parser::parse(g, "start", "ba").unwrap_err();
    Parser::parse(g, "start", "bc").unwrap_err();
    Parser::parse(g, "start", ".c").unwrap_err();
    Parser::parse(g, "start", "a").unwrap_err();
    Parser::parse(g, "start", "b").unwrap_err();
    Parser::parse(g, "start", "").unwrap_err();
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
        Parser::parse(g, "x", "ab").unwrap(),
        mat((0, 1, 1, 2, 1, 3), vec![])
    );
    assert_eq!(
        Parser::parse(g, "x", " \n").unwrap(),
        mat((0, 1, 1, 2, 2, 1), vec![])
    );

    Parser::parse(g, "x", "").unwrap_err();
    Parser::parse(g, "x", "a").unwrap_err();
    Parser::parse(g, "x", "abc").unwrap_err();
}

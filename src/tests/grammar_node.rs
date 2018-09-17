use constructors::*;
use parse::Parser;
use string_table::StringTable;
use tests::mat;

#[test]
fn minimal() {
    let mut tab = StringTable::new();
    let g = &[
        ("start", s(t(&tab, "a"))),
    ];

    assert_eq!(
        Parser::parse(g, &mat, "start", "aaa").unwrap(),
        mat((0, 1, 1, 3, 1, 4), vec![])
    );
}

#[test]
fn optional() {
    let mut tab = StringTable::new();
    let g = &[
        ("start", e(vec![
            q(t(&tab, "-")),
            q(t(&tab, "+")),
        ])),
    ];

    Parser::parse(g, &tab, "start", "-+").unwrap();
    Parser::parse(g, &tab, "start", "-").unwrap();
    Parser::parse(g, &tab, "start", "+").unwrap();
    Parser::parse(g, &tab, "start", "").unwrap();

    Parser::parse(g, &tab, "start", "+-").unwrap_err();
    Parser::parse(g, &tab, "start", " ").unwrap_err();
    Parser::parse(g, &tab, "start", "+0").unwrap_err();
    Parser::parse(g, &tab, "start", "0+").unwrap_err();
}

#[test]
fn decimal_integer() {
    let mut tab = StringTable::new();
    let g = &[
        ("dec_nonzero_digit", c(vec![
            t(&tab, "1"),
            t(&tab, "2"),
            t(&tab, "3"),
            t(&tab, "4"),
            t(&tab, "5"),
            t(&tab, "6"),
            t(&tab, "7"),
            t(&tab, "8"),
            t(&tab, "9"),
        ])),
        ("dec_digit", c(vec![
            t(&tab, "0"),
            k("dec_nonzero_digit"),
        ])),
        ("dec_int", e(vec![
            q(t(&tab, "-")),
            c(vec![
                t(&tab, "0"),
                e(vec![
                    k("dec_nonzero_digit"),
                    s(k("dec_digit")),
                ]),
            ]),
        ])),
    ];

    Parser::parse(g, &tab, "dec_int", "0").unwrap();
    Parser::parse(g, &tab, "dec_int", "1").unwrap();
    Parser::parse(g, &tab, "dec_int", "9").unwrap();
    Parser::parse(g, &tab, "dec_int", "10").unwrap();
    Parser::parse(g, &tab, "dec_int", "19").unwrap();
    Parser::parse(g, &tab, "dec_int", "99").unwrap();
    Parser::parse(g, &tab, "dec_int", "-0").unwrap();
    Parser::parse(g, &tab, "dec_int", "-1").unwrap();
    Parser::parse(g, &tab, "dec_int", "-9").unwrap();
    Parser::parse(g, &tab, "dec_int", "-10").unwrap();
    Parser::parse(g, &tab, "dec_int", "-19").unwrap();
    Parser::parse(g, &tab, "dec_int", "-99").unwrap();

    Parser::parse(g, &tab, "dec_int", "y").unwrap_err();
    Parser::parse(g, &tab, "dec_int", "-").unwrap_err();
    Parser::parse(g, &tab, "dec_int", "0-").unwrap_err();
    Parser::parse(g, &tab, "dec_int", "1-").unwrap_err();
    Parser::parse(g, &tab, "dec_int", "01").unwrap_err();
}

#[test]
fn simple_expr() {
    let mut tab = StringTable::new();
    let g = &[
        ("expr", e(vec![
            k("expr2"),
            s(e(vec![
                t(&tab, "+"),
                k("expr2"),
            ])),
        ])),
        ("expr2", c(vec![
            t(&tab, "1"),
            e(vec![
                t(&tab, "("),
                k("expr"),
                t(&tab, ")"),
            ]),
        ])),
    ];

    Parser::parse(g, &tab, "expr", "1").unwrap();
    Parser::parse(g, &tab, "expr", "1+1").unwrap();
    Parser::parse(g, &tab, "expr", "(1+1+1)+1").unwrap();
    Parser::parse(g, &tab, "expr", "1+(1+1+1)").unwrap();
}

#[test]
fn range() {
    let mut tab = StringTable::new();
    let g = &[
        ("start", e(vec![
            r('a', 'd'),
            t(&tab, "e"),
        ])),
    ];

    Parser::parse(g, &tab, "start", "ae").unwrap();
    Parser::parse(g, &tab, "start", "be").unwrap();
    Parser::parse(g, &tab, "start", "ce").unwrap();
    Parser::parse(g, &tab, "start", "de").unwrap();

    Parser::parse(g, &tab, "start", "a").unwrap_err();
    Parser::parse(g, &tab, "start", "e").unwrap_err();
    Parser::parse(g, &tab, "start", "ee").unwrap_err();
    Parser::parse(g, &tab, "start", "ea").unwrap_err();
}

#[test]
fn lookahead() {
    let mut tab = StringTable::new();
    let g = &[
        ("start", e(vec![
            z(t(&tab, "a")),
            r('a', 'c'),
            n(t(&tab, "a")),
            r('a', 'c'),
        ])),
    ];

    Parser::parse(g, &tab, "start", "ab").unwrap();
    Parser::parse(g, &tab, "start", "ac").unwrap();

    Parser::parse(g, &tab, "start", "aa").unwrap_err();
    Parser::parse(g, &tab, "start", "ba").unwrap_err();
    Parser::parse(g, &tab, "start", "bc").unwrap_err();
    Parser::parse(g, &tab, "start", ".c").unwrap_err();
    Parser::parse(g, &tab, "start", "a").unwrap_err();
    Parser::parse(g, &tab, "start", "b").unwrap_err();
    Parser::parse(g, &tab, "start", "").unwrap_err();
}

#[test]
fn anything() {
    let tab = StringTable::new();
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

    Parser::parse(g, &tab, "x", "").unwrap_err();
    Parser::parse(g, &tab, "x", "a").unwrap_err();
    Parser::parse(g, &tab, "x", "abc").unwrap_err();
}

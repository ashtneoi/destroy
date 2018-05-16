mod tree;

use prelude::*;

#[test]
fn minimal() {
    let mut g = e(vec![
        n("start", s(t("a"))),
    ]);

    let st = g.parse("start", "aaa").unwrap();
    assert_eq!(st.name, None);
    assert_eq!(st.raw, (0, 3));
    assert!(st.children.is_empty());
}

#[test]
fn optional() {
    let mut g = e(vec![
        n("start", e(vec![
            q(t("-")),
            q(t("+")),
        ])),
    ]);

    g.parse("start", "-+").unwrap();
    g.parse("start", "-").unwrap();
    g.parse("start", "+").unwrap();
    g.parse("start", "").unwrap();

    g.parse("start", "+-").unwrap_err();
    g.parse("start", " ").unwrap_err();
    g.parse("start", "+0").unwrap_err();
    g.parse("start", "0+").unwrap_err();
}

#[test]
fn weirdness() {
    let mut g = e(vec![
        n("start", e(vec![
            t("a"),
            n("bee", t("b")),
        ])),
    ]);

    g.parse("start", "ab").unwrap();
}

#[test]
fn decimal_integer() {
    let mut g = e(vec![
        n("dec_nonzero_digit", c(vec![
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
        n("dec_digit", c(vec![
            t("0"),
            k("dec_nonzero_digit"),
        ])),
        n("dec_int", e(vec![
            q(t("-")),
            c(vec![
                t("0"),
                e(vec![
                    k("dec_nonzero_digit"),
                    s(k("dec_digit")),
                ]),
            ]),
        ])),
    ]);

    g.parse("dec_int", "0").unwrap();
    g.parse("dec_int", "1").unwrap();
    g.parse("dec_int", "9").unwrap();
    g.parse("dec_int", "10").unwrap();
    g.parse("dec_int", "19").unwrap();
    g.parse("dec_int", "99").unwrap();
    g.parse("dec_int", "-0").unwrap();
    g.parse("dec_int", "-1").unwrap();
    g.parse("dec_int", "-9").unwrap();
    g.parse("dec_int", "-10").unwrap();
    g.parse("dec_int", "-19").unwrap();
    g.parse("dec_int", "-99").unwrap();

    g.parse("dec_int", "y").unwrap_err();
    g.parse("dec_int", "-").unwrap_err();
    g.parse("dec_int", "0-").unwrap_err();
    g.parse("dec_int", "1-").unwrap_err();
    g.parse("dec_int", "01").unwrap_err();
}

#[test]
fn simple_expr() {
    let mut g = e(vec![
        n("expr", e(vec![
            k("expr2"),
            s(e(vec![
                t("+"),
                k("expr2"),
            ])),
        ])),
        n("expr2", c(vec![
            t("1"),
            e(vec![
                t("("),
                k("expr"),
                t(")"),
            ]),
        ])),
    ]);

    g.parse("expr", "1").unwrap();
    g.parse("expr", "1+1").unwrap();
    g.parse("expr", "(1+1+1)+1").unwrap();
    g.parse("expr", "1+(1+1+1)").unwrap();
}

#[test]
fn range() {
    let mut g = n("start", e(vec![
        r('a', 'd'),
        t("e"),
    ]));

    g.parse("start", "ae").unwrap();
    g.parse("start", "be").unwrap();
    g.parse("start", "ce").unwrap();
    g.parse("start", "de").unwrap();

    g.parse("start", "a").unwrap_err();
    g.parse("start", "e").unwrap_err();
    g.parse("start", "ee").unwrap_err();
    g.parse("start", "ea").unwrap_err();
}

#[test]
fn lookahead() {
    let mut g = n("start", e(vec![
        z(t("a")),
        r('a', 'c'),
        g(t("a")),
        r('a', 'c'),
    ]));

    g.parse("start", "ab").unwrap();
    g.parse("start", "ac").unwrap();

    g.parse("start", "aa").unwrap_err();
    g.parse("start", "ba").unwrap_err();
    g.parse("start", "bc").unwrap_err();
    g.parse("start", ".c").unwrap_err();
    g.parse("start", "a").unwrap_err();
    g.parse("start", "b").unwrap_err();
    g.parse("start", "").unwrap_err();
}

#[test]
fn ident() {
    let mut g = get_grammar_grammar();

    g.parse("ident", "a").unwrap();
    g.parse("ident", "A").unwrap();
    g.parse("ident", "_").unwrap();
    g.parse("ident", "foo").unwrap();
    g.parse("ident", "foo_bar").unwrap();
    g.parse("ident", "_foo_bar_").unwrap();
    g.parse("ident", "a3").unwrap();
    g.parse("ident", "_3").unwrap();

    g.parse("ident", "3").unwrap_err();
    g.parse("ident", "3a").unwrap_err();
    g.parse("ident", "3_").unwrap_err();
}
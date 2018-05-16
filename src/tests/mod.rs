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

    // TODO: unwrap(), not assert!(...is_ok())
    assert!(g.parse("start", "-+").is_ok());
    assert!(g.parse("start", "-").is_ok());
    assert!(g.parse("start", "+").is_ok());
    assert!(g.parse("start", "").is_ok());
    // TODO: likewise
    assert!(g.parse("start", "+-").is_err());
    assert!(g.parse("start", " ").is_err());
    assert!(g.parse("start", "+0").is_err());
    assert!(g.parse("start", "0+").is_err());
}

#[test]
fn weirdness() {
    let mut g = e(vec![
        n("start", e(vec![
            t("a"),
            n("bee", t("b")),
        ])),
    ]);

    assert!(g.parse("start", "ab").is_ok());
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

    assert!(g.parse("dec_int", "0").is_ok());
    assert!(g.parse("dec_int", "1").is_ok());
    assert!(g.parse("dec_int", "9").is_ok());
    assert!(g.parse("dec_int", "10").is_ok());
    assert!(g.parse("dec_int", "19").is_ok());
    assert!(g.parse("dec_int", "99").is_ok());
    assert!(g.parse("dec_int", "-0").is_ok());
    assert!(g.parse("dec_int", "-1").is_ok());
    assert!(g.parse("dec_int", "-9").is_ok());
    assert!(g.parse("dec_int", "-10").is_ok());
    assert!(g.parse("dec_int", "-19").is_ok());
    assert!(g.parse("dec_int", "-99").is_ok());

    assert!(g.parse("dec_int", "y").is_err());
    assert!(g.parse("dec_int", "-").is_err());
    assert!(g.parse("dec_int", "0-").is_err());
    assert!(g.parse("dec_int", "1-").is_err());
    assert!(g.parse("dec_int", "01").is_err());
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

    assert!(g.parse("expr", "1").is_ok());
    assert!(g.parse("expr", "1+1").is_ok());
    assert!(g.parse("expr", "(1+1+1)+1").is_ok());
    assert!(g.parse("expr", "1+(1+1+1)").is_ok());
}

#[test]
fn range() {
    let mut g = n("start", e(vec![
        r('a', 'd'),
        t("e"),
    ]));

    assert!(g.parse("start", "ae").is_ok());
    assert!(g.parse("start", "be").is_ok());
    assert!(g.parse("start", "ce").is_ok());
    assert!(g.parse("start", "de").is_ok());

    assert!(g.parse("start", "a").is_err());
    assert!(g.parse("start", "e").is_err());
    assert!(g.parse("start", "ee").is_err());
    assert!(g.parse("start", "ea").is_err());
}

#[test]
fn lookahead() {
    let mut g = n("start", e(vec![
        z(t("a")),
        r('a', 'c'),
        g(t("a")),
        r('a', 'c'),
    ]));

    assert!(g.parse("start", "ab").is_ok());
    assert!(g.parse("start", "ac").is_ok());

    assert!(g.parse("start", "aa").is_err());
    assert!(g.parse("start", "ba").is_err());
    assert!(g.parse("start", "bc").is_err());
    assert!(g.parse("start", ".c").is_err());
    assert!(g.parse("start", "a").is_err());
    assert!(g.parse("start", "b").is_err());
    assert!(g.parse("start", "").is_err());
}

#[test]
fn ident() {
    let mut g = get_grammar_grammar();

    assert!(g.parse("ident", "a").is_ok());
    assert!(g.parse("ident", "A").is_ok());
    assert!(g.parse("ident", "_").is_ok());
    assert!(g.parse("ident", "foo").is_ok());
    assert!(g.parse("ident", "foo_bar").is_ok());
    assert!(g.parse("ident", "_foo_bar_").is_ok());
    assert!(g.parse("ident", "a3").is_ok());
    assert!(g.parse("ident", "_3").is_ok());

    assert!(g.parse("ident", "3").is_err());
    assert!(g.parse("ident", "3a").is_err());
    assert!(g.parse("ident", "3_").is_err());
}

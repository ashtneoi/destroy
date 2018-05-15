use prelude::*;

#[test]
fn minimal() {
    let mut g = e(vec![
        n("start", s(t("a"))),
    ]);

    let st = g.parse("start", "aaa").unwrap();
    assert_eq!(st.name.as_ref().unwrap(), "start");
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

    assert!(g.parse("start", "-+").is_ok());
    assert!(g.parse("start", "-").is_ok());
    assert!(g.parse("start", "+").is_ok());
    assert!(g.parse("start", "").is_ok());
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

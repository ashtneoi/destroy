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
        n("dec_int", c(vec![
            t("0"),
            e(vec![
                k("dec_nonzero_digit"),
                s(k("dec_digit")),
            ]),
        ])),
    ]);

    g.parse("dec_int", "0").unwrap();
    g.parse("dec_int", "1").unwrap();
    g.parse("dec_int", "9").unwrap();
    g.parse("dec_int", "10").unwrap();
    g.parse("dec_int", "19").unwrap();
    g.parse("dec_int", "99").unwrap();
}

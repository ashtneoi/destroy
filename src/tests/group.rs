use parse::parse;
use prelude::*;
use tests::mat;

#[test]
fn e_group_two_names() {
    let g = &[
        ("x", e(vec![
            u("A", q(t("a"))),
            u("B", q(t("b"))),
        ])),
    ];

    assert_eq!(
        parse(g, "x", "ab").unwrap(),
        mat((0, 1, 1, 2, 1, 3), vec![
            ("A", vec![mat((0, 1, 1, 1, 1, 2), vec![])]),
            ("B", vec![mat((1, 1, 2, 2, 1, 3), vec![])]),
        ])
    );
}

#[test]
fn e_group_one_name() {
    let g1 = &[
        ("x", e(vec![
            q(t("a")),
            u("B", t("b")),
        ])),
    ];

    assert_eq!(
        parse(g1, "x", "b").unwrap(),
        mat((0, 1, 1, 1, 1, 2), vec![
            ("B", vec![mat((0, 1, 1, 1, 1, 2), vec![])]),
        ])
    );
    assert_eq!(
        parse(g1, "x", "ab").unwrap(),
        mat((0, 1, 1, 2, 1, 3), vec![
            ("B", vec![mat((1, 1, 2, 2, 1, 3), vec![])]),
        ])
    );

    let g2 = &[
        ("x", e(vec![
            u("A", t("a")),
            q(t("b")),
        ])),
    ];

    assert_eq!(
        parse(g2, "x", "a").unwrap(),
        mat((0, 1, 1, 1, 1, 2), vec![
            ("A", vec![mat((0, 1, 1, 1, 1, 2), vec![])]),
        ])
    );
    assert_eq!(
        parse(g2, "x", "ab").unwrap(),
        mat((0, 1, 1, 2, 1, 3), vec![
            ("A", vec![mat((0, 1, 1, 1, 1, 2), vec![])]),
        ])
    );
}

#[test]
fn e_group_same_name() {
    let g = &[
        ("x", e(vec![
            u("A", t("a")),
            u("A", t("b")),
        ])),
    ];

    assert_eq!(
        parse(g, "x", "ab").unwrap(),
        mat((0, 1, 1, 2, 1, 3), vec![
            ("A", vec![
                mat((0, 1, 1, 1, 1, 2), vec![]),
                mat((1, 1, 2, 2, 1, 3), vec![]),
            ]),
        ])
    );
}

#[test]
fn e_group_no_names() {
    let g = &[
        ("x", e(vec![
            q(t("a")),
            q(t("b")),
        ])),
    ];

    assert_eq!(
        parse(g, "x", "").unwrap(),
        mat((0, 1, 1, 0, 1, 1), vec![])
    );
    assert_eq!(
        parse(g, "x", "a").unwrap(),
        mat((0, 1, 1, 1, 1, 2), vec![])
    );
    assert_eq!(
        parse(g, "x", "b").unwrap(),
        mat((0, 1, 1, 1, 1, 2), vec![])
    );
    assert_eq!(
        parse(g, "x", "ab").unwrap(),
        mat((0, 1, 1, 2, 1, 3), vec![])
    );
}

#[test]
fn c_group() {
    let g = &[
        ("x", c(vec![
            u("A", t("a")),
            u("B", t("b")),
        ])),
    ];

    assert_eq!(
        parse(g, "x", "a").unwrap(),
        mat((0, 1, 1, 1, 1, 2), vec![
            ("A", vec![mat((0, 1, 1, 1, 1, 2), vec![])]),
        ])
    );
    assert_eq!(
        parse(g, "x", "b").unwrap(),
        mat((0, 1, 1, 1, 1, 2), vec![
            ("B", vec![mat((0, 1, 1, 1, 1, 2), vec![])]),
        ])
    );
}

#[test]
fn s_group() {
    let g = &[
        ("x", s(
            u("A", u("E", t("a"))),
        )),
    ];

    assert_eq!(
        parse(g, "x", "").unwrap(),
        mat((0, 1, 1, 0, 1, 1), vec![])
    );
    assert_eq!(
        parse(g, "x", "aa").unwrap(),
        mat((0, 1, 1, 2, 1, 3), vec![
            ("A", vec![
                mat((0, 1, 1, 1, 1, 2), vec![
                    ("E", vec![mat((0, 1, 1, 1, 1, 2), vec![])]),
                ]),
                mat((1, 1, 2, 2, 1, 3), vec![
                    ("E", vec![mat((1, 1, 2, 2, 1, 3), vec![])]),
                ]),
            ]),
        ])
    );
}

#[test]
fn p_group() {
    let g = &[
        ("x", p(
            u("A", u("E", t("a"))),
        )),
    ];

    assert_eq!(
        parse(g, "x", "aa").unwrap(),
        mat((0, 1, 1, 2, 1, 3), vec![
            ("A", vec![
                mat((0, 1, 1, 1, 1, 2), vec![
                    ("E", vec![mat((0, 1, 1, 1, 1, 2), vec![])]),
                ]),
                mat((1, 1, 2, 2, 1, 3), vec![
                    ("E", vec![mat((1, 1, 2, 2, 1, 3), vec![])]),
                ]),
            ]),
        ])
    );
}

#[test]
fn q_group() {
    let g = &[
        ("x", q(
            u("A", t("a"))
        )),
    ];

    assert_eq!(
        parse(g, "x", "").unwrap(),
        mat((0, 1, 1, 0, 1, 1), vec![])
    );
    assert_eq!(
        parse(g, "x", "a").unwrap(),
        mat((0, 1, 1, 1, 1, 2), vec![
            ("A", vec![mat((0, 1, 1, 1, 1, 2), vec![])]),
        ])
    );
}

#[test]
fn z_group() {
    let g = &[
        ("x", e(vec![
            z(u("E", t("a"))),
            u("A", t("a")),
        ])),
    ];

    assert_eq!(
        parse(g, "x", "a").unwrap(),
        mat((0, 1, 1, 1, 1, 2), vec![
            ("A", vec![mat((0, 1, 1, 1, 1, 2), vec![])]),
        ])
    );
}

#[test]
fn g_group() {
    let g = &[
        ("x", e(vec![
            g(u("E", t("e"))),
            u("A", t("a")),
        ])),
    ];

    assert_eq!(
        parse(g, "x", "a").unwrap(),
        mat((0, 1, 1, 1, 1, 2), vec![
            ("A", vec![mat((0, 1, 1, 1, 1, 2), vec![])]),
        ])
    );
}

#[test]
fn x_group() {
    let g = &[
        ("x",
            x(u("A", t("a"))),
        ),
    ];

    assert_eq!(
        parse(g, "x", "a").unwrap(),
        mat((0, 1, 1, 1, 1, 2), vec![])
    );
}

#[test]
fn k_group() {
    let g = &[
        ("x", k("w")),
        ("w", u("A", t("a"))),
    ];

    assert_eq!(
        parse(g, "x", "a").unwrap(),
        mat((0, 1, 1, 1, 1, 2), vec![
            ("A", vec![mat((0, 1, 1, 1, 1, 2), vec![])]),
        ])
    );
}

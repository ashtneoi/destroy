use constructors::*;
use parse::Parser;
use string_table::StringTable;
use tests::mat;

#[test]
fn e_group_two_names() {
    let mut tab = StringTable::new();
    let g = &[
        ("x", e(vec![
            u("A", q(t(&mut tab, "a"))),
            u("B", q(t(&mut tab, "b"))),
        ])),
    ];

    assert_eq!(
        Parser::parse(g, "x", "ab").unwrap(),
        mat((0, 1, 1, 2, 1, 3), vec![
            ("A", vec![mat((0, 1, 1, 1, 1, 2), vec![])]),
            ("B", vec![mat((1, 1, 2, 2, 1, 3), vec![])]),
        ])
    );
}

#[test]
fn e_group_one_name() {
    let mut tab = StringTable::new();

    let g1 = &[
        ("x", e(vec![
            q(t(&mut tab, "a")),
            u("B", t(&mut tab, "b")),
        ])),
    ];

    assert_eq!(
        Parser::parse(g1, "x", "b").unwrap(),
        mat((0, 1, 1, 1, 1, 2), vec![
            ("B", vec![mat((0, 1, 1, 1, 1, 2), vec![])]),
        ])
    );
    assert_eq!(
        Parser::parse(g1, "x", "ab").unwrap(),
        mat((0, 1, 1, 2, 1, 3), vec![
            ("B", vec![mat((1, 1, 2, 2, 1, 3), vec![])]),
        ])
    );

    let g2 = &[
        ("x", e(vec![
            u("A", t(&mut tab, "a")),
            q(t(&mut tab, "b")),
        ])),
    ];

    assert_eq!(
        Parser::parse(g2, "x", "a").unwrap(),
        mat((0, 1, 1, 1, 1, 2), vec![
            ("A", vec![mat((0, 1, 1, 1, 1, 2), vec![])]),
        ])
    );
    assert_eq!(
        Parser::parse(g2, "x", "ab").unwrap(),
        mat((0, 1, 1, 2, 1, 3), vec![
            ("A", vec![mat((0, 1, 1, 1, 1, 2), vec![])]),
        ])
    );
}

#[test]
fn e_group_same_name() {
    let mut tab = StringTable::new();
    let g = &[
        ("x", e(vec![
            u("A", t(&mut tab, "a")),
            u("A", t(&mut tab, "b")),
        ])),
    ];

    assert_eq!(
        Parser::parse(g, "x", "ab").unwrap(),
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
    let mut tab = StringTable::new();
    let g = &[
        ("x", e(vec![
            q(t(&mut tab, "a")),
            q(t(&mut tab, "b")),
        ])),
    ];

    assert_eq!(
        Parser::parse(g, "x", "").unwrap(),
        mat((0, 1, 1, 0, 1, 1), vec![])
    );
    assert_eq!(
        Parser::parse(g, "x", "a").unwrap(),
        mat((0, 1, 1, 1, 1, 2), vec![])
    );
    assert_eq!(
        Parser::parse(g, "x", "b").unwrap(),
        mat((0, 1, 1, 1, 1, 2), vec![])
    );
    assert_eq!(
        Parser::parse(g, "x", "ab").unwrap(),
        mat((0, 1, 1, 2, 1, 3), vec![])
    );
}

#[test]
fn c_group() {
    let mut tab = StringTable::new();
    let g = &[
        ("x", c(vec![
            u("A", t(&mut tab, "a")),
            u("B", t(&mut tab, "b")),
        ])),
    ];

    assert_eq!(
        Parser::parse(g, "x", "a").unwrap(),
        mat((0, 1, 1, 1, 1, 2), vec![
            ("A", vec![mat((0, 1, 1, 1, 1, 2), vec![])]),
        ])
    );
    assert_eq!(
        Parser::parse(g, "x", "b").unwrap(),
        mat((0, 1, 1, 1, 1, 2), vec![
            ("B", vec![mat((0, 1, 1, 1, 1, 2), vec![])]),
        ])
    );
}

#[test]
fn s_group() {
    let mut tab = StringTable::new();
    let g = &[
        ("x", s(
            u("A", u("E", t(&mut tab, "a"))),
        )),
    ];

    assert_eq!(
        Parser::parse(g, "x", "").unwrap(),
        mat((0, 1, 1, 0, 1, 1), vec![])
    );
    assert_eq!(
        Parser::parse(g, "x", "aa").unwrap(),
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
    let mut tab = StringTable::new();
    let g = &[
        ("x", p(
            u("A", u("E", t(&mut tab, "a"))),
        )),
    ];

    assert_eq!(
        Parser::parse(g, "x", "aa").unwrap(),
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
    let mut tab = StringTable::new();
    let g = &[
        ("x", q(
            u("A", t(&mut tab, "a"))
        )),
    ];

    assert_eq!(
        Parser::parse(g, "x", "").unwrap(),
        mat((0, 1, 1, 0, 1, 1), vec![])
    );
    assert_eq!(
        Parser::parse(g, "x", "a").unwrap(),
        mat((0, 1, 1, 1, 1, 2), vec![
            ("A", vec![mat((0, 1, 1, 1, 1, 2), vec![])]),
        ])
    );
}

#[test]
fn z_group() {
    let mut tab = StringTable::new();
    let g = &[
        ("x", e(vec![
            z(u("E", t(&mut tab, "a"))),
            u("A", t(&mut tab, "a")),
        ])),
    ];

    assert_eq!(
        Parser::parse(g, "x", "a").unwrap(),
        mat((0, 1, 1, 1, 1, 2), vec![
            ("A", vec![mat((0, 1, 1, 1, 1, 2), vec![])]),
        ])
    );
}

#[test]
fn g_group() {
    let mut tab = StringTable::new();
    let g = &[
        ("x", e(vec![
            n(u("E", t(&mut tab, "e"))),
            u("A", t(&mut tab, "a")),
        ])),
    ];

    assert_eq!(
        Parser::parse(g, "x", "a").unwrap(),
        mat((0, 1, 1, 1, 1, 2), vec![
            ("A", vec![mat((0, 1, 1, 1, 1, 2), vec![])]),
        ])
    );
}

#[test]
fn x_group() {
    let mut tab = StringTable::new();
    let g = &[
        ("x",
            x(u("A", t(&mut tab, "a"))),
        ),
    ];

    assert_eq!(
        Parser::parse(g, "x", "a").unwrap(),
        mat((0, 1, 1, 1, 1, 2), vec![])
    );
}

#[test]
fn k_group() {
    let mut tab = StringTable::new();
    let g = &[
        ("x", k("w")),
        ("w", u("A", t(&mut tab, "a"))),
    ];

    assert_eq!(
        Parser::parse(g, "x", "a").unwrap(),
        mat((0, 1, 1, 1, 1, 2), vec![
            ("A", vec![mat((0, 1, 1, 1, 1, 2), vec![])]),
        ])
    );
}

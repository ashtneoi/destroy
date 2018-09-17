use constructors::*;
use parse::{
    parse_grammar,
    Parser,
};
use Pos;
use string_table::StringTable;
use tests::mat;

#[test]
fn raw_match() {
    let mut tab = StringTable::new();
    let g = &[
        ("start", e(vec![
            s(t(&mut tab, " ")),
            u("a", p(t(&mut tab, "a"))),
            u("tail", s(t(&mut tab, " "))),
        ])),
    ];

    assert_eq!(
        Parser::parse(g, "start", "a").unwrap().raw,
        (Pos { lin: 0, row: 1, col: 1 }, Pos { lin: 1, row: 1, col: 2 })
    );
    assert_eq!(
        Parser::parse(g, "start", "a   ").unwrap().raw,
        (Pos { lin: 0, row: 1, col: 1 }, Pos { lin: 4, row: 1, col: 5 })
    );
    assert_eq!(
        Parser::parse(g, "start", "  a").unwrap().raw,
        (Pos { lin: 0, row: 1, col: 1 }, Pos { lin: 3, row: 1, col: 4 })
    );
    assert_eq!(
        Parser::parse(g, "start", "  a   ").unwrap().raw,
        (Pos { lin: 0, row: 1, col: 1 }, Pos { lin: 6, row: 1, col: 7 })
    );
}

#[test]
fn multichar_text() {
    // Fixed by 47c39cbade923608565a5542e408e91af7cea4be.

    let mut tab = StringTable::new();
    let g = &[
        ("x", t(&mut tab, "aaa")),
    ];

    assert_eq!(
        Parser::parse(g, "x", "aaa").unwrap(),
        mat((0, 1, 1, 3, 1, 4), vec![])
    );
}

#[test]
fn pos_confusion() {
    // Fixed by 5fddc3165725a178817f967eb687d4f173e9b166.

    let mut tab = StringTable::new();
    let g = &[
        ("x", e(vec![
            s(e(vec![
                s(t(&mut tab, "a")),
                t(&mut tab, "b"),
            ])),
            t(&mut tab, "a"),
        ])),
    ];

    Parser::parse(g, "x", "a").unwrap();
}

#[test]
fn wrong_pos_after_nl() {
    // Fixed by dfc4bd5d6b9d14bef7d4873b0a0a07c0d1fafe95.

    let mut tab = StringTable::new();
    let g = &[
        ("x", e(vec![
            t(&mut tab, "a\nb"),
        ])),
    ];

    assert_eq!(
        Parser::parse(g, "x", "a\nb").unwrap().raw,
        (Pos { lin: 0, row: 1, col: 1}, Pos { lin: 3, row: 2, col: 2 })
    );
}

#[test]
fn choice_precedence() {
    // Fixed by 24ed683888da95edb2eea971da2e342d7ac40284.
    let mut tab = StringTable::new();
    let g = parse_grammar(&mut tab, r##"A = "a" / "b"? "c""##).unwrap();
    Parser::parse(&g, "A", "a").unwrap();
}

#[test]
fn wrong_prefix_order() {
    // Fixed by c5a113997e5b962ed247ef62f1e04099260f7d18.
    let mut tab = StringTable::new();
    let g1 = parse_grammar(&mut tab, r##"A = ^-"a""##).unwrap();
    assert_eq!(g1, vec![
        ("A".to_string(), c(vec![e(vec![z(n(t(&mut tab, "a")))])])),
    ]);
}

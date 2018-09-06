use constructors::*;
use GrammarAtom;
use parse::{
    parse_grammar,
    ParseError,
    Parser,
};
use Pos;
use tests::mat;

#[test]
fn raw_match() {
    let g = &[
        ("start", e(vec![
            s(t(" ")),
            u("a", p(t("a"))),
            u("tail", s(t(" "))),
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

    let g = &[
        ("x", t("aaa")),
    ];

    assert_eq!(
        Parser::parse(g, "x", "aaa").unwrap(),
        mat((0, 1, 1, 3, 1, 4), vec![])
    );
}

#[test]
fn pos_confusion() {
    // Fixed by 5fddc3165725a178817f967eb687d4f173e9b166.

    let g = &[
        ("x", e(vec![
            s(e(vec![
                s(t("a")),
                t("b"),
            ])),
            t("a"),
        ])),
    ];

    Parser::parse(g, "x", "a").unwrap();
}

#[test]
fn wrong_pos_after_nl() {
    // Fixed by dfc4bd5d6b9d14bef7d4873b0a0a07c0d1fafe95.

    let g = &[
        ("x", e(vec![
            t("a\nb"),
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
    let g = parse_grammar(r##"A = "a" / "b"? "c""##).unwrap();
    Parser::parse(&g, "A", "a").unwrap();
}

#[test]
fn wrong_prefix_order() {
    // Fixed by c5a113997e5b962ed247ef62f1e04099260f7d18.
    let g1 = parse_grammar(r##"A = ^-"a""##).unwrap();
    assert_eq!(g1, vec![
        ("A".to_string(), c(vec![e(vec![z(n(t("a")))])])),
    ]);
}

static SIMPLE_GRAMMAR_GRAMMAR_STR: &str = r##"
    nzdigit = '1'..'9'
    digit = "0" / nzdigit
    latin_letter = 'a'..'z' / 'A'..'Z'

    comment = "#" (-"\n" %)*

    wso_part = " " / "\t"
    ws_part = wso_part / comment? "\n"
    wso = wso_part*
    ws = ws_part*
    pwso = wso_part+
    pws = ws_part+

    hex_digit = digit / 'a'..'f' / 'A'..'F'
    hex_uint = "0x" hex_digit+

    str =
        "\""
        ("\\" ("n" / "t" / "\\" / "\"") / -"\"" -"\n" %)[cp]*
        "\""
    cp =
        hex_uint[hex]
        / "'" ("\\" ("n" / "t" / "\\" / "'") / -"'" -"\n" %)[raw] "'"
    cp_range = cp[from] ".." cp[to]
    ident_initial = latin_letter / "_" / 0x80..0x10FFFF # TODO
    ident = ident_initial (ident_initial / digit)* # TODO

    expr = expr_seq[opd] (ws "/" ws expr_seq[opd])*
    expr_seq = expr_affix[opd] (pws expr_affix[opd] -(wso "="))*
    expr_affix =
        ("^" / "-")[pre]*
        expr_atom[opd]
        ("*" / "+" / "?" / "[" ident[name] "]")[suf]*
    expr_atom =
        "%" / str / cp_range[r] / ident[id] / "(" ws expr[expr] ws ")"

    rule = ident[name] wso "=" ws expr[val]
    grammar = ws (rule wso comment? "\n" ws)* (rule wso comment?)?
"##;

#[test]
fn bad_expected_eof() {
    use GrammarAtom::*;

    let g = parse_grammar(SIMPLE_GRAMMAR_GRAMMAR_STR).unwrap();

    let e = Parser::parse(&g, "grammar", "a =").unwrap_err();
    match e {
        ParseError::MatchFail(_, pos, expected) => {
            assert_eq!(pos.lin, 3);
            assert!(!expected.contains(&Text("".to_string())));
        },
        e => panic!(format!("{}", e)),
    }
}

use constructors::*;
use parse::{
    get_grammar_grammar,
    parse_grammar,
    parse_grammar_with_grammar,
    Parser,
};
use test::Bencher;

#[test]
fn ident() {
    let g = get_grammar_grammar();

    Parser::parse(&g, "ident", "a").unwrap();
    Parser::parse(&g, "ident", "A").unwrap();
    Parser::parse(&g, "ident", "_").unwrap();
    Parser::parse(&g, "ident", "foo").unwrap();
    Parser::parse(&g, "ident", "foo_bar").unwrap();
    Parser::parse(&g, "ident", "_foo_bar_").unwrap();
    Parser::parse(&g, "ident", "a3").unwrap();
    Parser::parse(&g, "ident", "_3").unwrap();

    Parser::parse(&g, "ident", "3").unwrap_err();
    Parser::parse(&g, "ident", "3a").unwrap_err();
    Parser::parse(&g, "ident", "3_").unwrap_err();
}

#[bench]
fn bench_ident(b: &mut Bencher) {
    let g = get_grammar_grammar();

    b.iter(|| Parser::parse(&g, "ident", "_foo_bar90"));
}

#[test]
fn expr() {
    let g = get_grammar_grammar();

    Parser::parse(&g, "expr", "\"a\"").unwrap();
    Parser::parse(&g, "expr", "0x80..0x10FFFF").unwrap();
}

#[test]
fn wso() {
    let g = get_grammar_grammar();

    Parser::parse(&g, "wso", "").unwrap();
    Parser::parse(&g, "wso", " ").unwrap();
    Parser::parse(&g, "wso", "\t\t  ").unwrap();
    Parser::parse(&g, "wso", "\t\t\t\t\t").unwrap();

    Parser::parse(&g, "wso", "\n").unwrap_err();
    Parser::parse(&g, "wso", "  \n").unwrap_err();
    Parser::parse(&g, "wso", "\t\t\n").unwrap_err();
    Parser::parse(&g, "wso", "# foo\n").unwrap_err();
    Parser::parse(&g, "wso", "\n# foo").unwrap_err();
}

#[test]
fn ws() {
    let g = get_grammar_grammar();

    Parser::parse(&g, "ws", "").unwrap();
    Parser::parse(&g, "ws", " ").unwrap();
    Parser::parse(&g, "ws", "\t\t  ").unwrap();
    Parser::parse(&g, "ws", "# foo\n").unwrap();
    Parser::parse(&g, "ws", " # foo\n").unwrap();
    Parser::parse(&g, "ws", "\t\t  # foo\n").unwrap();
    Parser::parse(&g, "ws", "#\t\t\t\t\t\n").unwrap();
    Parser::parse(&g, "ws", "\n").unwrap();
    Parser::parse(&g, "ws", "\n\n\n\n").unwrap();
    Parser::parse(&g, "ws", "  \n").unwrap();
    Parser::parse(&g, "ws", "\t\t\n").unwrap();
    Parser::parse(&g, "ws", "# foo\n").unwrap();
    Parser::parse(&g, "ws", "\n# foo\n").unwrap();
}

#[test]
fn expr_plus() {
    let g = get_grammar_grammar();

    Parser::parse(&g, "expr", "c+").unwrap();
    Parser::parse(&g, "rule", "a = b c+").unwrap();
}

#[test]
fn expr_atom() {
    let g = get_grammar_grammar();

    Parser::parse(&g, "expr_atom", "\"a\"").unwrap();
}

#[test]
fn rule() {
    let g = get_grammar_grammar();

    Parser::parse(&g, "rule", "A = \"a\"").unwrap();
}

#[test]
fn grammar() {
    let g = get_grammar_grammar();

    Parser::parse(&g, "grammar", "A = \"a\"").unwrap();
    Parser::parse(&g, "grammar", "A = \"a\"\n").unwrap();
    Parser::parse(&g, "grammar", "A = \"a\"\n\n").unwrap();
    Parser::parse(&g, "grammar", "A = \"a\"\nB = \"b\"").unwrap();
    Parser::parse(&g, "grammar", "A = \"a\"\nB = \"b\"\n").unwrap();
    Parser::parse(&g, "grammar", "A = \"a\"\nB = \"b\"\n\n").unwrap();
}

static GRAMMAR_GRAMMAR_STR: &str = r##"
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
fn bootstrap_stage1_parse_only() {
    let g0 = get_grammar_grammar();

    Parser::parse(&g0, "grammar", GRAMMAR_GRAMMAR_STR).unwrap();
}

#[test]
fn parse_minimal_grammar() {
    let i = r##"
        A = "a"*
    "##;

    let g = parse_grammar(i).unwrap();
    assert_eq!(
        g,
        vec![
            ("A".to_string(), c(vec![e(vec![s(t("a"))])])),
        ],
    );
}

#[test]
fn bootstrap() {
    let g1 = parse_grammar(GRAMMAR_GRAMMAR_STR).unwrap();

    let g2 = parse_grammar_with_grammar(&g1, GRAMMAR_GRAMMAR_STR)
        .unwrap();

    assert_eq!(&g1, &g2);

    let g3 = parse_grammar_with_grammar(&g2, GRAMMAR_GRAMMAR_STR)
        .unwrap();

    assert_eq!(&g2, &g3);
}

#[bench]
fn bench_bootstrap(b: &mut Bencher) {
    b.iter(|| parse_grammar(GRAMMAR_GRAMMAR_STR).unwrap());
}

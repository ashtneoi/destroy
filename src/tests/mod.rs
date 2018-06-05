mod tree;

mod standard {
    use mat;
    use parse;
    use prelude::*;

    #[test]
    fn minimal() {
        let g = &[
            ("start", s(t("a"))),
        ];

        assert_eq!(
            parse(g, "start", "aaa").unwrap(),
            mat((0, 1, 1, 3, 1, 4), vec![])
        );
    }

    #[test]
    fn optional() {
        let g = &[
            ("start", e(vec![
                q(t("-")),
                q(t("+")),
            ])),
        ];

        parse(g, "start", "-+").unwrap();
        parse(g, "start", "-").unwrap();
        parse(g, "start", "+").unwrap();
        parse(g, "start", "").unwrap();

        parse(g, "start", "+-").unwrap_err();
        parse(g, "start", " ").unwrap_err();
        parse(g, "start", "+0").unwrap_err();
        parse(g, "start", "0+").unwrap_err();
    }

    #[test]
    fn decimal_integer() {
        let g = &[
            ("dec_nonzero_digit", c(vec![
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
            ("dec_digit", c(vec![
                t("0"),
                k("dec_nonzero_digit"),
            ])),
            ("dec_int", e(vec![
                q(t("-")),
                c(vec![
                    t("0"),
                    e(vec![
                        k("dec_nonzero_digit"),
                        s(k("dec_digit")),
                    ]),
                ]),
            ])),
        ];

        parse(g, "dec_int", "0").unwrap();
        parse(g, "dec_int", "1").unwrap();
        parse(g, "dec_int", "9").unwrap();
        parse(g, "dec_int", "10").unwrap();
        parse(g, "dec_int", "19").unwrap();
        parse(g, "dec_int", "99").unwrap();
        parse(g, "dec_int", "-0").unwrap();
        parse(g, "dec_int", "-1").unwrap();
        parse(g, "dec_int", "-9").unwrap();
        parse(g, "dec_int", "-10").unwrap();
        parse(g, "dec_int", "-19").unwrap();
        parse(g, "dec_int", "-99").unwrap();

        parse(g, "dec_int", "y").unwrap_err();
        parse(g, "dec_int", "-").unwrap_err();
        parse(g, "dec_int", "0-").unwrap_err();
        parse(g, "dec_int", "1-").unwrap_err();
        parse(g, "dec_int", "01").unwrap_err();
    }

    #[test]
    fn simple_expr() {
        let g = &[
            ("expr", e(vec![
                k("expr2"),
                s(e(vec![
                    t("+"),
                    k("expr2"),
                ])),
            ])),
            ("expr2", c(vec![
                t("1"),
                e(vec![
                    t("("),
                    k("expr"),
                    t(")"),
                ]),
            ])),
        ];

        parse(g, "expr", "1").unwrap();
        parse(g, "expr", "1+1").unwrap();
        parse(g, "expr", "(1+1+1)+1").unwrap();
        parse(g, "expr", "1+(1+1+1)").unwrap();
    }

    #[test]
    fn range() {
        let g = &[
            ("start", e(vec![
                r('a', 'd'),
                t("e"),
            ])),
        ];

        parse(g, "start", "ae").unwrap();
        parse(g, "start", "be").unwrap();
        parse(g, "start", "ce").unwrap();
        parse(g, "start", "de").unwrap();

        parse(g, "start", "a").unwrap_err();
        parse(g, "start", "e").unwrap_err();
        parse(g, "start", "ee").unwrap_err();
        parse(g, "start", "ea").unwrap_err();
    }

    #[test]
    fn lookahead() {
        let g = &[
            ("start", e(vec![
                z(t("a")),
                r('a', 'c'),
                g(t("a")),
                r('a', 'c'),
            ])),
        ];

        parse(g, "start", "ab").unwrap();
        parse(g, "start", "ac").unwrap();

        parse(g, "start", "aa").unwrap_err();
        parse(g, "start", "ba").unwrap_err();
        parse(g, "start", "bc").unwrap_err();
        parse(g, "start", ".c").unwrap_err();
        parse(g, "start", "a").unwrap_err();
        parse(g, "start", "b").unwrap_err();
        parse(g, "start", "").unwrap_err();
    }

    mod group_tests {
        use mat;
        use parse;
        use prelude::*;

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
    }

    #[test]
    fn anything() {
        let g = &[
            ("x", e(vec![
                a(),
                a(),
            ])),
        ];

        assert_eq!(
            parse(g, "x", "ab").unwrap(),
            mat((0, 1, 1, 2, 1, 3), vec![])
        );
        assert_eq!(
            parse(g, "x", " \n").unwrap(),
            mat((0, 1, 1, 2, 2, 1), vec![])
        );

        parse(g, "x", "").unwrap_err();
        parse(g, "x", "a").unwrap_err();
        parse(g, "x", "abc").unwrap_err();
    }

    mod grammar_grammar_tests {
        use parse;
        use parse_grammar;
        use parse_grammar_with_grammar;
        use prelude::*;
        use test::Bencher;

        #[test]
        fn ident() {
            let g = get_grammar_grammar();

            parse(&g, "ident", "a").unwrap();
            parse(&g, "ident", "A").unwrap();
            parse(&g, "ident", "_").unwrap();
            parse(&g, "ident", "foo").unwrap();
            parse(&g, "ident", "foo_bar").unwrap();
            parse(&g, "ident", "_foo_bar_").unwrap();
            parse(&g, "ident", "a3").unwrap();
            parse(&g, "ident", "_3").unwrap();

            parse(&g, "ident", "3").unwrap_err();
            parse(&g, "ident", "3a").unwrap_err();
            parse(&g, "ident", "3_").unwrap_err();
        }

        #[bench]
        fn bench_ident(b: &mut Bencher) {
            let g = get_grammar_grammar();

            b.iter(|| parse(&g, "ident", "_foo_bar90"));
        }

        #[test]
        fn expr() {
            let g = get_grammar_grammar();

            parse(&g, "expr", "\"a\"").unwrap();
            parse(&g, "expr", "0x80..0x10FFFF").unwrap();
        }

        #[test]
        fn wso() {
            let g = get_grammar_grammar();

            parse(&g, "wso", "").unwrap();
            parse(&g, "wso", " ").unwrap();
            parse(&g, "wso", "\t\t  ").unwrap();
            parse(&g, "wso", "\t\t\t\t\t").unwrap();

            parse(&g, "wso", "\n").unwrap_err();
            parse(&g, "wso", "  \n").unwrap_err();
            parse(&g, "wso", "\t\t\n").unwrap_err();
            parse(&g, "wso", "# foo\n").unwrap_err();
            parse(&g, "wso", "\n# foo").unwrap_err();
        }

        #[test]
        fn ws() {
            let g = get_grammar_grammar();

            parse(&g, "ws", "").unwrap();
            parse(&g, "ws", " ").unwrap();
            parse(&g, "ws", "\t\t  ").unwrap();
            parse(&g, "ws", "# foo\n").unwrap();
            parse(&g, "ws", " # foo\n").unwrap();
            parse(&g, "ws", "\t\t  # foo\n").unwrap();
            parse(&g, "ws", "#\t\t\t\t\t\n").unwrap();
            parse(&g, "ws", "\n").unwrap();
            parse(&g, "ws", "\n\n\n\n").unwrap();
            parse(&g, "ws", "  \n").unwrap();
            parse(&g, "ws", "\t\t\n").unwrap();
            parse(&g, "ws", "# foo\n").unwrap();
            parse(&g, "ws", "\n# foo\n").unwrap();
        }

        #[test]
        fn expr_plus() {
            let g = get_grammar_grammar();

            parse(&g, "expr", "c+").unwrap();
            parse(&g, "rule", "a = b c+").unwrap();
        }

        #[test]
        fn expr_atom() {
            let g = get_grammar_grammar();

            parse(&g, "expr_atom", "\"a\"").unwrap();
        }

        #[test]
        fn rule() {
            let g = get_grammar_grammar();

            parse(&g, "rule", "A = \"a\"").unwrap();
        }

        #[test]
        fn grammar() {
            let g = get_grammar_grammar();

            parse(&g, "grammar", "A = \"a\"").unwrap();
            parse(&g, "grammar", "A = \"a\"\n").unwrap();
            parse(&g, "grammar", "A = \"a\"\n\n").unwrap();
            parse(&g, "grammar", "A = \"a\"\nB = \"b\"").unwrap();
            parse(&g, "grammar", "A = \"a\"\nB = \"b\"\n").unwrap();
            parse(&g, "grammar", "A = \"a\"\nB = \"b\"\n\n").unwrap();
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

            expr = expr_seq[e] (ws "/" ws expr_seq[e])*
            expr_seq = expr_prefix[pre] (pws expr_prefix[pre] -(wso "="))*
            expr_prefix = ("^" / "-")[op]* expr_suffix[suf]
            expr_suffix =
                expr_atom[atom] ("*" / "+" / "?" / "[" ident[name] "]")[op]*
            expr_atom =
                "%" / str / cp_range[r] / ident[id] / "(" ws expr[c] ws ")"

            rule = ident[name] wso "=" ws expr[val]
            grammar = ws (rule wso comment? "\n" ws)* (rule wso comment?)?
        "##;

        #[test]
        fn bootstrap_stage1_parse_only() {
            let g0 = get_grammar_grammar();

            parse(&g0, "grammar", GRAMMAR_GRAMMAR_STR).unwrap();
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
        fn bootstrap_stage1() {
            let g1 = parse_grammar(GRAMMAR_GRAMMAR_STR).unwrap();

            parse(&g1, "ws", r##" "##).unwrap();
        }

        #[test]
        fn bootstrap_stage2() {
            let g1 = parse_grammar(GRAMMAR_GRAMMAR_STR).unwrap();

            let g2 = parse_grammar_with_grammar(&g1, GRAMMAR_GRAMMAR_STR)
                .unwrap();

            assert_eq!(&g1, &g2);
        }

        #[test]
        fn bootstrap_stage3() {
            let g1 = parse_grammar(GRAMMAR_GRAMMAR_STR).unwrap();

            let g2 = parse_grammar_with_grammar(&g1, GRAMMAR_GRAMMAR_STR)
                .unwrap();

            let g3 = parse_grammar_with_grammar(&g2, GRAMMAR_GRAMMAR_STR)
                .unwrap();

            assert_eq!(&g2, &g3);
        }
    }
}

mod regression {
    use mat;
    use parse;
    use parse_grammar;
    use prelude::*;
    use Pos;

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
            parse(g, "start", "a").unwrap().raw,
            (Pos { lin: 0, row: 1, col: 1 }, Pos { lin: 1, row: 1, col: 2 })
        );
        assert_eq!(
            parse(g, "start", "a   ").unwrap().raw,
            (Pos { lin: 0, row: 1, col: 1 }, Pos { lin: 4, row: 1, col: 5 })
        );
        assert_eq!(
            parse(g, "start", "  a").unwrap().raw,
            (Pos { lin: 0, row: 1, col: 1 }, Pos { lin: 3, row: 1, col: 4 })
        );
        assert_eq!(
            parse(g, "start", "  a   ").unwrap().raw,
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
            parse(g, "x", "aaa").unwrap(),
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

        parse(g, "x", "a").unwrap();
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
            parse(g, "x", "a\nb").unwrap().raw,
            (Pos { lin: 0, row: 1, col: 1}, Pos { lin: 3, row: 2, col: 2 })
        );
    }

    #[test]
    fn choice_precedence() {
        // Fixed by 24ed683888da95edb2eea971da2e342d7ac40284.
        let g = parse_grammar(r##"A = "a" / "b"? "c""##).unwrap();
        parse(&g, "A", "a").unwrap();
    }

    #[test]
    fn wrong_prefix_order() {
        // Fixed by c5a113997e5b962ed247ef62f1e04099260f7d18.
        let g1 = parse_grammar(r##"A = ^-"a""##).unwrap();
        assert_eq!(g1, vec![
            ("A".to_string(), c(vec![e(vec![z(g(t("a")))])])),
        ]);
    }
}

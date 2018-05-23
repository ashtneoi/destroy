mod tree;

mod standard {
    use prelude::*;
    use mat;

    #[test]
    fn minimal() {
        let mut g = e(vec![
            n("start", s(t("a"))),
        ]);

        assert_eq!(
            g.parse("start", "aaa").unwrap(),
            mat((0, 1, 1, 3, 1, 4), vec![])
        );
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

    mod group_tests {
        use prelude::*;
        use mat;

        #[test]
        fn e_group_two_names() {
            let mut g = n("x", e(vec![
                u("A", q(t("a"))),
                u("B", q(t("b"))),
            ]));

            assert_eq!(
                g.parse("x", "ab").unwrap(),
                mat((0, 1, 1, 2, 1, 3), vec![
                    ("A", vec![mat((0, 1, 1, 1, 1, 2), vec![])]),
                    ("B", vec![mat((1, 1, 2, 2, 1, 3), vec![])]),
                ])
            );
        }

        #[test]
        fn e_group_one_name() {
            let mut g1 = n("x", e(vec![
                q(t("a")),
                u("B", t("b")),
            ]));

            assert_eq!(
                g1.parse("x", "b").unwrap(),
                mat((0, 1, 1, 1, 1, 2), vec![
                    ("B", vec![mat((0, 1, 1, 1, 1, 2), vec![])]),
                ])
            );
            assert_eq!(
                g1.parse("x", "ab").unwrap(),
                mat((0, 1, 1, 2, 1, 3), vec![
                    ("B", vec![mat((1, 1, 2, 2, 1, 3), vec![])]),
                ])
            );

            let mut g2 = n("x", e(vec![
                u("A", t("a")),
                q(t("b")),
            ]));

            assert_eq!(
                g2.parse("x", "a").unwrap(),
                mat((0, 1, 1, 1, 1, 2), vec![
                    ("A", vec![mat((0, 1, 1, 1, 1, 2), vec![])]),
                ])
            );
            assert_eq!(
                g2.parse("x", "ab").unwrap(),
                mat((0, 1, 1, 2, 1, 3), vec![
                    ("A", vec![mat((0, 1, 1, 1, 1, 2), vec![])]),
                ])
            );
        }

        #[test]
        fn e_group_same_name() {
            let mut g = n("x", e(vec![
                u("A", t("a")),
                u("A", t("b")),
            ]));

            assert_eq!(
                g.parse("x", "ab").unwrap(),
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
            let mut g = n("x", e(vec![
                q(t("a")),
                q(t("b")),
            ]));

            assert_eq!(
                g.parse("x", "").unwrap(),
                mat((0, 1, 1, 0, 1, 1), vec![])
            );
            assert_eq!(
                g.parse("x", "a").unwrap(),
                mat((0, 1, 1, 1, 1, 2), vec![])
            );
            assert_eq!(
                g.parse("x", "b").unwrap(),
                mat((0, 1, 1, 1, 1, 2), vec![])
            );
            assert_eq!(
                g.parse("x", "ab").unwrap(),
                mat((0, 1, 1, 2, 1, 3), vec![])
            );
        }

        #[test]
        fn c_group() {
            let mut g = n("x", c(vec![
                u("A", t("a")),
                u("B", t("b")),
            ]));

            assert_eq!(
                g.parse("x", "a").unwrap(),
                mat((0, 1, 1, 1, 1, 2), vec![
                    ("A", vec![mat((0, 1, 1, 1, 1, 2), vec![])]),
                ])
            );
            assert_eq!(
                g.parse("x", "b").unwrap(),
                mat((0, 1, 1, 1, 1, 2), vec![
                    ("B", vec![mat((0, 1, 1, 1, 1, 2), vec![])]),
                ])
            );
        }

        #[test]
        fn s_group() {
            let mut g = n("x", s(
                u("A", u("E", t("a"))),
            ));

            assert_eq!(
                g.parse("x", "").unwrap(),
                mat((0, 1, 1, 0, 1, 1), vec![])
            );
            assert_eq!(
                g.parse("x", "aa").unwrap(),
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
            let mut g = n("x", p(
                u("A", u("E", t("a"))),
            ));

            assert_eq!(
                g.parse("x", "aa").unwrap(),
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
            let mut g = n("x", q(
                u("A", t("a"))
            ));

            assert_eq!(
                g.parse("x", "").unwrap(),
                mat((0, 1, 1, 0, 1, 1), vec![])
            );
            assert_eq!(
                g.parse("x", "a").unwrap(),
                mat((0, 1, 1, 1, 1, 2), vec![
                    ("A", vec![mat((0, 1, 1, 1, 1, 2), vec![])]),
                ])
            );
        }

        #[test]
        fn z_group() {
            let mut g = n("x", e(vec![
                z(u("E", t("a"))),
                u("A", t("a")),
            ]));

            assert_eq!(
                g.parse("x", "a").unwrap(),
                mat((0, 1, 1, 1, 1, 2), vec![
                    ("A", vec![mat((0, 1, 1, 1, 1, 2), vec![])]),
                ])
            );
        }

        #[test]
        fn g_group() {
            let mut g = n("x", e(vec![
                g(u("E", t("e"))),
                u("A", t("a")),
            ]));

            assert_eq!(
                g.parse("x", "a").unwrap(),
                mat((0, 1, 1, 1, 1, 2), vec![
                    ("A", vec![mat((0, 1, 1, 1, 1, 2), vec![])]),
                ])
            );
        }

        #[test]
        fn n_group() {
            let mut g = n("x",
                n("E", u("A", t("a"))),
            );

            assert_eq!(
                g.parse("x", "a").unwrap(),
                mat((0, 1, 1, 1, 1, 2), vec![
                    ("A", vec![mat((0, 1, 1, 1, 1, 2), vec![])]),
                ])
            );
        }

        #[test]
        fn x_group() {
            let mut g = n("x",
                x(u("A", t("a"))),
            );

            assert_eq!(
                g.parse("x", "a").unwrap(),
                mat((0, 1, 1, 1, 1, 2), vec![])
            );
        }

        #[test]
        fn k_group() {
            let mut g = e(vec![
                n("x", k("w")),
                n("w", u("A", t("a"))),
            ]);

            assert_eq!(
                g.parse("x", "a").unwrap(),
                mat((0, 1, 1, 1, 1, 2), vec![
                    ("A", vec![mat((0, 1, 1, 1, 1, 2), vec![])]),
                ])
            );
        }
    }

    #[test]
    fn anything() {
        let mut g = n("x", e(vec![
            a(),
            a(),
        ]));

        assert_eq!(
            g.parse("x", "ab").unwrap(),
            mat((0, 1, 1, 2, 1, 3), vec![])
        );
        assert_eq!(
            g.parse("x", " \n").unwrap(),
            mat((0, 1, 1, 2, 2, 1), vec![])
        );

        g.parse("x", "").unwrap_err();
        g.parse("x", "a").unwrap_err();
        g.parse("x", "abc").unwrap_err();
    }

    mod grammar_grammar_tests {
        use prelude::*;
        use test::Bencher;

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

        #[bench]
        fn bench_ident(b: &mut Bencher) {
            let mut g = get_grammar_grammar();

            b.iter(|| g.parse("ident", "_foo_bar90"));
        }

        #[test]
        fn expr() {
            let mut g = get_grammar_grammar();

            g.parse("expr", "\"a\"").unwrap();
            g.parse("expr", "0x80..0x10FFFF").unwrap();
        }

        #[test]
        fn wso() {
            let mut g = get_grammar_grammar();

            g.parse("wso", "").unwrap();
            g.parse("wso", " ").unwrap();
            g.parse("wso", "\t\t  ").unwrap();
            g.parse("wso", "\t\t\t\t\t").unwrap();

            g.parse("wso", "\n").unwrap_err();
            g.parse("wso", "  \n").unwrap_err();
            g.parse("wso", "\t\t\n").unwrap_err();
            g.parse("wso", "# foo\n").unwrap_err();
            g.parse("wso", "\n# foo").unwrap_err();
        }

        #[test]
        fn ws() {
            let mut g = get_grammar_grammar();

            g.parse("ws", "").unwrap();
            g.parse("ws", " ").unwrap();
            g.parse("ws", "\t\t  ").unwrap();
            g.parse("ws", "# foo\n").unwrap();
            g.parse("ws", " # foo\n").unwrap();
            g.parse("ws", "\t\t  # foo\n").unwrap();
            g.parse("ws", "#\t\t\t\t\t\n").unwrap();
            g.parse("ws", "\n").unwrap();
            g.parse("ws", "\n\n\n\n").unwrap();
            g.parse("ws", "  \n").unwrap();
            g.parse("ws", "\t\t\n").unwrap();
            g.parse("ws", "# foo\n").unwrap();
            g.parse("ws", "\n# foo\n").unwrap();
        }

        #[test]
        fn expr_plus() {
            let mut g = get_grammar_grammar();

            g.parse("expr", "c+").unwrap();
            g.parse("rule", "a = b c+").unwrap();
        }

        #[test]
        fn expr_atom() {
            let mut g = get_grammar_grammar();

            g.parse("expr_atom", "\"a\"").unwrap();
        }

        #[test]
        fn rule() {
            let mut g = get_grammar_grammar();

            g.parse("rule", "A = \"a\"").unwrap();
        }
    }

    static GRAMMAR_GRAMMAR_STR: &str = r##"
        comment = "#" (-"\n" %)*

        wso_part = " " / "\t"
        ws_part = wso_part / comment? "\n"
        wso = wso_part*
        ws = ws_part*
        pwso = wso_part+
        pws = ws_part+

        hex_digit = digit / 'a'..'f' / 'A'..'F'
        hex_uint = "0x" hex_digit+

        str = "\"" ("\\" ("n" / "\\" / "\"") / -"\"" -"\n" %)* "\""
        cp = hex_uint / "'" ("\\" ("n" / "\\" / "'") / -"'" -"\n" %) "'"
        cp_range = cp ".." cp
        ident_initial = latin_letter / "_" / 0x80..0x10FFFF # TODO
        ident = ident_initial (ident_initial / digit)* # TODO

        expr = expr_choice (pws expr_choice -(wso "="))*
        expr_choice = expr_prefix[opd] (ws "/"[op] ws expr_prefix[opd])*
        expr_prefix = ("^" / "-")[op]* expr_suffix[opd]
        expr_suffix =
            expr_atom[opd] ("*" / "+" / "?" / "[" ident[name] "]")[op]*
        expr_atom =
            ("%" / str / cp_range / ident / "(" ws expr ws ")")[atom]

        rule = ident[name] wso "=" ws expr[val]
        grammar = ws (rule wso comment? "\n" ws)*
    "##;

    #[test]
    fn meta_grammar_parse() {
        let mut g = get_grammar_grammar();

        g.parse("grammar", GRAMMAR_GRAMMAR_STR).unwrap();
    }
}

mod regression {
    use prelude::*;
    use mat;
    use Pos;

    #[test]
    fn raw_match() {
        let mut g = n("start", e(vec![
            s(t(" ")),
            u("a", p(t("a"))),
            u("tail", s(t(" "))),
        ]));

        assert_eq!(
            g.parse("start", "a").unwrap().raw,
            (Pos { lin: 0, row: 1, col: 1 }, Pos { lin: 1, row: 1, col: 2 })
        );
        assert_eq!(
            g.parse("start", "a   ").unwrap().raw,
            (Pos { lin: 0, row: 1, col: 1 }, Pos { lin: 4, row: 1, col: 5 })
        );
        assert_eq!(
            g.parse("start", "  a").unwrap().raw,
            (Pos { lin: 0, row: 1, col: 1 }, Pos { lin: 3, row: 1, col: 4 })
        );
        assert_eq!(
            g.parse("start", "  a   ").unwrap().raw,
            (Pos { lin: 0, row: 1, col: 1 }, Pos { lin: 6, row: 1, col: 7 })
        );
    }

    #[test]
    fn multichar_text() {
        // Fixed by 47c39cbade923608565a5542e408e91af7cea4be.

        let mut g = n("x", t("aaa"));

        assert_eq!(
            g.parse("x", "aaa").unwrap(),
            mat((0, 1, 1, 3, 1, 4), vec![])
        );
    }

    #[test]
    fn pos_confusion() {
        // Fixed by 5fddc3165725a178817f967eb687d4f173e9b166.

        let mut g = n("x", e(vec![
            s(e(vec![
                s(t("a")),
                t("b"),
            ])),
            t("a"),
        ]));

        g.parse("x", "a").unwrap();
    }

    #[test]
    fn wrong_pos_after_nl() {
        // Fixed by dfc4bd5d6b9d14bef7d4873b0a0a07c0d1fafe95.

        let mut g = n("x", e(vec![
            t("a\nb"),
        ]));

        assert_eq!(
            g.parse("x", "a\nb").unwrap().raw,
            (Pos { lin: 0, row: 1, col: 1}, Pos { lin: 3, row: 2, col: 2 })
        );
    }
}

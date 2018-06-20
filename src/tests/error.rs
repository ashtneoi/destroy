mod initial {
    use GrammarAtom;
    use GrammarAtom::*;
    use constructors::*;
    use GrammarNode;
    use parse::ParseNode;
    use parse::Parser;

    fn initial(grammar: GrammarNode) -> Vec<GrammarAtom> {
        let named = [("start", grammar)];
        let mut pn = ParseNode::new();
        let mut p = Parser::new(&named, "start", "", &mut pn).unwrap();

        p.initial()
    }

    fn tt(s: &str) -> GrammarAtom {
        Text(s.to_string())
    }

    #[test]
    fn seq() {
        assert_eq!(
            initial(e(vec![
                t("foo"),
            ])),
            vec![tt("foo")],
        );
        assert_eq!(
            initial(e(vec![
                t("foo"),
                t("bar"),
            ])),
            vec![tt("foo")],
        );
        assert_eq!(
            initial(e(vec![
                e(vec![
                    t("foo"),
                ]),
                t("bar"),
            ])),
            vec![tt("foo")],
        );
    }

    #[test]
    fn choice() {
        assert_eq!(
            initial(c(vec![
                t("foo")
            ])),
            vec![tt("foo")],
        );
        assert_eq!(
            initial(c(vec![
                t("foo"),
                t("bar")
            ])),
            vec![tt("foo"), tt("bar")],
        );
        assert_eq!(
            initial(c(vec![
                c(vec![
                    t("foo"),
                ]),
                t("bar"),
            ])),
            vec![tt("foo"), tt("bar")],
        );
    }

    #[test]
    fn simple() {
        let gg = vec![
            p(t("foo")),
            z(t("foo")),
            n(n(t("foo"))),
            u("bar", t("foo")),
            x(t("foo")),
        ];
        for g in gg {
            assert_eq!(
                initial(g),
                vec![tt("foo")],
            );
        }
    }

    #[test]
    fn star() {
        assert_eq!(
            initial(s(t("foo"))),
            vec![tt("foo"), tt("")],
        );
    }

    #[test]
    fn opt() {
        assert_eq!(
            initial(q(t("foo"))),
            vec![tt("foo"), tt("")],
        );
    }

    #[test]
    fn pos() {
        assert_eq!(
            initial(e(vec![
                z(t("foo")),
                t("foo"),
            ])),
            vec![tt("foo")],
        );
    }

    #[test]
    fn neg() {
        assert_eq!(
            initial(n(t("foo"))),
            vec![tt("")],
        );
        assert_eq!(
            initial(n(e(vec![
                t("foo"),
                t("bar"),
            ]))),
            vec![tt("")],
        );
        assert_eq!(
            initial(n(e(vec![
                t("foo"),
                n(t("bar")),
            ]))),
            vec![tt("")],
        );
        assert_eq!(
            initial(n(e(vec![
                e(vec![
                    e(vec![
                        t("foo"),
                    ]),
                ]),
            ]))),
            vec![tt("")],
        );
        assert_eq!(
            initial(e(vec![
                n(t("foo")),
                t("bar"),
            ])),
            vec![tt("bar")],
        );
        assert_eq!(
            initial(n(n(t("foo")))),
            vec![tt("foo")],
        );
    }

    #[test]
    fn text() {
        assert_eq!(initial(t("foo")), vec![tt("foo")]);
    }

    #[test]
    fn range() {
        assert_eq!(initial(r('a', 'z')), vec![Range('a', 'z')]);
    }

    #[test]
    fn anything() {
        assert_eq!(initial(a()), vec![Anything]);
    }
}

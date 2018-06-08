mod initial {
    use GrammarAtom;
    use GrammarAtom::*;
    use constructors::*;
    use GrammarNode;

    fn tt(s: &str) -> GrammarAtom {
        Text(s.to_string())
    }

    #[test]
    fn text() {
        assert_eq!(t("foo").initial(), vec![tt("foo")]);
    }

    #[test]
    fn range() {
        assert_eq!(r('a', 'z').initial(), vec![Range('a', 'z')]);
    }

    #[test]
    fn anything() {
        assert_eq!(a().initial(), vec![Anything]);
    }

    #[test]
    fn seq() {
        assert_eq!(
            e(vec![
                t("foo"),
            ]).initial(),
            vec![tt("foo")],
        );
        assert_eq!(
            e(vec![
                t("foo"),
                t("bar"),
            ]).initial(),
            vec![tt("foo")],
        );
        assert_eq!(
            e(vec![
                e(vec![
                    t("foo"),
                ]),
                t("bar"),
            ]).initial(),
            vec![tt("foo")],
        );
    }

    #[test]
    fn choice() {
        assert_eq!(
            c(vec![
                t("foo")
            ]).initial(),
            vec![tt("foo")],
        );
        assert_eq!(
            c(vec![
                t("foo"),
                t("bar")
            ]).initial(),
            vec![tt("foo"), tt("bar)],
        );
        assert_eq!(
            c(vec![
                c(vec![
                    t("foo"),
                ]),
                t("bar"),
            ]).initial(),
            vec![tt("foo"), tt("bar")],
        );
    }

    #[test]
    fn choice() {
        assert_eq!(
        );
    }

    #[test]
    fn choice() {
        assert_eq!(
        );
    }

    #[test]
    fn choice() {
        assert_eq!(
        );
    }

    #[test]
    fn choice() {
        assert_eq!(
        );
    }

    #[test]
    fn choice() {
        assert_eq!(
        );
    }

    #[test]
    fn choice() {
        assert_eq!(
        );
    }

    #[test]
    fn choice() {
        assert_eq!(
        );
    }
}

use prelude::*;

mod tree_tests {
    use std::ptr;
    use tree::{Down, TreeCursor};

    struct Node {
        children: Vec<Node>,
    }

    impl Down for Node {
        fn down(&mut self, idx: usize) -> Option<*mut Self> {
            self.children.get_mut(idx).map(|c: &mut Self| c as *mut Self)
        }
    }

    fn e(children: Vec<Node>) -> Node {
        Node { children }
    }

    fn f() -> Node {
        Node { children: Vec::new() }
    }

    #[test]
    fn full_traverse() {
        let mut t = e(vec![
            e(vec![
                f(),
                f(),
            ]),
            e(vec![
                f(),
            ]),
        ]);
        let root = &t as *const Node;

        let mut c = TreeCursor::new(&mut t);

        for _ in 0..100 {
            assert!(ptr::eq(c.get(), root));

            assert!(c.down());

            {
                let here = c.get() as *const Node;
                let here_mut = c.get_mut() as *mut Node;
                assert!(ptr::eq(here, here_mut));
                assert!(!ptr::eq(here, root));

                assert!(c.down());
                assert!(!ptr::eq(c.get(), here));
                assert!(!c.down());
                assert!(c.up());

                assert!(ptr::eq(c.get(), here));
                assert!(ptr::eq(c.get_mut(), here_mut));
            }

            assert!(c.down());
            assert!(!c.down());
            assert!(c.up());

            assert!(!c.down());
            assert!(c.up());

            assert!(c.down());

            assert!(c.down());
            assert!(!c.down());
            assert!(c.up());

            assert!(!c.down());
            assert!(c.up());

            assert!(!c.down());
            assert!(!c.up());
        }
    }

    #[test]
    fn partial_traverse() {
        let mut t = e(vec![
            e(vec![
                e(vec![
                    f(),
                ]),
                f(),
            ]),
            e(vec![
                f(),
                f(),
            ]),
        ]);
        let root = &t as *const Node;

        let mut c = TreeCursor::new(&mut t);

        for _ in 0..100 {
            assert!(ptr::eq(c.get(), root));

            assert!(c.down());

            {
                let here = c.get() as *const Node;
                let here_mut = c.get_mut() as *mut Node;
                assert!(ptr::eq(here, here_mut));
                assert!(!ptr::eq(here, root));

                assert!(c.down());
                assert!(!ptr::eq(c.get(), here));
                assert!(c.up());

                assert!(ptr::eq(c.get(), here));
                assert!(ptr::eq(c.get_mut(), here_mut));
            }

            assert!(c.down());
            assert!(c.up());
            assert!(c.up());

            assert!(c.down());

            assert!(c.down());
            assert!(c.up());
            assert!(c.up());
            assert!(!c.up());
        }
    }

    #[test]
    fn add_children() {
        let mut t = e(vec![]);

        let mut c = TreeCursor::new(&mut t);

        for _ in 0..10 {
            assert!(!c.down());

            let here = c.get_mut();
            here.children.push(e(vec![]));

            assert!(c.down());
        }

        for _ in 0..10 {
            assert!(!c.down());
            assert!(c.up());
        }

        assert!(!c.up());
    }

    #[test]
    fn remove_children() {
        let mut t = e(vec![
            e(vec![
                e(vec![
                    e(vec![
                        e(vec![
                            e(vec![
                                e(vec![]),
                            ]),
                        ]),
                    ]),
                ]),
            ]),
        ]);

        let mut c = TreeCursor::new(&mut t);

        for _ in 0..6 {
            assert!(c.down());
        }
        assert!(!c.down());
        for _ in 0..6 {
            assert!(c.up());
        }
        assert!(!c.up());

        for i in (1..=5).rev() {
            for _ in 0..i {
                assert!(c.down());
            }
            let here = c.get_mut();
            assert!(here.children.pop().is_some());
            assert!(!c.down());
            for _ in 0..i {
                assert!(c.up());
            }
            assert!(!c.up());
        }
    }
}

mod link_tree_tests {
    use std::ptr;
    use tree::{Down, Link, LinkError, LinkTreeCursor};

    #[derive(Debug)]
    enum Node {
        Seq(Vec<Node>),
        Name(String, Box<Node>),
        Link(String),
    }

    impl Down for Node {
        fn down(&mut self, idx: usize) -> Option<*mut Self> {
            match self {
                &mut Node::Seq(ref mut children) =>
                    children.get_mut(idx).map(|c: &mut Self| c as *mut Self),
                &mut Node::Name(_, ref mut child) => {
                    if idx == 0 {
                        Some(&mut **child as *mut Self)
                    } else {
                        None
                    }
                },
                &mut Node::Link(_) => None,
            }
        }
    }

    impl Link for Node {
        fn name(&self) -> Option<&str> {
            match self {
                &Node::Name(ref name, _) => Some(name),
                _ => None,
            }
        }

        fn target(&self) -> Option<&str> {
            match self {
                &Node::Link(ref target) => Some(target),
                _ => None,
            }
        }
    }

    fn e(children: Vec<Node>) -> Node {
        Node::Seq(children)
    }

    fn n(name: &str, child: Node) -> Node {
        Node::Name(name.to_string(), Box::new(child))
    }

    fn k(target: &str) -> Node {
        Node::Link(target.to_string())
    }

    fn f() -> Node {
        Node::Seq(Vec::new())
    }

    #[test]
    fn full_traverse() {
        let mut t = e(vec![
            f(),
            f(),
            f(),
            n("go", e(vec![
                n("foo", e(vec![
                    f(),
                    f(),
                ])),
                e(vec![
                    k("foo"),
                ]),
            ])),
        ]);

        let mut c = LinkTreeCursor::new(&mut t, "go").unwrap();

        for _ in 0..100 {
            let start = c.get() as *const Node;

            assert!(c.down());
            assert!(c.down());
            assert!(c.down());

            {
                let here = c.get() as *const Node;
                let here_mut = c.get_mut() as *mut Node;
                assert!(ptr::eq(here, here_mut));
                assert!(!ptr::eq(here, start));

                assert!(c.down());
                assert!(!ptr::eq(c.get(), here));
                assert!(!c.down());
                assert!(c.up());

                assert!(ptr::eq(c.get(), here));
                assert!(ptr::eq(c.get_mut(), here_mut));
            }

            assert!(c.down());
            assert!(!c.down());
            assert!(c.up());

            assert!(!c.down());
            assert!(c.up());

            assert!(!c.down());
            assert!(c.up());

            assert!(c.down());
            assert!(c.down());
            assert!(c.down());
            assert!(c.down());
            assert!(c.down());
            assert!(!c.down());
            assert!(c.up());

            assert!(c.down());
            assert!(!c.down());
            assert!(c.up());

            assert!(!c.down());
            assert!(c.up());

            assert!(!c.down());
            assert!(c.up());

            assert!(!c.down());
            assert!(c.up());

            assert!(!c.down());
            assert!(c.up());

            assert!(!c.down());
            assert!(c.up());

            assert!(!c.down());
            assert!(!c.up());

            assert!(ptr::eq(c.get(), start));
        }
    }

    #[test]
    fn deep_recursion() {
        let mut t = n("foo", k("foo"));

        let mut c = LinkTreeCursor::new(&mut t, "foo").unwrap();

        let nn = c.get() as *const Node;
        assert!(c.down());
        let kk = c.get() as *const Node;
        assert!(c.down());

        for _ in 0..1000 {
            assert!(ptr::eq(c.get(), nn));
            assert!(c.down());
            assert!(ptr::eq(c.get(), kk));
            assert!(c.down());
        }

        for _ in 0..1001 {
            assert!(ptr::eq(c.get(), nn));
            assert!(c.up());
            assert!(!c.down());
            assert!(ptr::eq(c.get(), kk));
            assert!(c.up());
            assert!(!c.down());
        }

        assert!(!c.up());
    }

    #[test]
    fn zero() {
        let mut t = n("foo", e(vec![
            f(),
            f(),
        ]));

        let mut c = LinkTreeCursor::new(&mut t, "foo").unwrap();

        assert!(c.down());

        assert!(c.down());
        assert!(!c.down());
        assert!(c.up());

        assert!(c.down());
        assert!(!c.down());
        assert!(c.up());

        assert!(!c.down());

        c.zero();

        assert!(c.down());
        assert!(!c.down());
        assert!(c.up());

        assert!(c.down());
        assert!(!c.down());
        assert!(c.up());

        c.zero();

        assert!(c.down());
        assert!(!c.down());
        assert!(c.up());

        assert!(c.down());
        assert!(!c.down());
        assert!(c.up());

        assert!(!c.down());

        assert!(c.up());
        assert!(!c.up());
    }

    #[test]
    fn link_errors() {
        {
            let mut t = e(vec![
                k("foo"),
                n("bar", f()),
            ]);
            assert_eq!(
                LinkTreeCursor::new(&mut t, "bar").unwrap_err(),
                LinkError::BrokenLink,
            );
        }

        {
            let mut t = e(vec![
                n("bar", e(vec![
                    e(vec![
                        f(),
                        f(),
                        k("foo"),
                    ]),
                ])),
            ]);
            assert_eq!(
                LinkTreeCursor::new(&mut t, "bar").unwrap_err(),
                LinkError::BrokenLink,
            );
        }

        {
            let mut t = e(vec![
                f(),
                n("foo", n("foo", f())),
                n("bar", f()),
            ]);
            assert_eq!(
                LinkTreeCursor::new(&mut t, "foo").unwrap_err(),
                LinkError::DuplicateName,
            );
            assert_eq!(
                LinkTreeCursor::new(&mut t, "bar").unwrap_err(),
                LinkError::DuplicateName,
            );
        }

        {
            let mut t = n("foo", f());
            assert_eq!(
                LinkTreeCursor::new(&mut t, "bar").unwrap_err(),
                LinkError::BrokenLink,
            );
        }
    }

    #[test]
    fn link_error_precedence() {
        {
            let mut t = n("foo", n("foo", f()));
            assert_eq!(
                LinkTreeCursor::new(&mut t, "bar").unwrap_err(),
                LinkError::DuplicateName,
            );
        }

        {
            let mut t = e(vec![
                n("foo", f()),
                f(),
                n("foo", f()),
            ]);
            assert_eq!(
                LinkTreeCursor::new(&mut t, "bar").unwrap_err(),
                LinkError::DuplicateName,
            );
        }
    }
}

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
fn optional() {
    let mut g = e(vec![
        n("start", e(vec![
            q(t("-")),
            q(t("+")),
        ])),
    ]);

    assert!(g.parse("start", "-+").is_ok());
    assert!(g.parse("start", "-").is_ok());
    assert!(g.parse("start", "+").is_ok());
    assert!(g.parse("start", "").is_ok());
    assert!(g.parse("start", "+-").is_err());
    assert!(g.parse("start", " ").is_err());
    assert!(g.parse("start", "+0").is_err());
    assert!(g.parse("start", "0+").is_err());
}

#[test]
fn weirdness() {
    let mut g = e(vec![
        n("start", e(vec![
            t("a"),
            n("bee", t("b")),
        ])),
    ]);

    assert!(g.parse("start", "ab").is_ok());
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

    assert!(g.parse("dec_int", "0").is_ok());
    assert!(g.parse("dec_int", "1").is_ok());
    assert!(g.parse("dec_int", "9").is_ok());
    assert!(g.parse("dec_int", "10").is_ok());
    assert!(g.parse("dec_int", "19").is_ok());
    assert!(g.parse("dec_int", "99").is_ok());
    assert!(g.parse("dec_int", "-0").is_ok());
    assert!(g.parse("dec_int", "-1").is_ok());
    assert!(g.parse("dec_int", "-9").is_ok());
    assert!(g.parse("dec_int", "-10").is_ok());
    assert!(g.parse("dec_int", "-19").is_ok());
    assert!(g.parse("dec_int", "-99").is_ok());

    assert!(g.parse("dec_int", "y").is_err());
    assert!(g.parse("dec_int", "-").is_err());
    assert!(g.parse("dec_int", "0-").is_err());
    assert!(g.parse("dec_int", "1-").is_err());
    assert!(g.parse("dec_int", "01").is_err());
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

    assert!(g.parse("expr", "1").is_ok());
    assert!(g.parse("expr", "1+1").is_ok());
    assert!(g.parse("expr", "(1+1+1)+1").is_ok());
    assert!(g.parse("expr", "1+(1+1+1)").is_ok());
}

#[test]
fn range() {
    let mut g = n("start", e(vec![
        r('a', 'd'),
        t("e"),
    ]));

    assert!(g.parse("start", "ae").is_ok());
    assert!(g.parse("start", "be").is_ok());
    assert!(g.parse("start", "ce").is_ok());
    assert!(g.parse("start", "de").is_ok());

    assert!(g.parse("start", "a").is_err());
    assert!(g.parse("start", "e").is_err());
    assert!(g.parse("start", "ee").is_err());
    assert!(g.parse("start", "ea").is_err());
}

#[test]
fn lookahead() {
    let mut g = n("start", e(vec![
        z(t("a")),
        r('a', 'c'),
        g(t("a")),
        r('a', 'c'),
    ]));

    assert!(g.parse("start", "ab").is_ok());
    assert!(g.parse("start", "ac").is_ok());

    assert!(g.parse("start", "aa").is_err());
    assert!(g.parse("start", "ba").is_err());
    assert!(g.parse("start", "bc").is_err());
    assert!(g.parse("start", ".c").is_err());
    assert!(g.parse("start", "a").is_err());
    assert!(g.parse("start", "b").is_err());
    assert!(g.parse("start", "").is_err());
}

#[test]
fn ident() {
    let mut g = get_grammar_grammar();

    assert!(g.parse("ident", "a").is_ok());
    assert!(g.parse("ident", "A").is_ok());
    assert!(g.parse("ident", "_").is_ok());
    assert!(g.parse("ident", "foo").is_ok());
    assert!(g.parse("ident", "foo_bar").is_ok());
    assert!(g.parse("ident", "_foo_bar_").is_ok());
    assert!(g.parse("ident", "a3").is_ok());
    assert!(g.parse("ident", "_3").is_ok());

    assert!(g.parse("ident", "3").is_err());
    assert!(g.parse("ident", "3a").is_err());
    assert!(g.parse("ident", "3_").is_err());
}

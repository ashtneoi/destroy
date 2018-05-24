mod tree_tests {
    use std::ptr;
    use tree::prelude::*;

    struct Node {
        children: Vec<Node>,
    }

    impl DownMut for Node {
        fn down_mut(&mut self, idx: usize) -> Option<&mut Self> {
            self.children.get_mut(idx)
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

        let mut c = MutTreeCursor::new(&mut t);

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

        let mut c = MutTreeCursor::new(&mut t);

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

        let mut c = MutTreeCursor::new(&mut t);

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

        let mut c = MutTreeCursor::new(&mut t);

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
    use tree::prelude::*;

    #[derive(Debug)]
    enum Node {
        Seq(Vec<Node>),
        Name(String, Box<Node>),
        Link(String),
    }

    impl Down for Node {
        fn down(&self, idx: usize) -> Option<&Self> {
            match self {
                &Node::Seq(ref children) => children.get(idx),
                &Node::Name(_, ref child) => {
                    if idx == 0 {
                        Some(child.as_ref())
                    } else {
                        None
                    }
                },
                &Node::Link(_) => None,
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
    fn nested_names() {
        let t = n("foo", n("bar", n("fuzz", f())));

        LinkTreeCursor::new(&t, "foo").unwrap();
        LinkTreeCursor::new(&t, "bar").unwrap();
        LinkTreeCursor::new(&t, "fuzz").unwrap();

        LinkTreeCursor::new(&t, "zap").unwrap_err();
    }

    #[test]
    fn full_traverse() {
        let t = e(vec![
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

        let mut c = LinkTreeCursor::new(&t, "go").unwrap();

        for _ in 0..100 {
            let start = c.get() as *const Node;

            assert!(c.down());
            assert!(c.down());

            {
                let here = c.get() as *const Node;
                assert!(!ptr::eq(here, start));

                assert!(c.down());
                assert!(!ptr::eq(c.get(), here));
                assert!(!c.down());
                assert!(c.up());

                assert!(ptr::eq(c.get(), here));
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
            assert!(!c.up());

            assert!(ptr::eq(c.get(), start));
        }
    }

    #[test]
    fn deep_recursion() {
        let t = n("foo", e(vec![
            k("foo")
        ]));

        let mut c = LinkTreeCursor::new(&t, "foo").unwrap();

        let ee = c.get() as *const Node;
        assert!(c.down());
        let kk = c.get() as *const Node;
        assert!(c.down());

        for _ in 0..1000 {
            assert!(ptr::eq(c.get(), ee));
            assert!(c.down());
            assert!(ptr::eq(c.get(), kk));
            assert!(c.down());
        }

        for _ in 0..1001 {
            assert!(ptr::eq(c.get(), ee));
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
        let t = n("foo", e(vec![
            f(),
            f(),
        ]));

        let mut c = LinkTreeCursor::new(&t, "foo").unwrap();

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
        assert!(!c.up());
    }

    #[test]
    fn link_errors() {
        {
            let t = e(vec![
                k("foo"),
                n("bar", f()),
            ]);
            assert_eq!(
                LinkTreeCursor::new(&t, "bar").unwrap_err(),
                LinkError::BrokenLink,
            );
        }

        {
            let t = e(vec![
                n("bar", e(vec![
                    e(vec![
                        f(),
                        f(),
                        k("foo"),
                    ]),
                ])),
            ]);
            assert_eq!(
                LinkTreeCursor::new(&t, "bar").unwrap_err(),
                LinkError::BrokenLink,
            );
        }

        {
            let t = e(vec![
                f(),
                n("foo", n("foo", f())),
                n("bar", f()),
            ]);
            assert_eq!(
                LinkTreeCursor::new(&t, "foo").unwrap_err(),
                LinkError::DuplicateName,
            );
            assert_eq!(
                LinkTreeCursor::new(&t, "bar").unwrap_err(),
                LinkError::DuplicateName,
            );
        }

        {
            let t = n("foo", f());
            assert_eq!(
                LinkTreeCursor::new(&t, "bar").unwrap_err(),
                LinkError::BrokenLink,
            );
        }
    }

    #[test]
    fn link_error_precedence() {
        {
            let t = n("foo", n("foo", f()));
            assert_eq!(
                LinkTreeCursor::new(&t, "bar").unwrap_err(),
                LinkError::DuplicateName,
            );
        }

        {
            let t = e(vec![
                n("foo", f()),
                f(),
                n("foo", f()),
            ]);
            assert_eq!(
                LinkTreeCursor::new(&t, "bar").unwrap_err(),
                LinkError::DuplicateName,
            );
        }
    }
}

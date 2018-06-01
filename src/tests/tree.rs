mod link_tree_tests {
    use std::ptr;
    use tree::{Link, LinkError, LinkTreeCursor};
    use tree_cursor::prelude::*;

    #[derive(Debug)]
    enum Node {
        Seq(Vec<Node>),
        Link(String),
    }

    impl Down for Node {
        fn down(&self, idx: usize) -> Option<&Self> {
            match self {
                &Node::Seq(ref children) => children.get(idx),
                &Node::Link(_) => None,
            }
        }
    }

    impl Link for Node {
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

    fn k(target: &str) -> Node {
        Node::Link(target.to_string())
    }

    fn f() -> Node {
        Node::Seq(Vec::new())
    }

    #[test]
    fn deep_recursion() {
        let named = &[
            ("foo", e(vec![
                k("foo")
            ])),
        ];

        let mut c = LinkTreeCursor::new(named, "foo").unwrap();

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
        let named = &[
            ("foo", e(vec![
                f(),
                f(),
            ])),
        ];

        let mut c = LinkTreeCursor::new(named, "foo").unwrap();

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
            let named = &[
                ("bar", f()),
                ("six", k("foo")),
            ];
            assert_eq!(
                LinkTreeCursor::new(named, "bar").unwrap_err(),
                LinkError::BrokenLink("foo".to_string()),
            );
        }

        {
            let named = &[
                ("bar", e(vec![
                    e(vec![
                        f(),
                        f(),
                        k("foo"),
                    ]),
                ])),
            ];
            assert_eq!(
                LinkTreeCursor::new(named, "bar").unwrap_err(),
                LinkError::BrokenLink("foo".to_string()),
            );
        }

        {
            let named = &[
                ("foo", f()),
                ("foo", f()),
                ("bar", f()),
            ];
            assert_eq!(
                LinkTreeCursor::new(named, "foo").unwrap_err(),
                LinkError::DuplicateName("foo".to_string()),
            );
            assert_eq!(
                LinkTreeCursor::new(named, "bar").unwrap_err(),
                LinkError::DuplicateName("foo".to_string()),
            );
        }

        {
            let named = &[
                ("foo", f()),
            ];
            assert_eq!(
                LinkTreeCursor::new(named, "bar").unwrap_err(),
                LinkError::BrokenLink("bar".to_string()),
            );
        }
    }

    #[test]
    fn link_error_precedence() {
        {
            let named = &[
                ("foo", f()),
                ("foo", f()),
            ];
            assert_eq!(
                LinkTreeCursor::new(named, "bar").unwrap_err(),
                LinkError::DuplicateName("foo".to_string()),
            );
        }

        {
            let named = &[
                ("foo", e(vec![
                    f()
                ])),
                ("foo", f()),
            ];
            assert_eq!(
                LinkTreeCursor::new(named, "bar").unwrap_err(),
                LinkError::DuplicateName("foo".to_string()),
            );
        }
    }
}

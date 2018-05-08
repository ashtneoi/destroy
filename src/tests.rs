use collections::{
    build_link_map,
    Down,
    Link,
    TreeCursor,
};
use std::ops::Index;
use std::ptr;

#[derive(Debug)]
enum Node {
    Group(Vec<Node>),
    Name(String, Box<Node>),
    Link(String),
    Leaf,
}

impl Index<usize> for Node {
    type Output = Node;

    fn index(&self, index: usize) -> &Node {
        self.down(index).expect("Index out of range")
    }
}

impl Down for Node {
    fn down(&self, idx: usize) -> Option<&Self> {
        match self {
            &Node::Group(ref nodes) => nodes.get(idx),
            &Node::Name(_, ref node) => {
                if idx == 0 {
                    Some(node)
                } else {
                    None
                }
            },
            _ => None,
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

fn g(children: Vec<Node>) -> Node {
    Node::Group(children)
}

fn n(name: &str, child: Node) -> Node {
    Node::Name(name.to_string(), Box::new(child))
}

fn k(target: &str) -> Node {
    Node::Link(target.to_string())
}

fn f() -> Node {
    Node::Leaf
}

trait TreeCursorAssertions {
    type Node;

    fn assert_up(&mut self, node: &Self::Node);
    fn assert_up_fail(&mut self, node: &Self::Node);
    fn assert_down(&mut self, node: &Self::Node);
    fn assert_down_fail(&mut self, node: &Self::Node);
}

impl<'n, N: 'n + Down + Link> TreeCursorAssertions for TreeCursor<'n, N> {
    type Node = N;

    fn assert_up(&mut self, node: &Self::Node) {
        assert!(self.up());
        assert!(ptr::eq(self.get(), node));
    }

    fn assert_up_fail(&mut self, node: &Self::Node) {
        assert!(!self.up());
        assert!(ptr::eq(self.get(), node));
    }

    fn assert_down(&mut self, node: &Self::Node) {
        assert!(self.down());
        assert!(ptr::eq(self.get(), node));
    }

    fn assert_down_fail(&mut self, node: &Self::Node) {
        assert!(!self.down());
        assert!(ptr::eq(self.get(), node));
    }
}

#[test]
fn cursor_no_links() {
    let t = g(vec![
            f(),
            k("Foo"),
            n("Foo", f()),
    ]);

    let mut c = TreeCursor::new(&t, None);
    assert!(ptr::eq(c.get(), &t));

    c.assert_down(&t[0]);
    c.assert_down_fail(&t[0]);
    c.assert_up(&t);

    c.assert_down(&t[1]);
    c.assert_down_fail(&t[1]);
    c.assert_up(&t);

    c.assert_down(&t[2]);

    c.assert_down(&t[2][0]);
    c.assert_down_fail(&t[2][0]);
    c.assert_up(&t[2]);

    c.assert_down_fail(&t[2]);
    c.assert_up(&t);

    c.assert_up_fail(&t);
}

#[test]
fn link_map_result() {
    assert!(build_link_map(&g(vec![
            n("Foo", f()),
            f(),
    ])).is_ok());

    assert!(build_link_map(&g(vec![
            n("Foo", f()),
            k("Foo"),
    ])).is_ok());

    assert!(build_link_map(&g(vec![
            k("Foo"),
            n("Foo", f()),
    ])).is_ok());

    assert!(build_link_map(&g(vec![
            n("Foo", g(vec![
                k("Foo"),
            ]))
    ])).is_ok());

    assert!(build_link_map(&g(vec![
            n("Foo", f()),
            k("Buzz"),
    ])).is_err());

    assert!(build_link_map(&g(vec![
            k("Buzz"),
            n("Foo", f()),
    ])).is_err());

    assert!(build_link_map(&g(vec![
            n("Foo", g(vec![
                k("Buzz"),
            ]))
    ])).is_err());

    assert!(build_link_map(&g(vec![
            n("Foo", g(vec![
                f(),
                n("Foo", g(vec![
                    f(),
                ])),
            ])),
    ])).is_err());

    assert!(build_link_map(&g(vec![
            n("Foo", g(vec![
                f(),
            ])),
            n("Foo", g(vec![
                f(),
            ])),
    ])).is_err());
}

#[test]
fn cursor_links() {
    let t = g(vec![
            f(),
            k("Foo"),
            n("Foo", f()),
    ]);

    let mut c = TreeCursor::new(&t, Some(build_link_map(&t).unwrap()));
    assert!(ptr::eq(c.get(), &t));

    c.assert_down(&t[0]);
    c.assert_down_fail(&t[0]);
    c.assert_up(&t);

    c.assert_down(&t[1]);

    c.assert_down(&t[2]);

    c.assert_down(&t[2][0]);
    c.assert_down_fail(&t[2][0]);
    c.assert_up(&t[2]);

    c.assert_up(&t[1]);

    c.assert_up(&t);

    c.assert_down(&t[2]);

    c.assert_down(&t[2][0]);
    c.assert_down_fail(&t[2][0]);
    c.assert_up(&t[2]);

    c.assert_down_fail(&t[2]);
    c.assert_up(&t);

    c.assert_up_fail(&t);
}

#[test]
fn cursor_irregular_walk() {
    let t = g(vec![
        f(),
        g(vec![
            g(vec![
                f(),
            ]),
            f(),
        ]),
    ]);

    let mut c = TreeCursor::new(&t, None);
    assert!(ptr::eq(c.get(), &t));

    c.assert_down(&t[0]);
    c.assert_down_fail(&t[0]);
    c.assert_down_fail(&t[0]);
    c.assert_down_fail(&t[0]);
    c.assert_up(&t);

    c.assert_up_fail(&t);
    c.assert_up_fail(&t);

    c.assert_down(&t[0]);
    c.assert_up(&t);

    c.assert_down(&t[1]);
    c.assert_down(&t[1][0]);
    c.assert_up(&t[1]);

    c.assert_up(&t);
    c.assert_up_fail(&t);
    c.assert_up_fail(&t);
    c.assert_up_fail(&t);
    c.assert_up_fail(&t);
}

#[test]
fn cursor_deep_recursion() {
    let t = n("flower", k("flower"));

    let mut c = TreeCursor::new(&t, Some(build_link_map(&t).unwrap()));

    let limit = 100000;

    for _ in 0..limit {
        c.assert_down(&t[0]);
        c.assert_down(&t);
    }
    for _ in 0..limit {
        c.assert_up(&t[0]);
        c.assert_up(&t);
    }

    c.assert_up_fail(&t);
}

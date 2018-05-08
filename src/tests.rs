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

fn e(name: &str, child: Node) -> Node {
    Node::Name(name.to_string(), Box::new(child))
}

fn k(target: &str) -> Node {
    Node::Link(target.to_string())
}

fn f() -> Node {
    Node::Leaf
}

#[test]
fn cursor_no_links() {
    let t = g(vec![
            f(),
            k("Foo"),
            e("Foo", f()),
    ]);

    let mut c = TreeCursor::new(&t, None);
    assert!(ptr::eq(c.get(), &t));

    assert!(c.down());
    assert!(ptr::eq(c.get(), &t[0]));
    assert!(!c.down());
    assert!(ptr::eq(c.get(), &t[0]));
    assert!(c.up());
    assert!(ptr::eq(c.get(), &t));

    assert!(c.down());
    assert!(ptr::eq(c.get(), &t[1]));
    assert!(!c.down());
    assert!(ptr::eq(c.get(), &t[1]));
    assert!(c.up());
    assert!(ptr::eq(c.get(), &t));

    assert!(c.down());
    assert!(ptr::eq(c.get(), &t[2]));

    assert!(c.down());
    assert!(ptr::eq(c.get(), &t[2][0]));
    assert!(!c.down());
    assert!(ptr::eq(c.get(), &t[2][0]));
    assert!(c.up());
    assert!(ptr::eq(c.get(), &t[2]));

    assert!(!c.down());
    assert!(ptr::eq(c.get(), &t[2]));
    assert!(c.up());
    assert!(ptr::eq(c.get(), &t));

    assert!(!c.up());
    assert!(ptr::eq(c.get(), &t));
}

#[test]
fn link_map_result() {
    assert!(build_link_map(&g(vec![
            e("Foo", f()),
            f(),
    ])).is_ok());

    assert!(build_link_map(&g(vec![
            e("Foo", f()),
            k("Foo"),
    ])).is_ok());

    assert!(build_link_map(&g(vec![
            k("Foo"),
            e("Foo", f()),
    ])).is_ok());

    assert!(build_link_map(&g(vec![
            e("Foo", g(vec![
                k("Foo"),
            ]))
    ])).is_ok());

    assert!(build_link_map(&g(vec![
            e("Foo", f()),
            k("Buzz"),
    ])).is_err());

    assert!(build_link_map(&g(vec![
            k("Buzz"),
            e("Foo", f()),
    ])).is_err());

    assert!(build_link_map(&g(vec![
            e("Foo", g(vec![
                k("Buzz"),
            ]))
    ])).is_err());

    assert!(build_link_map(&g(vec![
            e("Foo", g(vec![
                f(),
                e("Foo", g(vec![
                    f(),
                ])),
            ])),
    ])).is_err());

    assert!(build_link_map(&g(vec![
            e("Foo", g(vec![
                f(),
            ])),
            e("Foo", g(vec![
                f(),
            ])),
    ])).is_err());
}

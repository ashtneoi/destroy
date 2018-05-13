extern crate neoilib;

#[cfg(test)]
mod tests;

mod prelude {
    pub use {e, c, s, p, q, z, g, n, k, t};
    pub use GrammarNode;
}

use neoilib::tree::{Down, Link, LinkTreeCursor};

pub enum GrammarNode {
    Seq(Vec<GrammarNode>),
    Choice(Vec<GrammarNode>),
    Star(Box<GrammarNode>),
    Plus(Box<GrammarNode>),
    Opt(Box<GrammarNode>),
    Pos(Box<GrammarNode>),
    Neg(Box<GrammarNode>),
    Name(String, Box<GrammarNode>),
    Link(String),
    Text(String),
}

impl Down for GrammarNode {
    fn down(&mut self, idx: usize) -> Option<*mut Self> {
        use GrammarNode::*;
        match self {
            &mut Seq(ref mut children)
            | &mut Choice(ref mut children) => {
                children.get_mut(idx).map(|c| c as *mut Self)
            },
            &mut Star(ref mut child)
            | &mut Plus(ref mut child)
            | &mut Opt(ref mut child)
            | &mut Pos(ref mut child)
            | &mut Neg(ref mut child)
            | &mut Name(_, ref mut child) => {
                if idx == 0 {
                    Some(child.as_mut())
                } else {
                    None
                }
            },
            &mut Link(_)
            | &mut Text(_) => None,
        }
    }
}

impl Link for GrammarNode {
    fn name(&self) -> Option<&str> {
        match self {
            &GrammarNode::Name(ref n, _) => Some(n),
            _ => None,
        }
    }

    fn target(&self) -> Option<&str> {
        match self {
            &GrammarNode::Link(ref t) => Some(t),
            _ => None,
        }
    }
}

pub fn e(children: Vec<GrammarNode>) -> GrammarNode {
    GrammarNode::Seq(children)
}

pub fn c(children: Vec<GrammarNode>) -> GrammarNode {
    GrammarNode::Choice(children)
}

pub fn s(child: GrammarNode) -> GrammarNode {
    GrammarNode::Star(Box::new(child))
}

pub fn p(child: GrammarNode) -> GrammarNode {
    GrammarNode::Plus(Box::new(child))
}

pub fn q(child: GrammarNode) -> GrammarNode {
    GrammarNode::Opt(Box::new(child))
}

pub fn z(child: GrammarNode) -> GrammarNode {
    GrammarNode::Pos(Box::new(child))
}

pub fn g(child: GrammarNode) -> GrammarNode {
    GrammarNode::Neg(Box::new(child))
}

pub fn n(name: &str, child: GrammarNode) -> GrammarNode {
    GrammarNode::Name(name.to_string(), Box::new(child))
}

pub fn k(target: &str) -> GrammarNode {
    GrammarNode::Link(target.to_string())
}

pub fn t(text: &str) -> GrammarNode {
    GrammarNode::Text(text.to_string())
}

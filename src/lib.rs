#![feature(nll)]

mod collections;

#[cfg(test)]
mod tests;

mod prelude {
    pub use {e, c, s, p, q, z, g, n, k, t};
    pub use GrammarNode;
}

use collections::{Down, Link};

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
    fn down(&self, idx: usize) -> Option<&Self> {
        match self {
            &GrammarNode::Seq(ref children)
            | &GrammarNode::Choice(ref children) => children.get(idx),
            &GrammarNode::Star(ref child)
            | &GrammarNode::Plus(ref child)
            | &GrammarNode::Opt(ref child)
            | &GrammarNode::Pos(ref child)
            | &GrammarNode::Neg(ref child) => {
                if idx == 0 {
                    Some(child)
                } else {
                    None
                }
            },
            _ => None,
        }
    }
}

impl Link for GrammarNode {
    fn name(&self) -> Option<&str> {
        match self {
            &GrammarNode::Name(ref name, _) => Some(name),
            _ => None,
        }
    }

    fn target(&self) -> Option<&str> {
        match self {
            &GrammarNode::Link(ref target) => Some(target),
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

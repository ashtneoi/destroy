use GrammarAtom;
use GrammarNode;

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

pub fn u(name: &str, child: GrammarNode) -> GrammarNode {
    GrammarNode::Group(name.to_string(), Box::new(child))
}

pub fn x(child: GrammarNode) -> GrammarNode {
    GrammarNode::Erase(Box::new(child))
}

pub fn k(target: &str) -> GrammarNode {
    GrammarNode::Link(target.to_string())
}

pub fn r(from: char, to: char) -> GrammarNode {
    GrammarNode::Atom(GrammarAtom::Range(from, to))
}

pub fn t(text: &str) -> GrammarNode {
    GrammarNode::Atom(GrammarAtom::Text(text.to_string()))
}

pub fn a() -> GrammarNode {
    GrammarNode::Atom(GrammarAtom::Anything)
}

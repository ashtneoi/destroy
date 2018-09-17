use GrammarAtom;
use GrammarNode;
use string_table::StringTable;

pub fn e<'i>(children: Vec<GrammarNode<'i>>) -> GrammarNode<'i> {
    GrammarNode::Seq(children)
}

pub fn c<'i>(children: Vec<GrammarNode<'i>>) -> GrammarNode<'i> {
    GrammarNode::Choice(children)
}

pub fn s<'i>(child: GrammarNode<'i>) -> GrammarNode<'i> {
    GrammarNode::Star(Box::new(child))
}

pub fn p<'i>(child: GrammarNode<'i>) -> GrammarNode<'i> {
    GrammarNode::Plus(Box::new(child))
}

pub fn q<'i>(child: GrammarNode<'i>) -> GrammarNode<'i> {
    GrammarNode::Opt(Box::new(child))
}

pub fn z<'i>(child: GrammarNode<'i>) -> GrammarNode<'i> {
    GrammarNode::Pos(Box::new(child))
}

pub fn n<'i>(child: GrammarNode<'i>) -> GrammarNode<'i> {
    GrammarNode::Neg(Box::new(child))
}

pub fn u<'i>(name: &str, child: GrammarNode<'i>) -> GrammarNode<'i> {
    GrammarNode::Group(name.to_string(), Box::new(child))
}

pub fn x<'i>(child: GrammarNode<'i>) -> GrammarNode<'i> {
    GrammarNode::Erase(Box::new(child))
}

pub fn k<'i>(target: &str) -> GrammarNode<'i> {
    GrammarNode::Link(target.to_string())
}

pub fn r<'i>(from: char, to: char) -> GrammarNode<'i> {
    GrammarNode::Atom(GrammarAtom::Range(from, to))
}

pub fn t<'i>(tab: &mut StringTable<'i>, text: &str) -> GrammarNode<'i> {
    GrammarNode::Atom(GrammarAtom::Text(tab.insert(text.to_string())))
}

pub fn a<'i>() -> GrammarNode<'i> {
    GrammarNode::Atom(GrammarAtom::Anything)
}

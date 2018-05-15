extern crate neoilib;

#[cfg(test)]
mod tests;

pub mod prelude {
    pub use {e, c, s, p, q, z, g, n, k, t};
    pub use GrammarNode;
}

use neoilib::tree::{
    Down,
    Link,
    LinkError,
    LinkTreeCursor,
    TreeCursor
};
use std::collections::HashMap;
use std::fmt::{Debug, Formatter, self};

pub struct MatchNode {
    child: Option<Box<MatchNode>>,
    st: Option<STNode>,
}

impl Down for MatchNode {
    fn down(&mut self, _idx: usize) -> Option<*mut Self> {
        self.child = Some(Box::new(Self::new()));
        Some(self.child.as_mut().unwrap().as_mut())
    }
}

impl MatchNode {
    fn new() -> Self {
        Self { child: None, st: None }
    }
}

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

struct Action {
    down: bool,
    zero: bool,
    keep: bool,
    success: bool,
}

fn act(down: bool, zero: bool, keep: bool, success: bool) -> Action {
    Action { down, zero, keep, success }
}

#[derive(Debug)]
pub enum ParseError {
    BadGrammar(LinkError),
    MatchFail,
    UnmatchedInput, // TODO: should this include the syntax tree?
}

impl GrammarNode {
    fn action(&self) -> Action {
        use GrammarNode::*;
        match self {
            &Seq(_) => act(true, false, true, true),
            &Choice(_) => act(false, false, true, true),
            &Star(_)
            | &Plus(_) => act(true, true, true, true),
            &Opt(_) => act(false, false, true, true),
            &Pos(_) => act(false, false, false, true),
            &Neg(_) => act(false, false, false, false),
            &Name(_, _)
            | &Link(_)
            | &Text(_) => act(false, false, true, true),
        }
    }

    fn fail_action(&self) -> Action {
        use GrammarNode::*;
        match self {
            &Plus(_) => act(false, false, true, true),
            _ => self.fail_empty_action(),
        }
    }

    fn fail_empty_action(&self) -> Action {
        use GrammarNode::*;
        match self {
            &Seq(_) => act(false, false, false, false),
            &Choice(_) => act(true, false, false, false),
            &Star(_) => act(false, false, true, true),
            &Plus(_) => act(false, false, false, false),
            &Opt(_) => act(false, false, false, true),
            &Pos(_) => act(false, false, false, false),
            &Neg(_) => act(false, false, false, true),
            &Name(_, _)
            | &Link(_)
            | &Text(_) => act(false, false, false, false),
        }
    }

    pub fn parse(&mut self, start: &str, input: &str)
        -> Result<STNode, ParseError>
    {
        let mut c = LinkTreeCursor::new(self, start)
            .map_err(|le| ParseError::BadGrammar(le))?;

        let mut match_tree = MatchNode::new();
        let mut mc = TreeCursor::new(&mut match_tree);

        let mut pos = 0;

        while c.down() { assert!(mc.down()); }

        loop {
            // Prepare for match.

            let here = c.get();
            mc.get_mut().st = Some(STNode::new((pos, pos)));
            let here_st = mc.get_mut().st.as_mut().unwrap();

            // Match.

            let mut success = match here {
                &GrammarNode::Text(ref t) => {
                    let success = input[pos..].starts_with(t);
                    if success {
                        here_st.raw.1 += t.len();
                    }
                    success
                },
                _ => panic!(),
            };

            // Go up.

            loop {
                // Determine action.

                let a = if success {
                    c.get().action()
                } else if mc.get().st.as_ref()
                        .filter(|st| st.raw.0 < st.raw.1).is_some()
                        // TODO: That's gross.
                {
                    c.get().fail_action()
                } else {
                    c.get().fail_empty_action()
                };

                success = a.success;

                // Take action.

                if a.down {
                    if a.zero {
                        c.zero();
                    }

                    if let Some(ref st) = mc.get().st {
                        pos = st.raw.1;
                    }

                    let mut down = false;
                    while c.down() {
                        assert!(mc.down());
                        down = true;
                    }
                    if down {
                        break;
                    }
                }

                if !c.up() {
                    if success {
                        let st = mc.get_mut().st.take().unwrap();
                        if st.raw.1 < input.len() {
                            return Err(ParseError::UnmatchedInput);
                        }
                        return Ok(st);
                    } else {
                        return Err(ParseError::MatchFail);
                    }
                }
                let maybe_old_st = mc.get_mut().st.take();
                assert!(mc.up());

                if let &mut GrammarNode::Name(ref name, _) = c.get_mut() {
                    // New parent.
                    mc.get_mut().st = Some(
                        STNode::new((pos, pos)) // TODO
                    );
                    let new_st = &mut mc.get_mut().st.as_mut().unwrap();
                    new_st.name = Some(name.to_string());
                }

                if let Some(mut old_st) = maybe_old_st {
                    if a.keep {
                        if let &GrammarNode::Name(_, _) = c.get() {
                            // New parent (already created).
                            let new_st = &mut mc.get_mut().st
                                .as_mut().unwrap();
                            new_st.raw = (old_st.raw.0, old_st.raw.0);
                            if old_st.name.is_none() {
                                // Merge.
                                new_st.extend(&mut old_st);
                            } else {
                                // Insert as child.
                                new_st.insert_child(old_st);
                            }
                        } else {
                            if let Some(ref mut new_st) = mc.get_mut().st {
                                if old_st.name == new_st.name {
                                    // Merge.
                                    new_st.extend(&mut old_st);
                                } else {
                                    // Insert as child.
                                    new_st.insert_child(old_st);
                                }
                            } else {
                                // Bubble up.
                                mc.get_mut().st = Some(old_st);
                            }
                        }
                    } else {
                        pos = old_st.raw.0; // TODO
                    }
                }
            }
        }
    }
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

pub struct STNode {
    raw: (usize, usize),
    name: Option<String>,
    children: Vec<STNode>,
    name_map: HashMap<String, usize>,
}

impl Debug for STNode {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        if let Some(ref n) = self.name {
            write!(f, "{}", n)?;
        }
        write!(f, "({},{})", self.raw.0, self.raw.1)?;
        if !self.children.is_empty() {
            write!(f, ":[")?;
            let mut first = true;
            for child in self.children.iter() {
                if first {
                    first = false;
                } else {
                    write!(f, ", ")?;
                }
                write!(f, "{:?}", child)?;
            }
            write!(f, "]")?;
        }

        Ok(())
    }
}

impl STNode {
    fn new(raw: (usize, usize)) -> STNode {
        STNode {
            raw,
            name: None,
            children: Vec::new(),
            name_map: HashMap::new(),
        }
    }

    fn insert_child(&mut self, child: Self) {
        self.raw.1 = child.raw.1;
        if let Some(ref child_name) = child.name {
            self.name_map.insert(child_name.to_string(), self.children.len());
        }
        self.children.push(child);
    }

    fn extend(&mut self, other: &mut Self) {
        let mut moved = false;
        for child in other.children.drain(..) {
            moved = true;
            self.insert_child(child);
        }
        if moved {
            assert_eq!(self.raw.1, other.raw.1);
        } else {
            self.raw.1 = other.raw.1;
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

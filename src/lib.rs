#![feature(test)]

#[cfg(test)]
extern crate test;

#[cfg(test)]
mod tests;

mod tree;

pub mod prelude {
    pub use {e, c, s, p, q, z, g, n, u, k, r, t};
    pub use get_grammar_grammar;
    pub use GrammarNode;
}

use std::fmt::{Debug, Formatter, self};
use tree::MutVerticalCursorGroup;
use tree::prelude::*;

pub struct MatchCursor<'x> {
    g: LinkTreeCursor<'x, GrammarNode>,
    m: TreeCursor<'x, MatchNode>,
}

impl<'x> MutVerticalCursorGroup for MatchCursor<'x> {
    fn list(&mut self) -> Vec<&mut OpaqueVerticalCursor> {
        vec![&mut self.g, &mut self.m]
    }
}

pub struct MatchNode {
    child: Option<Box<MatchNode>>,
    st: Option<Match>,
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
    Group(String, Box<GrammarNode>),
    Link(String),
    Range(char, char),
    Text(String),
}

#[derive(Copy, Clone)]
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
    MatchFail(usize),
    UnmatchedInput(Match),
}

impl GrammarNode {
    fn try_match(&self, input: &str) -> usize {
        use GrammarNode::*;
        match self {
            &Range(from, to) => {
                if let Some(c) = input.chars().next() {
                    if from <= c && c <= to { 1 } else { 0 }
                } else {
                    0
                }
            },
            &Text(ref t) => if input.starts_with(t) { 1 } else { 0 },
            _ => panic!(),
        }
    }

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
            | &Group(_, _)
            | &Link(_)
            | &Range(_, _)
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
            | &Group(_, _)
            | &Link(_)
            | &Range(_, _)
            | &Text(_) => act(false, false, false, false),
        }
    }

    pub fn parse(&mut self, start: &str, input: &str)
            -> Result<Match, ParseError>
    {
        let mut _m = MatchNode::new();
        let mut p = Parser::new(self, start, input, &mut _m).map_err(
            |e| ParseError::BadGrammar(e)
        )?;

        loop {
            let mut success = p.try_match();

            loop {
                let a = match p.do_action(success) {
                    Some(a) => a,
                    None => break,
                };

                if let Some(r) = p.go_up(a) {
                    return r;
                }

                success = a.success;
            }
        }
    }
}

struct Parser<'g, 'm, 's> {
    c: LinkTreeCursor<'g, GrammarNode>,
    mc: TreeCursor<'m, MatchNode>,
    pos: usize,
    input: &'s str,
}

impl<'g, 'm, 's> Parser<'g, 'm, 's> {
    fn new(
            root: &'g mut GrammarNode,
            start: &str,
            input: &'s str,
            mroot: &'m mut MatchNode,
    ) -> Result<Parser<'g, 'm, 's>, LinkError> {
        let mut c = LinkTreeCursor::new(root, start)?;
        let mut mc = TreeCursor::new(mroot);

        while c.down() { assert!(mc.down()); }

        Ok(Parser { c, mc, pos: 0, input })
    }

    fn try_match(&mut self) -> bool {
        // Prepare for match.

        let here = self.c.get();
        self.mc.get_mut().st = Some(
            Match::new((self.pos, self.pos), None, vec![])
        );
        let here_st = self.mc.get_mut().st.as_mut().unwrap();

        // Match.

        let count = here.try_match(&self.input[self.pos..]);
        let success = count > 0;
        if success {
            here_st.raw.1 += count;
        }
        success
    }

    fn do_action(&mut self, success: bool) -> Option<Action> {
        // Determine action.

        let a = if success {
            self.c.get().action()
        } else if self.mc.get().st.as_ref()
                .filter(|st| st.raw.0 < st.raw.1).is_some()
        {
            self.c.get().fail_action()
        } else {
            self.c.get().fail_empty_action()
        };

        // Take action.

        if a.down {
            if a.zero {
                self.c.zero();
            }

            if let Some(ref st) = self.mc.get().st {
                self.pos = st.raw.1;
            }

            let mut down = false;
            while self.c.down() {
                assert!(self.mc.down());
                down = true;
            }
            if down {
                return None;
            }
        }

        Some(a)
    }

    fn go_up(&mut self, a: Action) -> Option<Result<Match, ParseError>> {
        if !self.c.up() {
            // Parsing finished.
            if a.success {
                let st = self.mc.get_mut()
                    .st.take().unwrap_or_else(
                        || Match::new((0, 0), None, vec![])
                    );
                if st.raw.1 < self.input.len() {
                    return Some(Err(ParseError::UnmatchedInput(st)));
                }
                return Some(Ok(st));
            } else {
                return Some(Err(ParseError::MatchFail(self.pos)));
            }
        }
        let old_st = self.mc.get_mut().st.take();
        assert!(self.mc.up());

        if let &GrammarNode::Group(ref name, _) = self.c.get() {
            // New parent.
            self.mc.get_mut().st = Some(Match::new(
                (self.pos, self.pos),
                Some(name),
                vec![],
            ));
        }

        if let Some(old_st) = old_st {
            if a.keep {
                self.combine_st(old_st);
            }
        }

        None
    }

    fn combine_st(&mut self, mut old_st: Match) {
        use GrammarNode::*;

        let new_st = &mut self.mc.get_mut().st;

        match self.c.get() {
            &Group(_, _) => {
                // New parent (already created).
                let new_st = new_st.as_mut().unwrap();
                new_st.start_at(&old_st);
                if old_st.name.is_none() {
                    // Merge.
                    new_st.extend(&mut old_st);
                } else {
                    // Insert as child.
                    new_st.insert_child(old_st);
                }
            },
            _ => {
                if let &mut Some(ref mut new_st) = new_st {
                    if new_st.name.is_some() {
                        if old_st.name == new_st.name {
                            // Merge.
                            new_st.extend(&mut old_st);
                        } else if old_st.name.is_some() {
                            // Insert as child.
                            new_st.insert_child(old_st);
                        } else {
                            // Drop.
                            new_st.advance_to(&old_st);
                        }
                    } else {
                        // Drop.
                        new_st.advance_to(&old_st);
                    }
                } else {
                    // Bubble up.
                    *new_st = Some(old_st);
                }
            },
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
            | &mut Name(_, ref mut child)
            | &mut Group(_, ref mut child) => {
                if idx == 0 {
                    Some(child.as_mut())
                } else {
                    None
                }
            },
            &mut Link(_)
            | &mut Range(_, _)
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

#[derive(PartialEq, Eq)]
pub struct Match {
    raw: (usize, usize),
    name: Option<String>,
    children: Vec<Match>,
}

impl Debug for Match {
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

impl Match {
    fn new(raw: (usize, usize), name: Option<&str>, children: Vec<Self>)
            -> Match
    {
        Match {
            raw,
            name: name.map(|s| s.to_string()),
            children,
        }
    }

    fn start_at(&mut self, other: &Self) {
        self.raw.0 = other.raw.0;
    }

    fn advance_to(&mut self, other: &Self) {
        self.raw.1 = other.raw.1;
    }

    fn insert_child(&mut self, child: Self) {
        self.advance_to(&child);
        self.children.push(child);
    }

    fn extend(&mut self, other: &mut Self) {
        for child in other.children.drain(..) {
            self.insert_child(child);
        }
        self.advance_to(other);
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

pub fn u(name: &str, child: GrammarNode) -> GrammarNode {
    GrammarNode::Group(name.to_string(), Box::new(child))
}

pub fn k(target: &str) -> GrammarNode {
    GrammarNode::Link(target.to_string())
}

pub fn r(from: char, to: char) -> GrammarNode {
    GrammarNode::Range(from, to)
}

pub fn t(text: &str) -> GrammarNode {
    GrammarNode::Text(text.to_string())
}

pub fn get_utils() -> GrammarNode {
    e(vec![
        n("ws", s(
            c(vec![
                t(" "),
                t("\t"),
            ]),
        )),
        n("wsp", p(
            c(vec![
                t(" "),
                t("\t"),
            ]),
        )),
        n("wsn", s(
            c(vec![
                t(" "),
                t("\t"),
                t("\n"),
            ]),
        )),
        n("wsnp", p(
            c(vec![
                t(" "),
                t("\t"),
                t("\n"),
            ]),
        )),

        n("nzdigit", c(vec![
            r('1', '9'),
        ])),
        n("digit", c(vec![
            t("0"),
            k("nzdigit"),
        ])),
        n("latin_letter", c(vec![
            r('a', 'z'),
            r('A', 'Z'),
        ])),
    ])
}

pub fn get_grammar_grammar() -> GrammarNode {
    e(vec![
        get_utils(),
        n("ident", e(vec![
            c(vec![
                k("latin_letter"),
                t("_"),
                r('\u{80}', '\u{10FFFF}'), // TODO
            ]),
            s(c(vec![
                k("latin_letter"),
                k("digit"),
                t("_"),
                r('\u{80}', '\u{10FFFF}'), // TODO
            ])),
        ])),
    ])
}

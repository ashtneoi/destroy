#![feature(nll, test)]

#[cfg(test)]
extern crate test;

extern crate tree_cursor;

use constructors::*;
use link_tree::Link;
use std::fmt;
use std::ops::{Add, AddAssign};
use tree_cursor::prelude::*;

pub mod constructors;
mod link_tree;
pub mod parse;
mod tests;

impl Pos {
    fn empty() -> Self {
        Pos { lin: 0, row: 1, col: 1 }
    }
}

impl fmt::Debug for Pos {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}/{},{}", self.lin, self.row, self.col)
    }
}

struct PosDelta {
    lin: usize,
    row: usize,
    col: usize,
}

impl fmt::Debug for PosDelta {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}/{},{}", self.lin, self.row, self.col)
    }
}

impl Add<PosDelta> for Pos {
    type Output = Pos;

    fn add(self, delta: PosDelta) -> Pos {
        Pos {
            lin: self.lin + delta.lin,
            row: self.row + delta.row,
            col: (
                if delta.row > 0 {
                    1
                } else {
                    self.col
                }
            ) + delta.col,
        }
    }
}

impl AddAssign<PosDelta> for Pos {
    fn add_assign(&mut self, delta: PosDelta) {
        *self = *self + delta
    }
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

// TODO: Derive more traits.
#[derive(Eq, PartialEq)]
pub enum GrammarAtom {
    Range(char, char),
    Text(String),
    Anything,
}

impl fmt::Debug for GrammarAtom {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use GrammarAtom::*;
        match self {
            &Range(to, from) =>
                write!(f, "{:#X}..{:#X}", &(to as u32), &(from as u32)),
            &Text(ref t) => write!(f, "{:?}", t),
            &Anything => write!(f, "%"),
        }
    }
}

// TODO: Derive more traits.
#[derive(Eq, PartialEq)]
pub enum GrammarNode {
    Seq(Vec<GrammarNode>),
    Choice(Vec<GrammarNode>),
    Star(Box<GrammarNode>),
    Plus(Box<GrammarNode>),
    Opt(Box<GrammarNode>),
    Pos(Box<GrammarNode>),
    Neg(Box<GrammarNode>),
    Group(String, Box<GrammarNode>),
    Erase(Box<GrammarNode>),
    Link(String),
    Atom(GrammarAtom),
}

impl fmt::Debug for GrammarNode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use GrammarNode::*;
        match self {
            &Seq(ref children) => {
                write!(f, "(")?;
                let mut first = true;
                for child in children.iter() {
                    if !first {
                        write!(f, " ")?;
                    }
                    write!(f, "{:?}", child)?;
                    first = false;
                }
                write!(f, ")")?;
            },
            &Choice(ref children) => {
                write!(f, "{{")?;
                let mut first = true;
                for child in children.iter() {
                    if !first {
                        write!(f, " / ")?;
                    }
                    write!(f, "{:?}", child)?;
                    first = false;
                }
                write!(f, "}}")?;
            },
            &Star(ref child) => write!(f, "{:?}*", child)?,
            &Plus(ref child) => write!(f, "{:?}+", child)?,
            &Opt(ref child) => write!(f, "{:?}?", child)?,
            // TODO: Parens or no? vvv
            &Pos(ref child) => write!(f, "^({:?})", child)?,
            &Neg(ref child) => write!(f, "-({:?})", child)?,
            &Group(ref name, ref child) =>
                write!(f, "({:?})[{}]", child, name)?,
            // TODO: ^^^
            &Erase(ref child) => write!(f, "Erase({:?})", child)?, // TODO
            &Link(ref target) => write!(f, "{}", target)?,
            &Atom(ref inner) => write!(f, "{:?}", inner)?,
        }
        Ok(())
    }
}

impl GrammarNode {
    fn try_match(&self, input: &str) -> Option<PosDelta> {
        use GrammarAtom::*;
        use GrammarNode::*;
        match self {
            &Atom(Range(from, to)) => {
                if let Some(c) = input.chars().next() {
                    if from <= c && c <= to {
                        if c == '\n' {
                            Some(PosDelta { lin: 1, row: 1, col: 0 })
                        } else {
                            Some(PosDelta { lin: 1, row: 0, col: 1 })
                        }
                    } else {
                        None
                    }
                } else {
                    None
                }
            },
            &Atom(Text(ref t)) => {
                if input.starts_with(t) {
                    let mut cp_count: isize = 0;
                    let nls: Vec<isize> = t.chars()
                        .enumerate()
                        .filter_map(
                            |(i, c)| {
                                cp_count += 1;
                                Some(i as isize).filter(|_| c == '\n')
                            }
                        ).collect();
                    let row: isize = nls.len() as isize;
                    let col =
                        cp_count
                        - (*nls.last().unwrap_or(&-1) + 1);
                    Some(PosDelta {
                        lin: cp_count as usize,
                        row: row as usize,
                        col: col as usize,
                    })
                } else {
                    None
                }
            },
            &Atom(Anything) => {
                match input.chars().next() {
                    Some('\n') =>
                        Some(PosDelta { lin: 1, row: 1, col: 0 }),
                    Some(_) =>
                        Some(PosDelta { lin: 1, row: 0, col: 1 }),
                    None => None,
                }
            },
            x => panic!("{:?}", x),
        }
    }

    fn action(&self) -> Action {
        use GrammarAtom::*;
        use GrammarNode::*;
        match self {
            &Seq(_) => act(true, false, true, true),
            &Choice(_) => act(false, false, true, true),
            &Star(_)
            | &Plus(_) => act(true, true, true, true),
            &Opt(_) => act(false, false, true, true),
            &Pos(_) => act(false, false, false, true),
            &Neg(_) => act(false, false, false, false),
            &Group(_, _)
            | &Erase(_)
            | &Link(_)
            | &Atom(_) => act(false, false, true, true),
        }
    }

    fn fail_action(&self) -> Action {
        use GrammarAtom::*;
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
            &Group(_, _)
            | &Erase(_)
            | &Link(_)
            | &Atom(_) => act(false, false, false, false),
        }
    }

}

impl Down for GrammarNode {
    fn down(&self, idx: usize) -> Option<&Self> {
        use GrammarNode::*;
        match self {
            &Seq(ref children)
            | &Choice(ref children) => children.get(idx),
            &Star(ref child)
            | &Plus(ref child)
            | &Opt(ref child)
            | &Pos(ref child)
            | &Neg(ref child)
            | &Group(_, ref child)
            | &Erase(ref child) => {
                if idx == 0 {
                    Some(child.as_ref())
                } else {
                    None
                }
            },
            &Link(_)
            | &Atom(_) => None,
        }
    }
}

impl DownMut for GrammarNode {
    fn down_mut(&mut self, idx: usize) -> Option<&mut Self> {
        use GrammarNode::*;
        match self {
            &mut Seq(ref mut children)
            | &mut Choice(ref mut children) => children.get_mut(idx),
            &mut Star(ref mut child)
            | &mut Plus(ref mut child)
            | &mut Opt(ref mut child)
            | &mut Pos(ref mut child)
            | &mut Neg(ref mut child)
            | &mut Group(_, ref mut child)
            | &mut Erase(ref mut child) => {
                if idx == 0 {
                    Some(child.as_mut())
                } else {
                    None
                }
            },
            &mut Link(_)
            | &mut Atom(_) => None,
        }
    }
}

impl Link for GrammarNode {
    fn target(&self) -> Option<&str> {
        match self {
            &GrammarNode::Link(ref t) => Some(t),
            _ => None,
        }
    }
}

// depth-first, left to right
fn initial(&self) -> Vec<GrammarAtom> {
    let s = vec![];
    s
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Pos {
    pub lin: usize,
    pub row: usize,
    pub col: usize,
}

pub fn get_utils() -> Vec<(&'static str, GrammarNode)> {
    vec![
        ("nzdigit", r('1', '9')),
        ("digit", c(vec![
            t("0"),
            k("nzdigit"),
        ])),
        ("latin_letter", c(vec![
            r('a', 'z'),
            r('A', 'Z'),
        ])),
    ]
}

#![feature(nll, option_xor, test)]

extern crate grow_collections;
extern crate splop;
#[cfg(test)] extern crate test;
extern crate tree_cursor;

use constructors::*;
use link_tree::Link;
use std::cmp::Ordering;
use std::fmt;
use std::ops::{Add, AddAssign};
use string_table::{StringTable, StringTableEntry};
use tree_cursor::prelude::*;

pub mod constructors;
mod link_tree;
pub mod parse;
pub mod string_table;
mod tests;

#[derive(Clone, Copy, Eq, PartialEq)]
pub struct Pos {
    pub lin: usize,
    pub row: usize,
    pub col: usize,
}

impl Pos {
    fn empty() -> Self {
        Pos { lin: 0, row: 1, col: 1 }
    }
}

impl PartialOrd for Pos {
    fn partial_cmp(&self, other: &Pos) -> Option<Ordering> {
        Some(self.lin.cmp(&other.lin))
    }
}

impl Ord for Pos {
    fn cmp(&self, other: &Pos) -> Ordering {
        self.lin.cmp(&other.lin)
    }
}

impl fmt::Debug for Pos {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}/{},{}", self.lin, self.row, self.col)
    }
}

impl fmt::Display for Pos {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}:{}", self.row, self.col)
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
#[derive(Clone, Eq, Ord)]
pub enum GrammarAtom<'i> {
    Anything,
    Text(&'i StringTableEntry),
    Range(char, char),
}

impl<'i> fmt::Debug for GrammarAtom<'i> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use GrammarAtom::*;
        match self {
            &Range(to, from) =>
                write!(f, "{:#X}..{:#X}", &(to as u32), &(from as u32)),
            &Text(&StringTableEntry(ref t, _)) => write!(f, "@\"{}\"", t),
            &Anything => write!(f, "%"),
        }
    }
}

impl<'i> fmt::Display for GrammarAtom<'i> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use GrammarAtom::*;
        match self {
            &Range(to, from) =>
                write!(
                    f,
                    "a code point from {:#X} to {:#X}",
                    &(to as u32),
                    &(from as u32),
                ),
            &Text(&StringTableEntry(ref t, _)) => {
                if t.is_empty() {
                    write!(f, "end of input")
                } else {
                    write!(f, "{:?}", t)
                }
            },
            &Anything => write!(f, "any code point"),
        }
    }
}

impl<'i> PartialEq for GrammarAtom<'i> {
    fn eq(&self, other: &Self) -> bool {
        use GrammarAtom::*;
        match (self, other) {
            (Anything, Anything) => true,
            (Anything, _) => false,
            (_, Anything) => false,
            (
                Text(&StringTableEntry(ref t, _)),
                Text(&StringTableEntry(ref u, _)),
            ) => t == u,
            (Range(a, b), Range(c, d)) => (a, b) == (c, d),
            (Range(..), _) => false,
            (_, Range(..)) => false,
        }
    }
}

impl<'i> PartialOrd for GrammarAtom<'i> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        use GrammarAtom::*;
        use Ordering::*;
        match (self, other) {
            (Anything, Anything) => Some(Equal),
            (Anything, _) => Some(Less),
            (_, Anything) => Some(Greater),
            (
                Text(&StringTableEntry(ref t, _)),
                Text(&StringTableEntry(ref u, _)),
            ) => t.partial_cmp(u),
            (Range(a, b), Range(c, d)) => (a, b).partial_cmp(&(c, d)),
            (Range(..), _) => Some(Greater),
            (_, Range(..)) => Some(Less),
        }
    }
}

// TODO: Derive more traits.
#[derive(Clone, Eq, PartialEq)]
pub enum GrammarNode<'i> {
    Seq(Vec<GrammarNode<'i>>),
    Choice(Vec<GrammarNode<'i>>),
    Star(Box<GrammarNode<'i>>),
    Plus(Box<GrammarNode<'i>>),
    Opt(Box<GrammarNode<'i>>),
    Pos(Box<GrammarNode<'i>>),
    Neg(Box<GrammarNode<'i>>),
    Group(String, Box<GrammarNode<'i>>),
    Erase(Box<GrammarNode<'i>>),
    Link(String),
    Atom(GrammarAtom<'i>),
}

impl<'i> fmt::Debug for GrammarNode<'i> {
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

impl<'i> GrammarNode<'i> {
    fn try_match(
        &self,
        input: &str,
    ) -> Option<(PosDelta, Option<&'i StringTableEntry>)> {
        use GrammarAtom::*;
        use GrammarNode::*;
        match self {
            &Atom(Range(from, to)) => {
                if let Some(c) = input.chars().next() {
                    if from <= c && c <= to {
                        if c == '\n' {
                            Some((PosDelta { lin: 1, row: 1, col: 0 }, None))
                        } else {
                            Some((PosDelta { lin: 1, row: 0, col: 1 }, None))
                        }
                    } else {
                        None
                    }
                } else {
                    None
                }
            },
            &Atom(Text(x)) => {
                let &StringTableEntry(ref t, _) = x;
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
                    Some((
                        PosDelta {
                            lin: cp_count as usize,
                            row: row as usize,
                            col: col as usize,
                        },
                        Some(x),
                    ))
                } else {
                    None
                }
            },
            &Atom(Anything) => {
                match input.chars().next() {
                    Some('\n') =>
                        Some((PosDelta { lin: 1, row: 1, col: 0 }, None)),
                    Some(_) =>
                        Some((PosDelta { lin: 1, row: 0, col: 1 }, None)),
                    None => None,
                }
            },
            x => panic!("{:?}", x),
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
            &Group(_, _)
            | &Erase(_)
            | &Link(_)
            | &Atom(_) => act(false, false, true, true),
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
            &Group(_, _)
            | &Erase(_)
            | &Link(_)
            | &Atom(_) => act(false, false, false, false),
        }
    }

}

impl<'i> Down for GrammarNode<'i> {
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

impl<'i> DownMut for GrammarNode<'i> {
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

impl<'i> Link for GrammarNode<'i> {
    fn target(&self) -> Option<&str> {
        match self {
            &GrammarNode::Link(ref t) => Some(t),
            _ => None,
        }
    }
}

pub fn get_utils<'i>(
    tab: &mut StringTable<'i>,
) -> Vec<(&'static str, GrammarNode<'i>)> {
    vec![
        ("nzdigit", r('1', '9')),
        ("digit", c(vec![
            t(tab, "0"),
            k("nzdigit"),
        ])),
        ("latin_letter", c(vec![
            r('a', 'z'),
            r('A', 'Z'),
        ])),
    ]
}

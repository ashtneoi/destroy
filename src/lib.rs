#![feature(nll, test)]

#[cfg(test)]
extern crate test;

#[cfg(test)]
mod tests;

mod tree;

pub mod prelude {
    pub use {e, c, s, p, q, z, g, n, u, x, k, r, t, a};
    pub use get_grammar_grammar;
    pub use GrammarNode;
    pub use parse_grammar;
}

use std::fmt::{Debug, Formatter, self};
use tree::OpaqueVerticalCursorGroup;
use tree::prelude::*;

pub struct MatchCursor<'x> {
    g: LinkTreeCursor<'x, GrammarNode>,
    m: MutTreeCursor<'x, MatchNode>,
}

impl<'x> MatchCursor<'x> {
    fn new(g: LinkTreeCursor<'x, GrammarNode>, m: MutTreeCursor<'x, MatchNode>)
            -> Self
    {
        MatchCursor { g, m }
    }
}

impl<'x> OpaqueVerticalCursorGroup for MatchCursor<'x> {
    fn list(&mut self) -> Vec<&mut OpaqueVerticalCursor> {
        vec![&mut self.g, &mut self.m] // TODO: don't allocate every time
    }
}

pub struct MatchNode {
    child: Option<Box<MatchNode>>,
    st: Option<Match>,
}

impl DownMut for MatchNode {
    fn down_mut(&mut self, _idx: usize) -> Option<&mut Self> {
        self.child = Some(Box::new(Self::new()));
        Some(self.child.as_mut().unwrap().as_mut())
    }
}

impl MatchNode {
    fn new() -> Self {
        Self { child: None, st: None }
    }
}

#[derive(Eq, PartialEq)]
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
    Erase(Box<GrammarNode>),
    Link(String),
    Range(char, char),
    Text(String),
    Anything,
}

impl Debug for GrammarNode {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
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
                let mut first = true;
                for child in children.iter() {
                    if !first {
                        write!(f, " / ")?;
                    }
                    write!(f, "{:?}", child)?;
                    first = false;
                }
            },
            &Star(ref child) => write!(f, "{:?}*", child)?,
            &Plus(ref child) => write!(f, "{:?}+", child)?,
            &Opt(ref child) => write!(f, "{:?}?", child)?,
            // TODO: Parens or no? vvv
            &Pos(ref child) => write!(f, "^({:?})", child)?,
            &Neg(ref child) => write!(f, "-({:?})", child)?,
            // TODO: ^^^
            &Name(ref name, ref child) =>
                write!(f, "{} = ({:?})\n", name, child)?,
            // TODO: vvv
            &Group(ref name, ref child) =>
                write!(f, "({:?})[{}]", child, name)?,
            // TODO: ^^^
            &Erase(ref child) => write!(f, "Erase({:?})", child)?, // TODO
            &Link(ref target) => write!(f, "{:?}", target)?,
            &Range(to, from) =>
                write!(f, "{:#X}..{:#X}", &(to as u32), &(from as u32))?,
            &Text(ref t) => write!(f, "{:?}", t)?,
            &Anything => write!(f, "%")?,
        }
        Ok(())
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

#[derive(Debug)]
pub enum ParseError {
    BadGrammar(LinkError),
    MatchFail(Pos),
    UnmatchedInput(Match),
}

impl GrammarNode {
    fn try_match(&self, input: &str) -> Option<PosDelta> {
        use GrammarNode::*;
        match self {
            &Range(from, to) => {
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
            &Text(ref t) => {
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
            &Anything => {
                match input.chars().next() {
                    Some('\n') =>
                        Some(PosDelta { lin: 1, row: 1, col: 0 }),
                    Some(_) =>
                        Some(PosDelta { lin: 1, row: 0, col: 1 }),
                    None => None,
                }
            },
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
            | &Erase(_)
            | &Link(_)
            | &Range(_, _)
            | &Text(_)
            | &Anything => act(false, false, true, true),
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
            | &Erase(_)
            | &Link(_)
            | &Range(_, _)
            | &Text(_)
            | &Anything => act(false, false, false, false),
        }
    }

    pub fn parse(&self, start: &str, input: &str)
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

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Pos {
    lin: usize,
    row: usize,
    col: usize,
}

impl Pos {
    fn empty() -> Self {
        Pos { lin: 0, row: 1, col: 1 }
    }
}

impl Debug for Pos {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}/{},{}", self.lin, self.row, self.col)
    }
}

struct PosDelta {
    lin: usize,
    row: usize,
    col: usize,
}

impl Debug for PosDelta {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}/{},{}", self.lin, self.row, self.col)
    }
}

struct Parser<'x, 's> {
    c: MatchCursor<'x>,
    pos: Pos,
    input: &'s str,
}

impl<'x, 's> Parser<'x, 's> {
    fn new(
            root: &'x GrammarNode,
            start: &str,
            input: &'s str,
            mroot: &'x mut MatchNode,
    ) -> Result<Self, LinkError> {
        let mut c = MatchCursor::new(
            LinkTreeCursor::new(root, start)?,
            MutTreeCursor::new(mroot),
        );

        while c.down() { }

        Ok(Parser { c, pos: Pos { lin: 0, row: 1, col: 1 }, input })
    }

    fn try_match(&mut self) -> bool {
        // Prepare for match.

        let here = self.c.g.get();
        self.c.m.get_mut().st = Some(
            Match::new((self.pos, self.pos), vec![])
        );
        let here_st = self.c.m.get_mut().st.as_mut().unwrap();

        // Match.

        let maybe_delta = here.try_match(&self.input[self.pos.lin..]);
        match maybe_delta {
            Some(delta) => {
                let prev_raw = here_st.raw.1;
                here_st.raw.1 = Pos {
                    lin: prev_raw.lin + delta.lin,
                    row: prev_raw.row + delta.row,
                    col: (
                        if delta.row > 0 {
                            1
                        } else {
                            prev_raw.col
                        }
                    ) + delta.col,
                };
                true
            },
            None => false,
        }
    }

    fn do_action(&mut self, success: bool) -> Option<Action> {
        // Determine action.

        let a = if success {
            self.c.g.get().action()
        } else if self.c.m.get().st.as_ref()
                .filter(|st| !st.is_empty()).is_some()
        {
            self.c.g.get().fail_action()
        } else {
            self.c.g.get().fail_empty_action()
        };

        // Take action.

        if a.down {
            if a.zero {
                self.c.zero();
            }

            if let Some(ref st) = self.c.m.get().st {
                self.pos = st.raw.1;
            }

            let mut down = false;
            while self.c.down() {
                down = true;
            }
            if down {
                return None;
            }
        }

        Some(a)
    }

    fn go_up(&mut self, a: Action) -> Option<Result<Match, ParseError>> {
        let old_st = self.c.m.get_mut().st.take();

        if !self.c.up() {
            // Parsing finished.
            if a.success {
                let st = old_st.unwrap_or_else(
                    || Match::new(
                        (Pos::empty(), Pos::empty()),
                        vec![],
                    )
                );
                if st.raw.1.lin < self.input.len() {
                    return Some(Err(ParseError::UnmatchedInput(st)));
                }
                return Some(Ok(st));
            } else {
                return Some(Err(ParseError::MatchFail(self.pos)));
            }
        }

        if let &GrammarNode::Group(ref name, _) = self.c.g.get() {
            // Special case: create Match even if old_st is None.
            self.c.m.get_mut().st = Some(Match::new(
                (self.pos, self.pos), vec![(name, vec![])]
            ));
        }

        if let Some(old_st) = old_st {
            if a.keep {
                self.combine_st(old_st);
            }
        }

        if !a.keep {
            if let Some(ref st) = self.c.m.get().st {
                self.pos = st.raw.0;
            }
        }

        None
    }

    fn combine_st(&mut self, mut old_st: Match) {
        use GrammarNode::*;

        let new_st = &mut self.c.m.get_mut().st;

        match self.c.g.get() {
            &Seq(_)
            | &Star(_)
            | &Plus(_) => {
                if let Some(ref mut new_st) = new_st {
                    new_st.extend(&mut old_st);
                } else {
                    *new_st = Some(old_st);
                }
            },
            &Group(ref name, _) => {
                // New parent (already created).
                let new_st = new_st.as_mut().unwrap();
                new_st.start_at(&old_st);
                new_st.insert_child(name, old_st);
            },
            &Erase(_) => {
                old_st.named = vec![];
                *new_st = Some(old_st);
            },
            _ => {
                // Bubble up.
                *new_st = Some(old_st);
            },
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
            | &Name(_, ref child)
            | &Group(_, ref child)
            | &Erase(ref child) => {
                if idx == 0 {
                    Some(child.as_ref())
                } else {
                    None
                }
            },
            &Link(_)
            | &Range(_, _)
            | &Text(_)
            | &Anything => None,
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
            | &mut Name(_, ref mut child)
            | &mut Group(_, ref mut child)
            | &mut Erase(ref mut child) => {
                if idx == 0 {
                    Some(child.as_mut())
                } else {
                    None
                }
            },
            &mut Link(_)
            | &mut Range(_, _)
            | &mut Text(_)
            | &mut Anything => None,
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
    raw: (Pos, Pos),
    named: Vec<(String, Vec<Match>)>,
}

pub fn mat(
        (lin0, row0, col0, lin1, row1, col1):
            (usize, usize, usize, usize, usize, usize),
        named: Vec<(&str, Vec<Match>)>,
) -> Match {
    Match::new(
        (
            Pos {
                lin: lin0,
                row: row0,
                col: col0,
            },
            Pos {
                lin: lin1,
                row: row1,
                col: col1,
            },
        ),
        named,
    )
}

impl Debug for Match {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "({:?}..{:?})", self.raw.0, self.raw.1)?;
        if !self.named.is_empty() {
            write!(f, "{{")?;
            let mut first = true;
            for &(ref name, ref children) in self.named.iter() {
                if first {
                    first = false;
                } else {
                    write!(f, ", ")?;
                }
                write!(f, "{}: {:?}", name, children)?;
            }
            write!(f, "}}")?;
        }

        Ok(())
    }
}

impl Match {
    fn new(raw: (Pos, Pos), named: Vec<(&str, Vec<Match>)>) -> Self
    {
        Match {
            raw,
            named: named.into_iter().map(
                |(s, children)| (s.to_string(), children)
            ).collect(),
        }
    }

    fn start_at(&mut self, other: &Self) {
        self.raw.0 = other.raw.0;
    }

    fn advance_to(&mut self, other: &Self) {
        self.raw.1 = other.raw.1;
    }

    fn get_mut(&mut self, name: &str) -> Option<&mut Vec<Self>> {
        self.named.iter_mut().find(|&&mut (ref n, _)| n == name)
            .map(|&mut (_, ref mut cc)| cc)
    }

    fn insert_child(&mut self, name: &str, child: Self) {
        self.advance_to(&child);
        match self.get_mut(name) {
            Some(children) => children.push(child),
            None => self.named.push((name.to_string(), vec![child])),
        }
    }

    fn extend(&mut self, other: &mut Self) {
        for (name, children) in other.named.drain(..) {
            match self.get_mut(&name) {
                Some(children2) => children2.extend(children),
                None => self.named.push((name, children)),
            }
        }
        self.advance_to(other);
    }

    fn is_empty(&self) -> bool {
        self.raw.0.lin == self.raw.1.lin
    }
}

impl Down for Match {
    fn down(&self, _idx: usize) -> Option<&Self> {
        None
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

pub fn x(child: GrammarNode) -> GrammarNode {
    GrammarNode::Erase(Box::new(child))
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

pub fn a() -> GrammarNode {
    GrammarNode::Anything
}

pub fn get_utils() -> GrammarNode {
    e(vec![
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

        n("comment", e(vec![
            t("#"),
            s(e(vec![
                g(t("\n")),
                a(),
            ])),
        ])),

        n("wso_part", c(vec![
            t(" "),
            t("\t"),
        ])),
        n("ws_part", c(vec![
            k("wso_part"),
            e(vec![
                q(k("comment")),
                t("\n"),
            ]),
        ])),
        n("wso", s(k("wso_part"))),
        n("ws", s(k("ws_part"))),
        n("pwso", p(k("wso_part"))),
        n("pws", p(k("ws_part"))),

        n("hex_digit", c(vec![
            k("digit"),
            r('a', 'f'),
            r('A', 'F'),
        ])),
        n("hex_uint", e(vec![
            t("0x"),
            p(k("hex_digit")),
        ])),

        n("str", e(vec![
            t("\""),
            s(c(vec![
                e(vec![
                    t("\\"),
                    c(vec![
                        t("n"),
                        t("\\"),
                        t("\""),
                    ]),
                ]),
                e(vec![
                    g(t("\"")),
                    g(t("\n")),
                    a(),
                ]),
            ])),
            t("\""),
        ])),
        n("cp", c(vec![
            k("hex_uint"),
            e(vec![
                t("'"),
                c(vec![
                    e(vec![
                        t("\\"),
                        c(vec![
                            t("n"),
                            t("\\"),
                            t("'"),
                        ]),
                    ]),
                    e(vec![
                        g(t("\"")),
                        g(t("\n")),
                        a(),
                    ]),
                ]),
                t("'"),
            ]),
        ])),
        n("cp_range", e(vec![
            k("cp"),
            t(".."),
            k("cp"),
        ])),
        n("ident_initial", c(vec![
            k("latin_letter"),
            t("_"),
            r('\u{80}', '\u{10FFFF}'), // TODO
        ])),
        n("ident", e(vec![
            k("ident_initial"),
            s(c(vec![
                k("ident_initial"),
                k("digit"), // TODO
            ])),
        ])),

        n("expr", e(vec![
            k("expr_choice"),
            s(e(vec![
                k("pws"),
                k("expr_choice"),
                g(e(vec![
                    k("wso"),
                    t("="),
                ])),
            ])),
        ])),
        n("expr_choice", e(vec![
            u("opd", k("expr_prefix")),
            s(e(vec![
                k("ws"),
                u("op", t("/")),
                k("ws"),
                u("opd", k("expr_prefix")),
            ])),
        ])),
        n("expr_prefix", e(vec![
            s(u("op", c(vec![
                t("^"),
                t("-"),
            ]))),
            u("opd", k("expr_suffix")),
        ])),
        n("expr_suffix", e(vec![
            u("opd", k("expr_atom")),
            s(u("op", c(vec![
                t("*"),
                t("+"),
                t("?"),
                e(vec![
                    t("["),
                    u("name", k("ident")),
                    t("]"),
                ]),
            ]))),
        ])),
        n("expr_atom", u("atom", c(vec![
            t("%"),
            k("str"),
            k("cp_range"),
            k("ident"),
            e(vec![
                t("("),
                k("ws"),
                k("expr"),
                k("ws"),
                t(")"),
            ]),
        ]))),

        n("rule", e(vec![
            u("name", k("ident")),
            k("wso"),
            t("="),
            k("ws"),
            u("val", k("expr")),
        ])),
        n("grammar", e(vec![
            k("ws"),
            s(e(vec![
                k("rule"),
                k("wso"),
                q(k("comment")),
                t("\n"),
                k("ws"),
            ])),
            q(e(vec![
                k("rule"),
                k("wso"),
                q(k("comment")),
            ])),
        ])),
    ])
}

use Action;
use constructors::*;
use get_utils;
use GrammarNode;
use link_tree::{LinkError, LinkTreeCursor};
use Pos;
use std::borrow::Borrow;
use std::char;
use std::fmt;
use std::mem;
use string_table::StringTable;
use tree_cursor::cursor::TreeCursorMut;
use tree_cursor::prelude::*;

// TODO: verify that Clone is acceptable
#[derive(Clone, PartialEq, Eq)]
pub struct Match<'i> {
    pub(super) raw: (Pos, Pos),
    interned: Option<&'i (String, usize)>,
    named: Vec<(String, Vec<Match<'i>>)>,
}

impl<'i> fmt::Debug for Match<'i> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
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

impl<'i> Match<'i> {
    fn empty() -> Self {
        Match {
            raw: (Pos::empty(), Pos::empty()),
            interned: None,
            named: vec![],
        }
    }

    pub(super) fn new(
        raw: (Pos, Pos),
        named: Vec<(&str, Vec<Match<'i>>)>,
    ) -> Self {
        Match {
            raw,
            interned: None,
            named: named.into_iter().map(
                |(s, children)| (s.to_string(), children)
            ).collect(),
        }
    }

    pub fn raw<'s>(&self, input: &'s str) -> &'s str {
        &input[self.raw.0.lin..self.raw.1.lin]
    }

    fn start_at(&mut self, other: &Self) {
        self.raw.0 = other.raw.0;
    }

    fn advance_to(&mut self, other: &Self) {
        self.raw.1 = other.raw.1;
    }

    fn clear(&mut self) {
        self.raw.1 = self.raw.0;
    }

    pub fn count(&self, name: &str) -> usize {
        self.get(name).map(|v| v.len()).unwrap_or(0)
    }

    pub fn get_or_empty(&self, name: &str) -> &[Self] {
        self.get(name).map(|v| v.as_slice()).unwrap_or(empty_slice())
    }

    pub fn iter(&self, name: &str) -> impl Iterator<Item = &Match<'i>> {
        self.get_or_empty(name).iter()
    }

    pub fn iter_rev(&self, name: &str) -> impl Iterator<Item = &Match<'i>> {
        self.get_or_empty(name).iter().rev()
    }

    pub fn get(&self, name: &str) -> Option<&Vec<Self>> {
        self.named.iter().find(|&&(ref n, _)| n == name)
            .map(|&(_, ref cc)| cc)
    }

    pub fn get_mut(&mut self, name: &str) -> Option<&mut Vec<Self>> {
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

impl<'i> Down for Match<'i> {
    fn down(&self, _idx: usize) -> Option<&Self> {
        None
    }
}

#[derive(Clone)]
pub(crate) struct ParseNode<'i> {
    child: Option<Box<ParseNode<'i>>>,
    m: Match<'i>,
}

impl<'i> DownMut for ParseNode<'i> {
    fn down_mut(&mut self, _idx: usize) -> Option<&mut Self> {
        // TODO: The borrow checker made this ugly. Fix it.

        if self.child.is_some() {
            self.child.as_mut().map(|b| b.as_mut())
        } else {
            let mut raw = self.m.raw; // should be copy
            raw.0 = raw.1;
            self.child = Some(Box::new(ParseNode {
                child: None,
                m: Match::new(raw, vec![]),
            }));
            Some(self.child.as_mut().unwrap().as_mut())
        }
    }
}

impl<'i> TakeChild for ParseNode<'i> {
    fn take_child(&mut self, idx: usize) -> Self {
        assert!(idx == 0);
        *self.child.take().unwrap()
    }
}

impl<'i> ParseNode<'i> {
    pub(crate) fn new() -> Self {
        Self {
            child: None,
            m: Match::new((Pos::empty(), Pos::empty()), vec![]),
        }
    }
}

struct MatchCursor<'x, 'i: 'x> {
    g: LinkTreeCursor<'x, GrammarNode<'i>>,
    m: TreeCursorMut<'x, 'x, ParseNode<'i>>,
}

impl<'x, 'i: 'x> MatchCursor<'x, 'i> {
    fn new(
        named: &'x [(impl Borrow<str>, GrammarNode<'i>)],
        start: &str,
        pnroot: &'x mut ParseNode<'i>,
    ) -> Result<Self, LinkError> {
        Ok(MatchCursor {
            g: LinkTreeCursor::new(named, start)?,
            m: TreeCursorMut::new(pnroot),
        })
    }

    fn zero(&mut self) {
        self.g.zero();
        self.m.zero();
    }

    fn up(&mut self, preserve_match: bool) -> bool {
        if self.g.up() {
            if !preserve_match {
                // TODO: this is very unclean
                self.m.take_node().unwrap();
            }
            true
        } else {
            false
        }
    }

    fn down(&mut self) -> bool {
        if self.g.down() {
            assert!(self.m.down());
            true
        } else {
            false
        }
    }
}

pub fn empty_slice<'a, T>() -> &'a [T] {
    &[]
}

// TODO
#[derive(Debug)]
pub enum ParseError {
    BadGrammar(LinkError),
    MatchFail(Option<Pos>),
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ParseError::BadGrammar(_) =>
                write!(f, "your grammar sucks"),
            ParseError::MatchFail(pos) =>
                if let Some(pos) = pos {
                    write!(f, "match failed at {}", pos)
                } else {
                    write!(f, "match failed somewhere")
                }
        }
    }
}

/// 'x: GrammarNode and ParseNode
/// 's: input
/// 'i: grammar StringTable
pub struct Parser<'x, 's, 'i: 'x> {
    c: MatchCursor<'x, 'i>,
    input: &'s str,
    fail_pos: Option<Pos>,
}

impl<'x, 's, 'i: 'x> Parser<'x, 's, 'i> {
    pub fn parse(
        named: &'x [(impl Borrow<str>, GrammarNode<'i>)],
        start: &str,
        input: &'s str,
    ) -> Result<Match<'i>, ParseError> {
        let mut pn = ParseNode::new();
        let mut p = Parser::new(named, start, input, &mut pn).map_err(
            |e| ParseError::BadGrammar(e)
        )?;

        loop {
            let success = p.try_match();

            match p.step(success, false) {
                Some(Ok(m)) => return Ok(m),
                Some(Err(_)) =>
                    return Err(ParseError::MatchFail(p.fail_pos)),
                None => (),
            }
        }
    }

    /// `None` means keep going. `Some(Ok(_))` means success. `Some(Err(_))`
    /// means there was a parse error.
    pub(crate) fn step(
        &mut self,
        mut success: bool,
        suppress_down: bool,
    ) -> Option<Result<Match<'i>, Match<'i>>> {
        // TODO: Return type isn't very clear.
        loop {
            let a = if success {
                self.c.g.get().action()
            } else if self.c.m.get().m.is_empty() {
                self.c.g.get().fail_empty_action()
            } else {
                self.c.g.get().fail_action()
            };

            // TODO: This is more efficient than before, right?
            if !a.success {
                if self.fail_pos.is_none() {
                    self.fail_pos = Some(self.c.m.get().m.raw.1);
                }
            }

            if self.do_action(a, suppress_down) {
                self.fail_pos = None;
                return None;
            }

            if let Some(r) = self.go_up(a) {
                return Some(r);
            }

            success = a.success;
        }
    }

    pub(crate) fn new(
            named: &'x [(impl Borrow<str>, GrammarNode<'i>)],
            start: &str,
            input: &'s str,
            pnroot: &'x mut ParseNode<'i>,
    ) -> Result<Self, LinkError> {
        let mut p = Parser {
            c: MatchCursor::new(named, start, pnroot)?,
            input,
            fail_pos: None,
        };
        while p.c.down() { }
        Ok(p)
    }

    /// Returns whether the input matched.
    fn try_match(&mut self) -> bool {
        // Prepare for match.

        let here = self.c.g.get();
        let here_st = &mut self.c.m.get_mut().m;

        // Match.

        let pos = here_st.raw.0;
        let maybe_delta = here.try_match(&self.input[pos.lin..]);
        match maybe_delta {
            Some(delta) => { here_st.raw.1 += delta; true }
            None => false,
        }
    }

    /// `true` means we reached an atom and should try to match it. `false`
    /// means we should go up and call this method again.
    fn do_action(&mut self, a: Action, suppress_down: bool) -> bool {
        if a.down && !suppress_down {
            if a.zero {
                self.c.zero();
            }

            let mut down = false;
            while self.c.down() {
                down = true;
            }
            if down {
                return true;
            }
        }

        // This is specifically for initial(). (See MatchCursor::up().) There's
        // probably a better way to do it.
        if !a.keep {
            self.c.m.get_mut().m.clear();
        }

        false
    }

    /// `None` means we went up. `Some(Ok(_))` means the parse was successful.
    /// `Some(Err(_))` means there was a parse error.
    fn go_up(&mut self, a: Action) -> Option<Result<Match<'i>, Match<'i>>> {
        let old_st = mem::replace(&mut self.c.m.get_mut().m, Match::empty());

        if !self.c.up(false) {
            // Parsing finished.
            if a.success {
                if old_st.raw.1.lin < self.input.len() {
                    return Some(Err(old_st));
                }
                return Some(Ok(old_st));
            } else {
                return Some(Err(old_st));
            }
        }

        if a.keep {
            self.combine_st(old_st);
        }

        None
    }

    fn combine_st(&mut self, mut old_st: Match<'i>) {
        use GrammarNode::*;

        let new_st = &mut self.c.m.get_mut().m;

        new_st.interned = new_st.interned.xor(old_st.interned);

        match self.c.g.get() {
            &Seq(_)
            | &Star(_)
            | &Plus(_) => new_st.extend(&mut old_st),
            &Group(ref name, _) => {
                // New parent (already created).
                new_st.start_at(&old_st);
                new_st.insert_child(name, old_st);
            },
            &Erase(_) => {
                old_st.named = vec![];
                *new_st = old_st;
            },
            _ => {
                // Bubble up.
                *new_st = old_st;
            },
        }
    }
}

pub(super) fn get_grammar_grammar<'i>(
    tab: &mut StringTable<'i>,
) -> Vec<(&'static str, GrammarNode<'i>)> {
    let mut gg = get_utils(tab);

    gg.append(&mut vec![
        ("comment", e(vec![
            t(tab, "#"),
            s(e(vec![
                n(t(tab, "\n")),
                a(),
            ])),
        ])),

        ("wso_part", c(vec![
            t(tab, " "),
            t(tab, "\t"),
        ])),
        ("ws_part", c(vec![
            k("wso_part"),
            e(vec![
                q(k("comment")),
                t(tab, "\n"),
            ]),
        ])),
        ("wso", s(k("wso_part"))),
        ("ws", s(k("ws_part"))),
        ("pwso", p(k("wso_part"))),
        ("pws", p(k("ws_part"))),

        ("hex_digit", c(vec![
            k("digit"),
            r('a', 'f'),
            r('A', 'F'),
        ])),
        ("hex_uint", e(vec![
            t(tab, "0x"),
            p(k("hex_digit")),
        ])),

        ("str", e(vec![
            t(tab, "\""),
            s(u("cp", c(vec![
                e(vec![
                    t(tab, "\\"),
                    c(vec![
                        t(tab, "n"),
                        t(tab, "t"),
                        t(tab, "\\"),
                        t(tab, "\""),
                    ]),
                ]),
                e(vec![
                    n(t(tab, "\"")),
                    n(t(tab, "\n")),
                    a(),
                ]),
            ]))),
            t(tab, "\""),
        ])),
        ("cp", c(vec![
            u("hex", k("hex_uint")),
            e(vec![
                t(tab, "'"),
                u("raw", c(vec![
                    e(vec![
                        t(tab, "\\"),
                        c(vec![
                            t(tab, "n"),
                            t(tab, "t"),
                            t(tab, "\\"),
                            t(tab, "'"),
                        ]),
                    ]),
                    e(vec![
                        n(t(tab, "'")),
                        n(t(tab, "\n")),
                        a(),
                    ]),
                ])),
                t(tab, "'"),
            ]),
        ])),
        ("cp_range", e(vec![
            u("from", k("cp")),
            t(tab, ".."),
            u("to", k("cp")),
        ])),
        ("ident_initial", c(vec![
            k("latin_letter"),
            t(tab, "_"),
            r('\u{80}', '\u{10FFFF}'), // TODO
        ])),
        ("ident", e(vec![
            k("ident_initial"),
            s(c(vec![
                k("ident_initial"),
                k("digit"), // TODO
            ])),
        ])),

        ("expr", e(vec![
            u("opd", k("expr_seq")),
            s(e(vec![
                k("ws"),
                t(tab, "/"),
                k("ws"),
                u("opd", k("expr_seq")),
            ])),
        ])),
        ("expr_seq", e(vec![
            u("opd", k("expr_affix")),
            s(e(vec![
                k("pws"),
                u("opd", k("expr_affix")),
                n(e(vec![
                    k("wso"),
                    t(tab, "="),
                ])),
            ])),
        ])),
        ("expr_affix", e(vec![
            s(u("pre", c(vec![
                t(tab, "^"),
                t(tab, "-"),
            ]))),
            u("opd", k("expr_atom")),
            s(u("suf", c(vec![
                t(tab, "*"),
                t(tab, "+"),
                t(tab, "?"),
                e(vec![
                    t(tab,"["),
                    u("name", k("ident")),
                    t(tab, "]"),
                ]),
            ]))),
        ])),
        ("expr_atom", c(vec![
            t(tab, "%"),
            k("str"),
            u("r", k("cp_range")),
            u("id", k("ident")),
            e(vec![
                t(tab, "("),
                k("ws"),
                u("expr", k("expr")),
                k("ws"),
                t(tab, ")"),
            ]),
        ])),

        ("rule", e(vec![
            u("name", k("ident")),
            k("wso"),
            t(tab, "="),
            k("ws"),
            u("val", k("expr")),
        ])),
        ("grammar", e(vec![
            k("ws"),
            s(e(vec![
                k("rule"),
                k("wso"),
                q(k("comment")),
                t(tab, "\n"),
                k("ws"),
            ])),
            q(e(vec![
                k("rule"),
                k("wso"),
                q(k("comment")),
            ])),
        ])),
    ]);

    gg
}

fn parse_escape(s: &str) -> char {
    let ss: Vec<_> = s.chars().collect();
    match ss[0] {
        '\\' => {
            assert!(ss.len() == 2);
            match ss[1] {
                'n' => '\n',
                't' => '\t',
                // TODO: Just panic?
                c => c, // note: grammar should be more restrictive than this
            }
        },
        c => {
            assert!(ss.len() == 1);
            c
        }
    }
}

fn parse_expr<'i>(
    input: &str,
    expr: &Match<'i>,
    tab: &mut StringTable<'i>,
    gc: &mut TreeCursorMut<GrammarNode<'i>>,
) -> Result<(), ParseError> {
    use GrammarNode::*;

    assert_eq!(gc.get(), &Choice(vec![]));

    for ee in expr.iter("opd") {
        // push c
        if let &mut Choice(ref mut v) = gc.get_mut() {
            v.push(e(vec![]));
        } else { panic!(); }
        // down
        let mut gc = gc.split_below().unwrap();

        for af in ee.iter("opd") {
            // push a
            if let &mut Seq(ref mut v) = gc.get_mut() {
                v.push(a()); // placeholder
            } else { panic!(); }
            // down
            let mut gc = gc.split_below().unwrap();

            for pre in af.iter("pre") {
                // change a to op(a)
                *gc.get_mut() = match pre.raw(input) {
                    "^" => z(a()),
                    "-" => n(a()),
                    _ => panic!(),
                };
                // down
                assert!(gc.down()); // TODO: probably fine, right?
            }

            for suf in af.iter_rev("suf") {
                // change a to op(a)
                *gc.get_mut() = match suf.raw(input) {
                    "*" => s(a()),
                    "+" => p(a()),
                    "?" => q(a()),
                    name => {
                        assert!(name.starts_with("["));
                        assert!(name.ends_with("]"));
                        u(suf.get("name").unwrap()[0].raw(input), a())
                    },
                };
                // down
                assert!(gc.down()); // TODO: probably fine, right?
            }

            let atom = &af.get("opd").unwrap()[0];

            fn first<'a, 'i>(v: &'a Vec<Match<'i>>) -> &'a Match<'i> {
                &v[0]
            }

            let atom_raw = atom.raw(input);
            if atom_raw == "%" {
                *gc.get_mut() = a();
            } else if atom_raw.starts_with('"') {
                let mut s = String::new();
                for cp in atom.iter("cp") {
                    s.push(parse_escape(cp.raw(input)));
                }
                *gc.get_mut() = t(tab, &s);
            } else if let Some(rr) = atom.get("r").map(first) {
                let ff = &rr.get("from").unwrap()[0];
                let tt = &rr.get("to").unwrap()[0];
                let ffc = if let Some(hex) = ff.get("hex").map(first) {
                    char::from_u32(
                        u32::from_str_radix(&hex.raw(input)[2..], 16)
                            .unwrap()
                    ).unwrap() // TODO
                } else if let Some(raw) = ff.get("raw").map(first) {
                    parse_escape(raw.raw(input))
                } else { panic!(); };
                let ttc = if let Some(hex) = tt.get("hex").map(first) {
                    char::from_u32(
                        u32::from_str_radix(&hex.raw(input)[2..], 16)
                            .unwrap()
                    ).unwrap() // TODO
                } else if let Some(raw) = tt.get("raw").map(first) {
                    parse_escape(raw.raw(input))
                } else { panic!(); };
                *gc.get_mut() = r(ffc, ttc);
            } else if let Some(id) = atom.get("id").map(first) {
                *gc.get_mut() = k(id.raw(input));
            } else if let Some(inner_expr) = atom.get("expr").map(first) {
                // change a to c([])
                *gc.get_mut() = c(vec![]);
                // recurse
                parse_expr(input, &inner_expr, tab, &mut gc)?;
            }
        }
    }

    Ok(())
}

pub(super) fn parse_grammar_with_grammar<'i>(
    gg: &[(impl Borrow<str>, GrammarNode<'i>)],
    tab: &mut StringTable<'i>,
    input: &str,
) -> Result<Vec<(String, GrammarNode<'i>)>, ParseError> {
    use GrammarNode::*;

    let m = match Parser::parse(&gg, "grammar", input) {
        Ok(x) => x,
        Err(ParseError::BadGrammar(e)) => panic!("{:?}", e),
        Err(e) => return Err(e),
    };

    let mut g = Vec::<(String, GrammarNode)>::new();

    let rule_count = m.count("name");
    assert_eq!(rule_count, m.count("val"));

    for (cname, cval) in m.iter("name").zip(m.iter("val")) {
        let name = cname.raw(input);
        g.push((name.to_string(), Choice(vec![])));
        let mut gc = TreeCursorMut::new(&mut g.last_mut().unwrap().1);
        parse_expr(input, &cval, tab, &mut gc)?;
    }

    Ok(g)
}

pub fn parse_grammar<'i>(
    tab: &mut StringTable<'i>,
    input: &str,
) -> Result<Vec<(String, GrammarNode<'i>)>, ParseError> {
    let mut gg_tab = StringTable::new();
    let gg = get_grammar_grammar(&mut gg_tab);
    parse_grammar_with_grammar(&gg, tab, input)
}

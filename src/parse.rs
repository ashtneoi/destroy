use Action;
use constructors::*;
use get_utils;
use GrammarAtom;
use GrammarNode;
use link_tree::{LinkError, LinkTreeCursor};
use Pos;
use std::borrow::Borrow;
use std::char;
use std::fmt;
use std::mem;
use std::ops::Index;
use tree_cursor::cursor::{
    TreeCursorMut,
    TreeCursorPos,
};
use tree_cursor::prelude::*;

#[derive(PartialEq, Eq)]
pub struct Match {
    pub(super) raw: (Pos, Pos),
    named: Vec<(String, Vec<Match>)>,
}

impl fmt::Debug for Match {
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

impl Match {
    fn empty() -> Self {
        Match {
            raw: (Pos::empty(), Pos::empty()),
            named: vec![],
        }
    }

    pub(super) fn new(raw: (Pos, Pos), named: Vec<(&str, Vec<Match>)>) -> Self
    {
        Match {
            raw,
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

    pub fn count(&self, name: &str) -> usize {
        self.get(name).map(|v| v.len()).unwrap_or(0)
    }

    pub fn get_or_empty(&self, name: &str) -> &[Self] {
        static EMPTY_MATCH_SLICE: &[Match] = &[];
        self.get(name).map(|v| v.as_slice()).unwrap_or(EMPTY_MATCH_SLICE)
    }

    pub fn iter(&self, name: &str) -> impl Iterator<Item = &Match> {
        self.get_or_empty(name).iter()
    }

    pub fn iter_rev(&self, name: &str) -> impl Iterator<Item = &Match> {
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

// TODO: Remove this since it's not remotely constant-time.
impl<'a> Index<&'a str> for Match {
    type Output = Vec<Self>;

    fn index(&self, index: &'a str) -> &Self::Output {
        self.get(index).unwrap()
    }
}

impl Down for Match {
    fn down(&self, _idx: usize) -> Option<&Self> {
        None
    }
}

pub(crate) struct ParseNode {
    child: Option<Box<ParseNode>>,
    m: Match,
}

impl DownMut for ParseNode {
    fn down_mut(&mut self, _idx: usize) -> Option<&mut Self> {
        let c: Option<&mut ParseNode> =
            self.child
            .as_mut()
            .map(|&mut b| &mut *b);
        if c.is_some() {
            c
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

impl ParseNode {
    pub(crate) fn new() -> Self {
        Self {
            child: None,
            m: Match::new((Pos::empty(), Pos::empty()), vec![]),
        }
    }
}

struct MatchPos<'x> {
    g: LinkTreeCursor<'x, GrammarNode>,
    m: Vec<Match>,
    mp: TreeCursorPos,
}

struct MatchCursor<'x> {
    g: LinkTreeCursor<'x, GrammarNode>,
    m: TreeCursorMut<'x, 'x, ParseNode>,
}

impl<'x> MatchCursor<'x> {
    fn new(
        named: &'x [(impl Borrow<str>, GrammarNode)],
        start: &str,
        pnroot: &'x mut ParseNode,
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

    fn up(&mut self) -> bool {
        if self.g.up() {
            assert!(self.m.up());
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

    fn pos(&self) -> MatchPos<'x> {
        let x = MatchPos {
            g: self.g.clone(),
            mp: self.m.pos(),
        };
        MatchPos {
            g: self.g.clone(),
            m: self.m.pos(),
        }
    }

    /// panics on failure
    fn set_pos(&mut self, pos: &MatchPos<'x>) {
        self.g = pos.g.clone();
        self.m.set_pos(&pos.m)
    }
}

pub fn empty_slice<'a, T>() -> &'a [T] {
    &[]
}

// TODO
#[derive(Debug)]
pub enum ParseError {
    BadGrammar(LinkError),
    MatchFail(Match, Pos, Vec<GrammarAtom>),
}

pub struct Parser<'x, 's> {
    c: MatchCursor<'x>,
    input: &'s str,
    fail_cause: Option<MatchPos<'x>>,
}

impl<'x, 's> Parser<'x, 's> {
    pub fn parse(
        named: &[(impl Borrow<str>, GrammarNode)],
        start: &str,
        input: &str,
    ) -> Result<Match, ParseError> {
        let mut pn = ParseNode::new();
        let mut p = Parser::new(named, start, input, &mut pn).map_err(
            |e| ParseError::BadGrammar(e)
        )?;

        loop {
            let success = p.try_match();

            match p.step(success, false) {
                Some(Ok(m)) => return Ok(m),
                Some(Err(m)) => {
                    // TODO
                    let pos;
                    let initial;
                    if let Some(cause) = p.fail_cause.take() {
                        p.c.set_pos(&cause);
                        pos = p.c.m.get().m.raw.1.clone();
                        let mut start = p.c.pos();
                        while p.c.m.get().m.is_empty() {
                            // Ugh I hate this.
                            println!("{:?}", p.c.m.get().m);
                            println!("{:?}", p.c.g.get());
                            start = p.c.pos();
                            if !p.c.up() {
                                break;
                            }
                        }
                        p.c.set_pos(&start);
                        p.c.zero();
                        while p.c.down() { }
                        initial = p.initial();
                    } else {
                        pos = m.raw.1.clone();
                        initial = vec![];
                    }
                    return Err(ParseError::MatchFail(m, pos, initial));
                },
                None => (),
            }
        }
    }

    pub fn initial(&mut self) -> Vec<GrammarAtom> {
        let mut atoms = vec![];

        let pos = self.c.pos();

        'outer: for count in 0.. {
            self.c.set_pos(&pos);

            for i in 0..=count {
                if i < count {
                    match self.step(false, false) {
                        None => (),
                        Some(Ok(_)) => {
                            atoms.push(GrammarAtom::Text("".to_string()));
                            break 'outer;
                        },
                        Some(Err(m)) => {
                            if m.is_empty() {
                                break 'outer
                            } else {
                                panic!()
                            }
                        },
                    }
                } else {
                    let atom;
                    if let &GrammarNode::Atom(ref a) = self.c.g.get() {
                        atom = a.clone();
                    } else { panic!(); }
                    let last;
                    loop {
                        if let Some(x) = self.step(true, true) {
                            last = x;
                            break;
                        }
                    }
                    match last {
                        Ok(_) => atoms.push(atom),
                        Err(m) => {
                            if m.is_empty() {
                                ()
                            } else {
                                atoms.push(atom)
                            }
                        },
                    }
                }
            }
        }

        return atoms;
    }

    /// `None` means keep going. `Some(Ok(_))` means success. `Some(Err(_))`
    /// means there was a parse error.
    pub(crate) fn step(
        &mut self,
        mut success: bool,
        suppress_down: bool,
    ) -> Option<Result<Match, Match>> {
        loop {
            let a = if success {
                self.c.g.get().action()
            } else if self.c.m.get().m.is_empty() {
                self.c.g.get().fail_empty_action()
            } else {
                self.c.g.get().fail_action()
            };

            // TODO: What's the perf cost here?
            if !a.success {
                if self.fail_cause.is_none() {
                    self.fail_cause = Some(self.c.pos());
                }
            }

            if self.do_action(a, suppress_down) {
                self.fail_cause = None;
                return None;
            }

            if let Some(r) = self.go_up(a) {
                return Some(r);
            }

            success = a.success;
        }
    }

    pub(crate) fn new(
            named: &'x [(impl Borrow<str>, GrammarNode)],
            start: &str,
            input: &'s str,
            pnroot: &'x mut ParseNode,
    ) -> Result<Self, LinkError> {
        let mut p = Parser {
            c: MatchCursor::new(named, start, pnroot)?,
            input,
            fail_cause: None,
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

    /// `None` means we reached an atom and should try to match it. `Some(_)`
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

        false
    }

    /// `None` means we went up. `Some(Ok(_))` means the parse was successful.
    /// `Some(Err(_))` means there was a parse error.
    fn go_up(&mut self, a: Action) -> Option<Result<Match, Match>> {
        let old_st = mem::replace(&mut self.c.m.get_mut().m, Match::empty());

        if !self.c.up() {
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

    fn combine_st(&mut self, mut old_st: Match) {
        use GrammarNode::*;

        let new_st = &mut self.c.m.get_mut().m;

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

pub(super) fn get_grammar_grammar() -> Vec<(&'static str, GrammarNode)> {
    let mut gg = get_utils();

    gg.append(&mut vec![
        ("comment", e(vec![
            t("#"),
            s(e(vec![
                n(t("\n")),
                a(),
            ])),
        ])),

        ("wso_part", c(vec![
            t(" "),
            t("\t"),
        ])),
        ("ws_part", c(vec![
            k("wso_part"),
            e(vec![
                q(k("comment")),
                t("\n"),
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
            t("0x"),
            p(k("hex_digit")),
        ])),

        ("str", e(vec![
            t("\""),
            s(u("cp", c(vec![
                e(vec![
                    t("\\"),
                    c(vec![
                        t("n"),
                        t("t"),
                        t("\\"),
                        t("\""),
                    ]),
                ]),
                e(vec![
                    n(t("\"")),
                    n(t("\n")),
                    a(),
                ]),
            ]))),
            t("\""),
        ])),
        ("cp", c(vec![
            u("hex", k("hex_uint")),
            e(vec![
                t("'"),
                u("raw", c(vec![
                    e(vec![
                        t("\\"),
                        c(vec![
                            t("n"),
                            t("t"),
                            t("\\"),
                            t("'"),
                        ]),
                    ]),
                    e(vec![
                        n(t("'")),
                        n(t("\n")),
                        a(),
                    ]),
                ])),
                t("'"),
            ]),
        ])),
        ("cp_range", e(vec![
            u("from", k("cp")),
            t(".."),
            u("to", k("cp")),
        ])),
        ("ident_initial", c(vec![
            k("latin_letter"),
            t("_"),
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
                t("/"),
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
                    t("="),
                ])),
            ])),
        ])),
        ("expr_affix", e(vec![
            s(u("pre", c(vec![
                t("^"),
                t("-"),
            ]))),
            u("opd", k("expr_atom")),
            s(u("suf", c(vec![
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
        ("expr_atom", c(vec![
            t("%"),
            k("str"),
            u("r", k("cp_range")),
            u("id", k("ident")),
            e(vec![
                t("("),
                k("ws"),
                u("expr", k("expr")),
                k("ws"),
                t(")"),
            ]),
        ])),

        ("rule", e(vec![
            u("name", k("ident")),
            k("wso"),
            t("="),
            k("ws"),
            u("val", k("expr")),
        ])),
        ("grammar", e(vec![
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
                c => c, // note: grammar should be more restrictive than this
            }
        },
        c => {
            assert!(ss.len() == 1);
            c
        }
    }
}

fn parse_expr(
    input: &str,
    expr: &Match,
    gc: &mut TreeCursorMut<GrammarNode>,
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
                let mut old_gc = gc;
                gc = old_gc.split_below().unwrap();
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
                        u(suf["name"][0].raw(input), a())
                    },
                };
                // down
                let mut old_gc = gc;
                gc = old_gc.split_below().unwrap();
            }

            let atom = &af["opd"][0];

            fn first<'a>(v: &'a Vec<Match>) -> &'a Match {
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
                *gc.get_mut() = t(&s);
            } else if let Some(rr) = atom.get("r").map(first) {
                let ff = &rr["from"][0];
                let tt = &rr["to"][0];
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
                parse_expr(input, &inner_expr, &mut gc)?;
            }
        }
    }

    Ok(())
}

pub(super) fn parse_grammar_with_grammar<S>(
    gg: &[(S, GrammarNode)], input: &str
) -> Result<Vec<(String, GrammarNode)>, ParseError>
where
    S: Borrow<str>,
{
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
        parse_expr(input, &cval, &mut gc)?;
    }

    Ok(g)
}

pub fn parse_grammar(
    input: &str
) -> Result<Vec<(String, GrammarNode)>, ParseError> {
    let gg = get_grammar_grammar();
    parse_grammar_with_grammar(&gg, input)
}

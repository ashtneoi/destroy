use Action;
use get_utils;
use link_tree::{LinkError, LinkTreeCursor};
use prelude::*;
use std::borrow::Borrow;
use std::char;
use std::fmt;
use std::ops::Index;
use tree_cursor::prelude::*;
use tree_cursor::cursor::TreeCursorMut;

static EMPTY_MATCH_SLICE: &[Match] = &[];

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

struct MatchCursor<'x> {
    g: LinkTreeCursor<'x, GrammarNode>,
    m: TreeCursorMut<'x, MatchNode>,
}

impl<'x> MatchCursor<'x> {
    fn new(g: LinkTreeCursor<'x, GrammarNode>, m: TreeCursorMut<'x, MatchNode>)
            -> Self
    {
        MatchCursor { g, m }
    }

    fn zero(&mut self) {
        self.g.zero();
        self.m.zero();
    }

    fn up(&mut self) -> bool {
        self.g.up() && self.m.up()
    }

    fn down(&mut self) -> bool {
        self.g.down() && self.m.down()
    }
}

struct Parser<'x, 's> {
    c: MatchCursor<'x>,
    pos: Pos,
    input: &'s str,
}

impl<'x, 's> Parser<'x, 's> {
    fn new(
            named: &'x [(impl Borrow<str>, GrammarNode)],
            start: &str,
            input: &'s str,
            mroot: &'x mut MatchNode,
    ) -> Result<Self, LinkError> {
        let mut c = MatchCursor::new(
            LinkTreeCursor::new(named, start)?,
            TreeCursorMut::new(mroot),
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

// TODO
#[derive(Debug)]
pub enum ParseError {
    BadGrammar(LinkError),
    MatchFail(Pos),
    UnmatchedInput(Match),
}

pub fn parse(
    named: &[(impl Borrow<str>, GrammarNode)],
    start: &str,
    input: &str,
) -> Result<Match, ParseError> {
    let mut m = MatchNode::new();
    let mut p = Parser::new(named, start, input, &mut m).map_err(
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

pub(super) fn get_grammar_grammar() -> Vec<(&'static str, GrammarNode)> {
    let mut gg = get_utils();

    gg.append(&mut vec![
        ("comment", e(vec![
            t("#"),
            s(e(vec![
                g(t("\n")),
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
                    g(t("\"")),
                    g(t("\n")),
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
                        g(t("'")),
                        g(t("\n")),
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
            u("e", k("expr_seq")),
            s(e(vec![
                k("ws"),
                t("/"),
                k("ws"),
                u("e", k("expr_seq")),
            ])),
        ])),
        ("expr_seq", e(vec![
            u("pre", k("expr_prefix")),
            s(e(vec![
                k("pws"),
                u("pre", k("expr_prefix")),
                g(e(vec![
                    k("wso"),
                    t("="),
                ])),
            ])),
        ])),
        ("expr_prefix", e(vec![
            s(u("op", c(vec![
                t("^"),
                t("-"),
            ]))),
            u("suf", k("expr_suffix")),
        ])),
        ("expr_suffix", e(vec![
            u("atom", k("expr_atom")),
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
        ("expr_atom", c(vec![
            t("%"),
            k("str"),
            u("r", k("cp_range")),
            u("id", k("ident")),
            e(vec![
                t("("),
                k("ws"),
                u("c", k("expr")),
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

    for ee in expr.iter("e") {
        // push c
        if let &mut Choice(ref mut v) = gc.get_mut() {
            v.push(e(vec![]));
        } else { panic!(); }
        // down
        let mut gc = gc.down_new().unwrap();

        for pre in ee.iter("pre") {
            // push a
            if let &mut Seq(ref mut v) = gc.get_mut() {
                v.push(a()); // placeholder
            } else { panic!(); }
            // down
            let mut gc = gc.down_new().unwrap();

            for op in pre.iter("op") {
                // change a to op(a)
                *gc.get_mut() = match op.raw(input) {
                    "^" => z(a()),
                    "-" => g(a()),
                    _ => panic!(),
                };
                // down
                let mut old_gc = gc;
                gc = old_gc.down_new().unwrap();
            }

            let suf = &pre["suf"][0];

            for op in suf.iter_rev("op") {
                // change a to op(a)
                *gc.get_mut() = match op.raw(input) {
                    "*" => s(a()),
                    "+" => p(a()),
                    "?" => q(a()),
                    name => {
                        assert!(name.starts_with("["));
                        assert!(name.ends_with("]"));
                        u(op["name"][0].raw(input), a())
                    },
                };
                // down
                let mut old_gc = gc;
                gc = old_gc.down_new().unwrap();
            }

            for atom in suf.iter("atom") {
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
                } else if let Some(cc) = atom.get("c").map(first) {
                    // change a to c([])
                    *gc.get_mut() = c(vec![]);
                    // recurse
                    parse_expr(input, &cc, &mut gc)?;
                }
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

    let st = match parse(&gg, "grammar", input) {
        Ok(x) => x,
        Err(ParseError::BadGrammar(e)) => panic!("{:?}", e),
        Err(e) => return Err(e),
    };

    let mut g = Vec::<(String, GrammarNode)>::new();

    let rule_count = st.count("name");
    assert_eq!(rule_count, st.count("val"));

    for (cname, cval) in st.iter("name").zip(st.iter("val")) {
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

use std::borrow::Borrow;
use std::collections::HashMap;
use tree_cursor::cursor::TreeCursor;
use tree_cursor::prelude::*;

pub(super) trait Link {
    fn target(&self) -> Option<&str>;
}

pub(super) type LinkMap<X> = HashMap<String, X>;

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum LinkError {
    DuplicateName(String),
    BrokenLink(String),
}

#[derive(Clone, Debug)]
pub(super) struct LinkTreeCursor<'n, N: 'n + Down + Link> {
    tree_cursor: TreeCursor<'n, 'n, N>,
    link_map: LinkMap<&'n N>,
}

impl<'n, N: 'n + Down + Link> LinkTreeCursor<'n, N> {
    pub fn new(
        named: &'n [(impl Borrow<str>, N)], start: &str
    ) -> Result<Self, LinkError> {
        let mut link_map = LinkMap::new();

        for &(ref name, ref node) in named {
            let name = name.borrow();
            if !link_map.insert(name.to_string(), node).is_none() {
                return Err(LinkError::DuplicateName(name.to_string()));
            }
        }

        for &(_, ref node) in named {
            let mut c = TreeCursor::new(node);
            loop {
                while c.down() { }

                if let Some(target) = c.get().target() {
                    if !link_map.contains_key(target) {
                        return Err(LinkError::BrokenLink(target.to_string()));
                    }
                }

                if !c.up() { break; }
            }
        }

        let start_node = match link_map.get(start) {
            Some(n) => n,
            None => return Err(LinkError::BrokenLink(start.to_string())),
        };

        Ok(Self { tree_cursor: TreeCursor::new(start_node), link_map })
    }

    pub fn zero(&mut self) {
        self.tree_cursor.zero();
    }

    pub fn up(&mut self) -> bool {
        self.tree_cursor.up()
    }

    pub fn get(&self) -> &N {
        self.tree_cursor.get()
    }

    pub fn down(&mut self) -> bool {
        let m = &self.link_map; // borrowck workaround
        self.tree_cursor.down_with(|node, idx| {
            node.target().map(|target| {
                m[target]
            }).filter(|_| idx == 0)
        }) || self.tree_cursor.down()
    }
}

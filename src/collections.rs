use std::collections::HashMap;
use std::ptr;

pub trait Down {
    fn down(&self, idx: usize) -> Option<&Self>;
}

pub trait Link {
    fn name(&self) -> Option<&str>;
    fn target(&self) -> Option<&str>;
}

type LinkMap<X> = HashMap<String, X>;

pub struct TreeCursor<'n, N: 'n + Down + Link> {
    stack: Vec<(&'n N, usize)>,
    link_map: Option<LinkMap<&'n N>>,
}

impl <'n, N: 'n + Down + Link> TreeCursor<'n, N> {
    pub fn new(root: &'n N, link_map: Option<LinkMap<&'n N>>) -> Self {
        TreeCursor { stack: vec![(root, 0)], link_map }
    }

    pub fn get(&self) -> &'n N {
        self.stack.last().unwrap().0
    }

    pub fn up(&mut self) -> bool {
        if self.stack.len() == 1 {
            false
        } else {
            self.stack.pop();
            true
        }
    }

    pub fn down(&mut self) -> bool {
        let (ref mut last, ref mut idx) = self.stack.last_mut().unwrap();
        let maybe_next =
            if let (Some(target), &Some(ref link_map)) =
                    (last.target(), &self.link_map)
            {
                Some(link_map[target])
            } else {
                let d = last.down(*idx);
                if d.is_some() {
                    *idx += 1;
                }
                d
            };

        if let Some(next) = maybe_next {
            self.stack.push((next, 0));
            true
        } else {
            false
        }
    }
}

pub enum LinkMapError {
    BrokenLink,
    DuplicateName,
}

pub fn build_link_map<N: Down + Link>(root: &N)
    -> Result<LinkMap<&N>, LinkMapError>
{
    let mut c = TreeCursor::new(root, None);
    let mut m = LinkMap::<&N>::new();
    let mut targets = Vec::new();

    'outer1: loop {
        let node = c.get();
        if let Some(name) = node.name() {
            if m.insert(name.to_string(), node).is_some() {
                return Err(LinkMapError::DuplicateName)
            }
        } else if let Some(target) = node.target() {
            targets.push(target);
        }

        while !c.down() {
            if !c.up() {
                break 'outer1;
            }
        }
    }

    assert!(ptr::eq(root, c.get()));

    for target in targets {
        if !m.contains_key(target) {
            return Err(LinkMapError::BrokenLink)
        }
    }

    Ok(m)
}

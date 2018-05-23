use std::collections::HashMap;
use std::marker::PhantomData;
use std::ptr;

pub mod prelude {
    pub use super::{
        Down,
        Link,
        LinkError,
        LinkTreeCursor,
        MutVerticalCursor,
        OpaqueVerticalCursor,
        MutTreeCursor,
        VerticalCursor,
    };
}

pub trait Down {
    fn down(&mut self, idx: usize) -> Option<*mut Self>;
}

pub trait Link {
    fn name(&self) -> Option<&str>;
    fn target(&self) -> Option<&str>;
}

pub trait OpaqueVerticalCursor {
    fn zero(&mut self);
    fn down(&mut self) -> bool;
    fn up(&mut self) -> bool;
}

pub trait VerticalCursor<'n>: OpaqueVerticalCursor {
    type Item;

    fn get(&self) -> &'n Self::Item;
}

pub trait MutVerticalCursor<'n>: VerticalCursor<'n> {
    fn get_mut(&mut self) -> &'n mut Self::Item;
}

#[derive(Debug)]
pub struct MutTreeCursor<'n, N: 'n + Down> {
    root: PhantomData<&'n mut N>,
    stack: Vec<(*mut N, usize)>,
}

impl<'n, N: 'n + Down> MutTreeCursor<'n, N> {
    pub fn new(root: &'n mut N) -> Self {
        let root_ptr: *mut N = root;
        MutTreeCursor { root: PhantomData, stack: vec![(root_ptr, 0)] }
    }
}

impl<'n, N: 'n + Down> OpaqueVerticalCursor for MutTreeCursor<'n, N> {
    fn zero(&mut self) {
        self.stack.last_mut().unwrap().1 = 0;
    }

    fn down(&mut self) -> bool {
        let idx = self.stack.last().unwrap().1;
        let new_ptr = match self.get_mut().down(idx) {
            Some(x) => x,
            None => return false,
        };

        self.stack.last_mut().unwrap().1 += 1;
        self.stack.push((new_ptr, 0));
        true
    }

    fn up(&mut self) -> bool {
        if self.stack.len() == 1 {
            self.stack[0].1 = 0;
            false
        } else {
            self.stack.pop().unwrap();
            true
        }
    }
}

impl<'n, N: 'n + Down> VerticalCursor<'n> for MutTreeCursor<'n, N> {
    type Item = N;

    fn get(&self) -> &'n N {
        let here: *const N = self.stack.last().unwrap().0;
        (unsafe { here.as_ref() }).unwrap()
    }

}

impl<'n, N: 'n + Down> MutVerticalCursor<'n> for MutTreeCursor<'n, N> {
    fn get_mut(&mut self) -> &'n mut N {
        let here = self.stack.last().unwrap().0;
        (unsafe { here.as_mut() }).unwrap()
    }
}

type LinkMap<X> = HashMap<String, X>;

#[derive(Debug, PartialEq)]
pub enum LinkError {
    DuplicateName,
    BrokenLink,
    NameWithoutChild,
}

#[derive(Debug)]
pub struct LinkTreeCursor<'n, N: 'n + Down + Link> {
    tree_cursor: MutTreeCursor<'n, N>,
    link_map: LinkMap<*mut N>,
}

impl<'n, N: 'n + Down + Link> LinkTreeCursor<'n, N> {
    pub fn new(root: &'n mut N, start: &str) -> Result<Self, LinkError> {
        let mut c = MutTreeCursor::new(root);
        let mut link_map = LinkMap::<*mut N>::new();

        let mut targets = Vec::new();

        let mut child: *mut N = ptr::null_mut();

        loop {
            while c.down() { child = ptr::null_mut(); }

            let here = c.get_mut();
            if let Some(name) = match here.name() {
                    Some(n) => Some(n.to_string()),
                    None => None,
            } {
                if child.is_null() {
                    return Err(LinkError::NameWithoutChild);
                }
                if link_map.insert(name, child).is_some() {
                    return Err(LinkError::DuplicateName);
                }
            }
            if let Some(target) = here.target() {
                targets.push(target.to_string());
            }

            child = here as *mut N;

            if !c.up() {
                break;
            }
        }

        for target in targets {
            if !link_map.contains_key(&target) {
                return Err(LinkError::BrokenLink);
            }
        }

        let start_node = match link_map.get(start) {
            Some(n) => (unsafe { n.as_mut() }).unwrap(),
            None => return Err(LinkError::BrokenLink),
        };

        Ok(Self { tree_cursor: MutTreeCursor::new(start_node), link_map })
    }
}

impl<'n, N: 'n + Down + Link> OpaqueVerticalCursor for LinkTreeCursor<'n, N> {
    fn zero(&mut self) {
        self.tree_cursor.zero();
    }

    fn down(&mut self) -> bool {
        match self.tree_cursor.get().target()
                .filter(|_| self.tree_cursor.stack.last().unwrap().1 == 0)
                .map(|target| self.link_map[target])
        {
            Some(new_ptr) => {
                self.tree_cursor.stack.last_mut().unwrap().1 += 1;
                self.tree_cursor.stack.push((new_ptr, 0));
                true
            },
            None => self.tree_cursor.down(),
        }
    }

    fn up(&mut self) -> bool {
        self.tree_cursor.up()
    }
}

impl<'n, N: 'n + Down + Link> VerticalCursor<'n> for LinkTreeCursor<'n, N> {
    type Item = N;

    fn get(&self) -> &'n N {
        self.tree_cursor.get()
    }
}

impl<'n, N: 'n + Down + Link> MutVerticalCursor<'n>
    for LinkTreeCursor<'n, N>
{
    fn get_mut(&mut self) -> &'n mut N {
        self.tree_cursor.get_mut()
    }
}

pub trait MutVerticalCursorGroup {
    fn list(&mut self) -> Vec<&mut OpaqueVerticalCursor>;

    fn zero(&mut self) {
        for c in self.list().iter_mut() {
            c.zero();
        }
    }

    fn down(&mut self) -> bool {
        for c in self.list().iter_mut() {
            if !c.down() {
                return false;
            }
        }
        true
    }

    fn up(&mut self) -> bool {
        for c in self.list().iter_mut() {
            if !c.up() {
                return false;
            }
        }
        true
    }
}

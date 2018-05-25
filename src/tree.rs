use std::collections::HashMap;
use std::marker::PhantomData;
use std::ptr;

pub mod prelude {
    pub use super::{
        Down,
        DownMut,
        Link,
        LinkError,
        LinkTreeCursor,
        MutVerticalCursor,
        OpaqueVerticalCursor,
        MutTreeCursor,
        TreeCursor,
        VerticalCursor,
    };
}

pub trait Down {
    fn down(&self, idx: usize) -> Option<&Self>;
}

pub trait DownMut {
    fn down_mut(&mut self, idx: usize) -> Option<&mut Self>;
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

pub trait VerticalCursor<'n>: OpaqueVerticalCursor
where
    Self: Sized,
{
    type Item;

    fn get(&self) -> &Self::Item;
    fn map_down<F>(&mut self, f: F)
        where F: Fn(&Self::Item) -> &Self::Item;
    fn new_here(&mut self) -> Option<Self>;
    fn new_down(&mut self) -> Option<Self> {
        if self.down() {
            self.new_here().or_else(|| panic!())
        } else {
            None
        }
    }
}

pub trait MutVerticalCursor<'n>: VerticalCursor<'n> {
    fn get_mut(&mut self) -> &mut Self::Item;
}

#[derive(Debug)]
pub struct TreeCursor<'n, N: 'n + Down> {
    root: PhantomData<&'n N>,
    stack: Vec<(*const N, usize)>,
}

impl<'n, N: 'n + Down> TreeCursor<'n, N> {
    pub fn new(root: &'n N) -> Self {
        let root_ptr: *const N = root;
        Self { root: PhantomData, stack: vec![(root_ptr, 0)] }
    }

    fn up_get(&mut self) -> Option<&'n N> {
        if self.stack.len() == 1 {
            self.stack[0].1 = 0;
            None
        } else {
            let here: *const N = self.stack.pop().unwrap().0;
            Some((unsafe { here.as_ref() }).unwrap())
        }
    }
}

impl<'n, N: 'n + Down> OpaqueVerticalCursor for TreeCursor<'n, N> {
    fn zero(&mut self) {
        self.stack.last_mut().unwrap().1 = 0;
    }

    fn down(&mut self) -> bool {
        let idx = self.stack.last().unwrap().1;
        let new_ptr = match self.get().down(idx) {
            Some(x) => x as *const N,
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

impl<'n, N: 'n + Down> VerticalCursor<'n> for TreeCursor<'n, N> {
    type Item = N;

    fn get(&self) -> &N {
        let here: *const N = self.stack.last().unwrap().0;
        (unsafe { here.as_ref() }).unwrap()
    }

    fn map_down<F>(&mut self, f: F)
    where
        F: Fn(&Self::Item) -> &Self::Item
    {
        let new_ptr = f(self.get()) as *const N;
        self.stack.push((new_ptr, 0));
    }

    fn new_here(&mut self) -> Option<Self> {
        /*
        match self.up_get() {
            Some(root) => Some(Self::new(root)),
            None => {
                self.stack[0].1 = 0;
                None
            }
        }
        */
        let here = self.up_get();
        if here.is_none() {
            self.stack[0].1 = 0;
        }
        here.map(|h| Self::new(h))
    }
}

#[derive(Debug)]
pub struct MutTreeCursor<'n, N: 'n + DownMut> {
    root: PhantomData<&'n mut N>,
    stack: Vec<(*mut N, usize)>,
}

impl<'n, N: 'n + DownMut> MutTreeCursor<'n, N> {
    pub fn new(root: &'n mut N) -> Self {
        let root_ptr: *mut N = root;
        Self { root: PhantomData, stack: vec![(root_ptr, 0)] }
    }

    fn up_get(&mut self) -> Option<&'n mut N> {
        if self.stack.len() == 1 {
            self.stack[0].1 = 0;
            None
        } else {
            let here: *mut N = self.stack.pop().unwrap().0;
            Some((unsafe { here.as_mut() }).unwrap())
        }
    }
}

impl<'n, N: 'n + DownMut> OpaqueVerticalCursor for MutTreeCursor<'n, N> {
    fn zero(&mut self) {
        self.stack.last_mut().unwrap().1 = 0;
    }

    fn down(&mut self) -> bool {
        let idx = self.stack.last().unwrap().1;
        let new_ptr = match self.get_mut().down_mut(idx) {
            Some(x) => x as *mut N,
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

impl<'n, N: 'n + DownMut> VerticalCursor<'n> for MutTreeCursor<'n, N> {
    type Item = N;

    fn get(&self) -> &N {
        let here: *const N = self.stack.last().unwrap().0;
        (unsafe { here.as_ref() }).unwrap()
    }

    fn map_down<F>(&mut self, f: F)
    where
        F: Fn(&Self::Item) -> &Self::Item
    {
        let new_ptr = f(self.get()) as *const N as *mut N;
        self.stack.push((new_ptr, 0));
    }

    fn new_here(&mut self) -> Option<Self> {
        self.up_get().map(|h| Self::new(h))
    }
}

impl<'n, N: 'n + DownMut> MutVerticalCursor<'n> for MutTreeCursor<'n, N> {
    fn get_mut(&mut self) -> &mut N {
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
    tree_cursor: TreeCursor<'n, N>,
    link_map: LinkMap<*const N>,
}

impl<'n, N: 'n + Down + Link> LinkTreeCursor<'n, N> {
    pub fn new(root: &'n N, start: &str) -> Result<Self, LinkError> {
        let mut c = TreeCursor::new(root);
        let mut link_map = LinkMap::<*const N>::new();

        let mut targets = Vec::new();

        let mut child: *const N = ptr::null();

        loop {
            while c.down() { child = ptr::null(); }

            let here = c.get();
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

            child = here as *const N;

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
            Some(n) => (unsafe { n.as_ref() }).unwrap(),
            None => return Err(LinkError::BrokenLink),
        };

        Ok(Self { tree_cursor: TreeCursor::new(start_node), link_map })
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

    fn get(&self) -> &N {
        self.tree_cursor.get()
    }

    fn map_down<F>(&mut self, f: F)
    where
        F: Fn(&Self::Item) -> &Self::Item
    {
        self.tree_cursor.map_down(f);
    }

    fn new_here(&mut self) -> Option<Self> {
        unimplemented!(); // TODO
    }
}

pub trait OpaqueVerticalCursorGroup {
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

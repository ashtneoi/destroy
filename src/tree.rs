use std::collections::HashMap;
use std::marker::PhantomData;

pub trait Down {
    fn down(&mut self, idx: usize) -> Option<*mut Self>;
}

pub trait Link {
    fn name(&self) -> Option<&str>;
    fn target(&self) -> Option<&str>;
}

#[derive(Debug)]
pub struct TreeCursor<'n, N: 'n + Down> {
    root: PhantomData<&'n mut N>,
    stack: Vec<(*mut N, usize)>,
}

impl<'n, N: 'n + Down> TreeCursor<'n, N> {
    pub fn new(root: &'n mut N) -> Self {
        let root_ptr: *mut N = root;
        TreeCursor { root: PhantomData, stack: vec![(root_ptr, 0)] }
    }

    pub fn get(&self) -> &'n N {
        let here: *const N = self.stack.last().unwrap().0;
        (unsafe { here.as_ref() }).unwrap()
    }

    pub fn get_mut(&mut self) -> &'n mut N {
        let here = self.stack.last().unwrap().0;
        (unsafe { here.as_mut() }).unwrap()
    }

    pub fn zero(&mut self) {
        self.stack.last_mut().unwrap().1 = 0;
    }

    pub fn down(&mut self) -> bool {
        let idx = self.stack.last().unwrap().1;
        let new_ptr = match self.get_mut().down(idx) {
            Some(x) => x,
            None => return false,
        };

        self.stack.last_mut().unwrap().1 += 1;
        self.stack.push((new_ptr, 0));
        true
    }

    pub fn up(&mut self) -> bool {
        if self.stack.len() == 1 {
            self.stack[0].1 = 0;
            false
        } else {
            self.stack.pop().unwrap();
            true
        }
    }
}

type LinkMap<X> = HashMap<String, X>;

#[derive(Debug, PartialEq)]
pub enum LinkError {
    DuplicateName,
    BrokenLink,
}

#[derive(Debug)]
pub struct LinkTreeCursor<'n, N: 'n + Down + Link> {
    tree_cursor: TreeCursor<'n, N>,
    link_map: LinkMap<*mut N>,
}

impl<'n, N: 'n + Down + Link> LinkTreeCursor<'n, N> {
    pub fn new(root: &'n mut N, start: &str) -> Result<Self, LinkError> {
        let mut c = TreeCursor::new(root);
        let mut link_map = LinkMap::<*mut N>::new();

        let mut targets = Vec::new();

        loop {
            while c.down() { }

            let here = c.get_mut();
            if let Some(name) = match here.name() {
                    Some(n) => Some(n.to_string()),
                    None => None,
            } {
                if link_map.insert(name, here).is_some() {
                    return Err(LinkError::DuplicateName);
                }
            }
            if let Some(target) = here.target() {
                targets.push(target.to_string());
            }

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

        Ok(Self { tree_cursor: TreeCursor::new(start_node), link_map })
    }

    pub fn get(&self) -> &'n N {
        self.tree_cursor.get()
    }

    pub fn get_mut(&mut self) -> &'n mut N {
        self.tree_cursor.get_mut()
    }

    pub fn zero(&mut self) {
        self.tree_cursor.zero();
    }

    pub fn down(&mut self) -> bool {
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

    pub fn up(&mut self) -> bool {
        self.tree_cursor.up()
    }
}

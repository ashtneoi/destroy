use grow_collections::GrowHashSet;
use std::cmp::Ordering;
use std::hash::{Hash, Hasher};

#[derive(Clone, Debug, Eq, Ord)]
pub struct StringTableEntry(pub String, pub usize);

impl PartialEq for StringTableEntry {
    fn eq(&self, other: &Self) -> bool {
        &self.0 == &other.0
    }
}

impl PartialOrd for StringTableEntry {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.0.partial_cmp(&other.0)
    }
}

impl Hash for StringTableEntry {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.hash(state);
    }
}

pub struct StringTable<'t> {
    h: GrowHashSet<'t, StringTableEntry>,
    count: usize,
}

impl<'t> StringTable<'t> {
    pub fn new() -> Self {
        Self {
            h: GrowHashSet::with_capacity(256),
            count: 0,
        }
    }

    pub fn insert(&mut self, s: String) -> &'t StringTableEntry {
        let x = self.h.insert(StringTableEntry(s, self.count));
        if x.1 == self.count {
            self.count += 1;
        }
        x
    }
}

#[cfg(test)]
#[test]
fn test_insert() {
    use std::ptr;

    let mut t = StringTable::new();
    let f1 = t.insert("foo".to_string());
    let f2 = t.insert("foo".to_string());
    let b1 = t.insert("bar".to_string());
    let b2 = t.insert("bar".to_string());
    let q1 = t.insert("qux".to_string());
    let q2 = t.insert("qux".to_string());
    assert_eq!(f1, &StringTableEntry("foo".to_string(), 0));
    assert_eq!(f1.1, f2.1);
    assert!(ptr::eq(f1, f2));
    assert_eq!(b1, &StringTableEntry("bar".to_string(), 1));
    assert_eq!(b1.1, b2.1);
    assert!(ptr::eq(b1, b2));
    assert_eq!(q1, &StringTableEntry("qux".to_string(), 2));
    assert_eq!(f1.1, f2.1);
    assert!(ptr::eq(q1, q2));
}

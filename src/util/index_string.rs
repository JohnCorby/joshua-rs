use std::cell::RefCell;
use std::ops::Deref;
use std::rc::Rc;

#[derive(Debug, Clone, Default)]
pub struct IndexStringIndex(Rc<RefCell<usize>>);

/// string that can give indexes that will move with inserts and stuff
#[derive(Debug, Default)]
pub struct IndexString {
    string: String,
    indexes: Vec<IndexStringIndex>,
}

impl IndexString {
    pub fn make_index(&mut self, index: usize) -> IndexStringIndex {
        let index = IndexStringIndex(Rc::new(RefCell::new(index)));
        self.indexes.push(index.clone());
        index
    }

    pub fn push(&mut self, ch: char) {
        self.string.push(ch)
    }
    pub fn push_str(&mut self, string: &str) {
        self.string.push_str(string)
    }
    pub fn pop(&mut self) -> Option<char> {
        self.string.pop()
    }
    pub fn insert_str(&mut self, idx: IndexStringIndex, string: &str) {
        let idx = *idx.0.borrow();
        self.string.insert_str(idx, string);

        for index in &self.indexes {
            let mut index = index.0.borrow_mut();
            if *index >= idx {
                *index += string.len()
            }
        }
    }
}

impl Deref for IndexString {
    type Target = str;
    fn deref(&self) -> &Self::Target {
        &self.string
    }
}

#[cfg(test)]
#[test]
fn tests() {
    let mut s = IndexString::default();
    s.push_str("hello world!");
    let i1 = s.make_index(s.len());
    assert_eq!(*i1.0.borrow(), 12);

    s.push_str(" here's another part!");
    assert_eq!(*i1.0.borrow(), 12);
    let i2 = s.make_index(0);
    s.insert_str(i2, "this part is before! ");
    assert_eq!(*i1.0.borrow(), 21 + 12);
}

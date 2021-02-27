use std::cell::RefCell;
use std::ops::Deref;
use std::rc::Rc;

/// string that can give indexes that will move with inserts and stuff
#[derive(Debug, Default)]
pub struct IndexString {
    string: String,
    indexes: Vec<Rc<RefCell<usize>>>,
}

impl IndexString {
    /// note: do not mutate returned value
    pub fn make_index(&mut self, index: usize) -> Rc<RefCell<usize>> {
        self.indexes.push(Rc::new(RefCell::new(index)));
        self.indexes.last().unwrap().clone()
    }

    pub fn push(&mut self, ch: char) {
        self.string.push(ch)
    }
    pub fn push_str(&mut self, string: &str) {
        self.string.push_str(string)
    }
    pub fn pop(&mut self) -> Option<char> {
        let last_index = self.string.len() - 2; // after pop
        for index in &self.indexes {
            let index = *index.borrow();
            assert!(
                index < last_index,
                "tried to pop IndexString, but tracked index {} would go out of bounds",
                index
            );
        }

        self.string.pop()
    }
    pub fn insert_str(&mut self, idx: usize, string: &str) {
        self.string.insert_str(idx, string);

        for index in &self.indexes {
            let mut index = index.borrow_mut();
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
    let index = s.make_index(s.len());
    assert_eq!(*index.borrow(), 12);

    s.push_str(" here's another part!");
    assert_eq!(*index.borrow(), 12);
    s.insert_str(0, "this part is before! ");
    assert_eq!(*index.borrow(), 21 + 12);
}

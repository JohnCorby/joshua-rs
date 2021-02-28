//! modified from elsa::FrozenVec

use stable_deref_trait::StableDeref;
use std::cell::UnsafeCell;

/// append only vec
/// that doesn't require mutable borrow.
/// returns stable deref address that can be stored in other places and will never move.
#[derive(Debug, Default)]
pub struct FrozenVec<T>(UnsafeCell<Vec<T>>);
impl<T: StableDeref> FrozenVec<T> {
    pub fn push_get(&self, value: T) -> &T::Target {
        unsafe {
            let vec = &mut *self.0.get();
            vec.push(value);
            vec.get_unchecked(vec.len() - 1)
        }
    }
}
impl<'a, T: StableDeref> IntoIterator for &'a FrozenVec<T> {
    type Item = &'a T::Target;
    type IntoIter = Iter<'a, T>;
    fn into_iter(self) -> Self::IntoIter {
        Iter {
            vec: unsafe { &*self.0.get() },
            index: 0,
        }
    }
}

pub struct Iter<'a, T> {
    vec: &'a [T],
    index: usize,
}
impl<'a, T: StableDeref> Iterator for Iter<'a, T> {
    type Item = &'a T::Target;
    fn next(&mut self) -> Option<Self::Item> {
        self.vec.get(self.index).map(|it| {
            self.index += 1;
            it.deref()
        })
    }
}

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

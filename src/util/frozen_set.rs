//! copied from https://github.com/Manishearth/elsa/blob/master/src/index_set.rs
#![allow(dead_code)]

use stable_deref_trait::StableDeref;
use std::borrow::Borrow;
use std::cell::{Cell, UnsafeCell};
use std::collections::HashSet;
use std::hash::Hash;
use std::iter::FromIterator;

/// Append-only version of `std::collections::HashSet` where
/// insertion does not require mutable access
pub struct FrozenSet<T> {
    set: UnsafeCell<HashSet<T>>,
    /// Eq/Hash implementations can have side-effects, and using Rc it is possible
    /// for FrozenIndexSet::insert to be called on a key that itself contains the same
    /// `FrozenIndexSet`, whose `eq` implementation also calls FrozenIndexSet::insert
    ///
    /// We use this `in_use` flag to guard against any reentrancy.
    in_use: Cell<bool>,
}

// safety: UnsafeCell implies !Sync

impl<T: Eq + Hash> FrozenSet<T> {
    pub fn new() -> Self {
        Self {
            set: UnsafeCell::new(Default::default()),
            in_use: Cell::new(false),
        }
    }
}

impl<T: Eq + Hash + StableDeref> FrozenSet<T> {
    // these should never return &T
    // these should never delete any entries
    pub fn insert(&self, value: T) -> &T::Target {
        assert!(!self.in_use.get());
        self.in_use.set(true);
        let ret = unsafe {
            let set = self.set.get();
            &*(*set).get_or_insert(value)
        };
        self.in_use.set(false);
        ret
    }

    pub fn get<Q: ?Sized>(&self, k: &Q) -> Option<&T::Target>
    where
        T: Borrow<Q>,
        Q: Hash + Eq,
    {
        assert!(!self.in_use.get());
        self.in_use.set(true);
        let ret = unsafe {
            let set = self.set.get();
            (*set).get(k).map(|x| &**x)
        };
        self.in_use.set(false);
        ret
    }

    pub fn into_set(self) -> HashSet<T> {
        self.set.into_inner()
    }

    /// Get mutable access to the underlying [`IndexSet`].
    ///
    /// This is safe, as it requires a `&mut self`, ensuring nothing is using
    /// the 'frozen' contents.
    pub fn as_mut(&mut self) -> &mut HashSet<T> {
        unsafe { &mut *self.set.get() }
    }
}

impl<T> From<HashSet<T>> for FrozenSet<T> {
    fn from(set: HashSet<T>) -> Self {
        Self {
            set: UnsafeCell::new(set),
            in_use: Cell::new(false),
        }
    }
}

impl<T: Eq + Hash> FromIterator<T> for FrozenSet<T> {
    fn from_iter<U>(iter: U) -> Self
    where
        U: IntoIterator<Item = T>,
    {
        let set: HashSet<_> = iter.into_iter().collect();
        set.into()
    }
}

impl<T: Eq + Hash> Default for FrozenSet<T> {
    fn default() -> Self {
        FrozenSet::new()
    }
}

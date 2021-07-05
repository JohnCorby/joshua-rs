//! copied from https://github.com/Manishearth/elsa/blob/master/src/index_set.rs
#![allow(dead_code)]

use indexmap::IndexSet;
use stable_deref_trait::StableDeref;
use std::borrow::Borrow;
use std::cell::{Cell, UnsafeCell};
use std::hash::Hash;
use std::iter::FromIterator;
use std::ops::Index;

/// Append-only version of `indexmap::IndexSet` where
/// insertion does not require mutable access
#[derive(Debug)]
pub struct FrozenIndexSet<T> {
    set: UnsafeCell<IndexSet<T>>,
    /// Eq/Hash implementations can have side-effects, and using Rc it is possible
    /// for FrozenIndexSet::insert to be called on a key that itself contains the same
    /// `FrozenIndexSet`, whose `eq` implementation also calls FrozenIndexSet::insert
    ///
    /// We use this `in_use` flag to guard against any reentrancy.
    in_use: Cell<bool>,
}

// safety: UnsafeCell implies !Sync

impl<T: Eq + Hash> FrozenIndexSet<T> {
    pub fn new() -> Self {
        Self {
            set: UnsafeCell::new(Default::default()),
            in_use: Cell::new(false),
        }
    }
}

impl<T: Eq + Hash + StableDeref> FrozenIndexSet<T> {
    // these should never return &T
    // these should never delete any entries
    pub fn insert(&self, value: T) -> &T::Target {
        assert!(!self.in_use.get());
        self.in_use.set(true);
        let ret = unsafe {
            let set = self.set.get();
            let (index, _was_vacant) = (*set).insert_full(value);
            &*(*set)[index]
        };
        self.in_use.set(false);
        ret
    }

    // these should never return &T
    // these should never delete any entries
    pub fn insert_full(&self, value: T) -> (usize, &T::Target) {
        assert!(!self.in_use.get());
        self.in_use.set(true);
        let ret = unsafe {
            let set = self.set.get();
            let (index, _was_vacant) = (*set).insert_full(value);
            (index, &*(*set)[index])
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

    pub fn get_full<Q: ?Sized>(&self, k: &Q) -> Option<(usize, &T::Target)>
    where
        T: Borrow<Q>,
        Q: Hash + Eq,
    {
        assert!(!self.in_use.get());
        self.in_use.set(true);
        let ret = unsafe {
            let set = self.set.get();
            (*set).get_full(k).map(|(i, x)| (i, &**x))
        };
        self.in_use.set(false);
        ret
    }

    pub fn get_index(&self, index: usize) -> Option<&T::Target> {
        assert!(!self.in_use.get());
        self.in_use.set(true);
        let ret = unsafe {
            let set = self.set.get();
            (*set).get_index(index).map(|r| &**r)
        };
        self.in_use.set(false);
        ret
    }

    pub fn into_set(self) -> IndexSet<T> {
        self.set.into_inner()
    }

    /// Get mutable access to the underlying [`IndexSet`].
    ///
    /// This is safe, as it requires a `&mut self`, ensuring nothing is using
    /// the 'frozen' contents.
    pub fn as_mut(&mut self) -> &mut IndexSet<T> {
        unsafe { &mut *self.set.get() }
    }
}

impl<T> From<IndexSet<T>> for FrozenIndexSet<T> {
    fn from(set: IndexSet<T>) -> Self {
        Self {
            set: UnsafeCell::new(set),
            in_use: Cell::new(false),
        }
    }
}

impl<T: Eq + Hash + StableDeref> Index<usize> for FrozenIndexSet<T> {
    type Output = T::Target;
    fn index(&self, idx: usize) -> &T::Target {
        assert!(!self.in_use.get());
        self.in_use.set(true);
        let ret = unsafe {
            let set = self.set.get();
            &*(*set)[idx]
        };
        self.in_use.set(false);
        ret
    }
}

impl<T: Eq + Hash> FromIterator<T> for FrozenIndexSet<T> {
    fn from_iter<U>(iter: U) -> Self
    where
        U: IntoIterator<Item = T>,
    {
        let set: IndexSet<_> = iter.into_iter().collect();
        set.into()
    }
}

impl<T: Eq + Hash> Default for FrozenIndexSet<T> {
    fn default() -> Self {
        FrozenIndexSet::new()
    }
}

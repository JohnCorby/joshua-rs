#![allow(dead_code)]

use crate::pos::Pos;
use crate::ty::Type;
use std::ops::{Deref, DerefMut};

#[derive(Debug, Copy, Clone, Default, Hash, Ord, PartialOrd, Eq, PartialEq)]
pub struct With<T, E> {
    pub inner: T,
    pub extra: E,
}
impl<T, E> With<T, E> {
    pub fn map<T2>(self, mut f: impl FnMut(T) -> T2) -> With<T2, E> {
        With {
            inner: f(self.inner),
            extra: self.extra,
        }
    }
}

pub trait ToWith<E>: Sized {
    fn with(self, extra: E) -> With<Self, E>;
}
impl<T, E> ToWith<E> for T {
    fn with(self, extra: E) -> With<Self, E> {
        With { inner: self, extra }
    }
}

impl<T, E> Deref for With<T, E> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}
impl<T, E> DerefMut for With<T, E> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner
    }
}

pub type WithPos<T> = With<T, Pos>;
pub type WithType<T> = With<T, Type>;

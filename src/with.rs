use std::ops::{Deref, DerefMut};

#[derive(Debug, Copy, Clone, Default, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub struct With<T, E> {
    pub inner: T,
    pub extra: E,
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

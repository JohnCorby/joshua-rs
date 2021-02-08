use std::ops::{Deref, DerefMut};

#[derive(Debug, Copy, Clone)]
pub struct With<T, E>(pub T, pub E);

impl<T, E> With<T, E> {
    pub fn map<T2>(self, mut f: impl FnMut(T) -> T2) -> With<T2, E> {
        With(f(self.0), self.1)
    }
    #[allow(dead_code)]
    pub fn map_extra<E2>(self, mut f: impl FnMut(E) -> E2) -> With<T, E2> {
        With(self.0, f(self.1))
    }
}

pub trait ToWith<E>: Sized {
    fn with(self, extra: E) -> With<Self, E>;
}
impl<T, E> ToWith<E> for T {
    fn with(self, extra: E) -> With<Self, E> {
        With(self, extra)
    }
}

impl<T, E> Deref for With<T, E> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
impl<T, E> DerefMut for With<T, E> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

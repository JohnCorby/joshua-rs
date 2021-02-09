use crate::error::warn_internal;
use std::ops::Deref;

#[derive(Debug, Copy, Clone)]
pub struct LateInit<T> {
    inner: Option<T>,
    what: &'static str,
}

impl<T> LateInit<T> {
    pub fn new(what: &'static str) -> Self {
        Self { inner: None, what }
    }

    pub fn init(&mut self, value: T) {
        if self.inner.is_some() {
            return warn_internal(format!("{} already initialized", self.what));
        }
        self.inner = Some(value)
    }
}

impl<T> Deref for LateInit<T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        self.inner
            .as_ref()
            .unwrap_or_else(|| panic!("{} not initialized", self.what))
    }
}

use std::lazy::OnceCell;
use std::ops::Deref;

/// value that is initialized later
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct LateInit<T>(OnceCell<T>);

impl<T> Default for LateInit<T> {
    fn default() -> Self {
        Self(Default::default())
    }
}

impl<T> LateInit<T> {
    pub fn get(&self) -> Option<&T> {
        self.0.get()
    }

    pub fn init(&self, value: T) {
        assert!(self.0.set(value).is_ok(), "LateInit already initialized");
    }
}

impl<T> Deref for LateInit<T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        self.get().expect("LateInit not initialized")
    }
}

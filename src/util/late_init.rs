use std::fmt::{Debug, Formatter};
use std::lazy::OnceCell;
use std::ops::Deref;

/// value that is initialized later
#[derive(Clone, Eq, PartialEq)]
pub struct LateInit<T>(OnceCell<T>);

impl<T> Default for LateInit<T> {
    fn default() -> Self {
        Self(Default::default())
    }
}

impl<T> LateInit<T> {
    pub fn init(&self, value: T) {
        assert!(self.0.set(value).is_ok(), "LateInit already initialized")
    }
}

impl<T> Deref for LateInit<T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        self.0.get().expect("LateInit not initialized")
    }
}

impl<T> From<T> for LateInit<T> {
    fn from(value: T) -> Self {
        LateInit(OnceCell::from(value))
    }
}

impl<T: Debug> Debug for LateInit<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self.0.get() {
            Some(v) => f.debug_tuple("LateInit").field(v).finish(),
            None => f.write_str("LateInit(Uninit)"),
        }
    }
}

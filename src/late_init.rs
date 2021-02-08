use std::lazy::OnceCell;
use std::ops::Deref;

#[derive(Debug, Clone)]
pub struct LateInit<T>(OnceCell<T>);

impl<T> Default for LateInit<T> {
    fn default() -> Self {
        LateInit(OnceCell::default())
    }
}

impl<T> LateInit<T> {
    pub fn init(&self, value: T) {
        assert!(self.0.set(value).is_ok(), "late init already initialized")
    }
}

impl<T> Deref for LateInit<T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        self.0.get().expect("late init not initialized")
    }
}

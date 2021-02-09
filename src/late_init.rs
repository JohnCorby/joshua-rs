use std::ops::Deref;

#[derive(Debug, Clone)]
pub struct LateInit<T>(Option<T>);

impl<T> Default for LateInit<T> {
    fn default() -> Self {
        LateInit(Default::default())
    }
}

impl<T> LateInit<T> {
    pub fn init(&mut self, value: T) {
        assert!(self.0.is_none(), "late init already initialized");
        self.0 = Some(value)
    }
}

impl<T> Deref for LateInit<T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        self.0.as_ref().expect("late init not initialized")
    }
}

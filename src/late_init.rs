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
        assert!(self.inner.is_none(), "{} already initialized", self.what);
        self.inner = Some(value)
    }
}

impl<T> Deref for LateInit<T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        self.inner
            .as_ref()
            .unwrap_or_else(|| panic!("{} needs to be initialized", self.what))
    }
}

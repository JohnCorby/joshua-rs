use crate::error::{warn_internal, MyResult};

#[derive(Debug, Copy, Clone)]
pub struct LateInit<T> {
    inner: Option<T>,
    what: &'static str,
}

impl<T> LateInit<T> {
    pub fn new(what: &'static str) -> Self {
        Self {
            inner: Default::default(),
            what,
        }
    }

    pub fn get_or_try_init(&mut self, f: impl FnOnce() -> MyResult<T>) -> MyResult<&T> {
        Ok(match self.inner {
            Some(ref inner) => {
                warn_internal(format!("{} already initialized", self.what));
                inner
            }
            None => {
                self.inner = Some(f()?);
                self.inner.as_ref().unwrap()
            }
        })
    }
}

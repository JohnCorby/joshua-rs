use crate::error::MyResult;

#[derive(Debug, Copy, Clone)]
pub struct InitCached<T> {
    value: Option<T>,
    what: &'static str,
}

impl<T> InitCached<T> {
    pub fn new(what: &'static str) -> Self {
        Self {
            value: Default::default(),
            what,
        }
    }

    pub fn get_or_try_init(&mut self, f: impl FnOnce() -> MyResult<T>) -> MyResult<&T> {
        Ok(match self.value {
            Some(ref inner) => inner,
            None => {
                self.value = Some(f()?);
                self.value.as_ref().unwrap()
            }
        })
    }
}

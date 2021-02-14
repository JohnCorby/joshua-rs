use crate::error::Res;

#[derive(Debug, Copy, Clone, Default)]
pub struct InitCached<T>(Option<T>);

impl<T> InitCached<T> {
    pub fn get_or_try_init(&mut self, f: impl FnOnce() -> Res<T>) -> Res<&T> {
        Ok(match self.0 {
            Some(ref inner) => inner,
            None => {
                self.0 = Some(f()?);
                self.0.as_ref().unwrap()
            }
        })
    }
}

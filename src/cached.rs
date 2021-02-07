//! string interner, but works for anything

use parking_lot::{Mutex, MutexGuard};
use std::fmt::{Debug, Display, Formatter};
use std::lazy::SyncLazy;
use std::marker::PhantomData;
use string_interner::{DefaultSymbol, StringInterner, Symbol};

#[derive(Clone, Default, Hash, Ord, PartialOrd, Eq, PartialEq)]
pub struct Cached<T> {
    index: usize,
    phantom_data: PhantomData<T>,
}
impl<T: Clone> Copy for Cached<T> {}

impl<T> From<usize> for Cached<T> {
    fn from(index: usize) -> Self {
        Self {
            index,
            phantom_data: PhantomData,
        }
    }
}

pub type CachedString = Cached<String>;
static INTERNER: SyncLazy<Mutex<StringInterner>> =
    SyncLazy::new(|| Mutex::new(StringInterner::new()));
fn interner() -> MutexGuard<'static, StringInterner> {
    INTERNER.try_lock().expect("INTERNER locked")
}
impl From<String> for CachedString {
    fn from(s: String) -> Self {
        interner().get_or_intern(s).to_usize().into()
    }
}
impl From<&str> for CachedString {
    fn from(s: &str) -> Self {
        interner().get_or_intern(s).to_usize().into()
    }
}
impl Display for CachedString {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        Display::fmt(
            interner()
                .resolve(DefaultSymbol::try_from_usize(self.index).unwrap())
                .unwrap(),
            f,
        )
    }
}
impl Debug for CachedString {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        f.debug_struct("CachedString")
            .field("index", &self.index)
            .field("value", &self.to_string())
            .finish()
    }
}

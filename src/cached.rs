//! string interner, but works for anything
use lazy_static::lazy_static;
use parking_lot::{Mutex, MutexGuard};
use std::fmt::{Debug, Formatter};
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
lazy_static! {
    static ref INTERNER: Mutex<StringInterner> = Mutex::new(StringInterner::new());
}
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
impl From<CachedString> for String {
    fn from(c: CachedString) -> Self {
        let symbol = DefaultSymbol::try_from_usize(c.index).unwrap();
        interner().resolve(symbol).unwrap().to_string()
    }
}
impl ToString for CachedString {
    fn to_string(&self) -> String {
        String::from(*self)
    }
}
impl Debug for CachedString {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("CachedString")
            .field("index", &self.index)
            .field("value", &self.to_string())
            .finish()
    }
}

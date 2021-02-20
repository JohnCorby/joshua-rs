use crate::context::Ctx;
use std::fmt::{Debug, Display, Formatter};
use std::hash::{Hash, Hasher};
use std::ops::Deref;
use string_interner::Symbol;

/// stores string for fast access, and index for fast compare
#[derive(Copy, Clone)]
pub struct InternedStr<T> {
    str: T,
    index: usize,
}

pub trait Intern<T> {
    fn intern(self, ctx: &mut Ctx<'_>) -> InternedStr<T>;
}
impl<T: AsRef<str>> Intern<T> for T {
    fn intern(self, ctx: &mut Ctx<'_>) -> InternedStr<T> {
        InternedStr {
            index: ctx.interner.get_or_intern(self.as_ref()).to_usize(),
            str: self,
        }
    }
}

impl<T: AsRef<str>> Deref for InternedStr<T> {
    type Target = str;
    fn deref(&self) -> &Self::Target {
        self.str.as_ref()
    }
}
impl InternedStr<&str> {
    pub fn str_to_string(self) -> InternedStr<String> {
        InternedStr {
            str: self.str.to_string(),
            index: self.index,
        }
    }
}

impl<T> Hash for InternedStr<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.index.hash(state)
    }
}
impl<T, U> PartialEq<InternedStr<U>> for InternedStr<T> {
    fn eq(&self, other: &InternedStr<U>) -> bool {
        self.index == other.index
    }
}
impl<T> Eq for InternedStr<T> {}

impl<T: AsRef<str>> Display for InternedStr<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.str.as_ref())
    }
}
impl<T: AsRef<str>> Debug for InternedStr<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("InternedStr")
            .field("str", &self.str.as_ref())
            .field("index", &self.index)
            .finish()
    }
}

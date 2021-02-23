use crate::context::Ctx;
use crate::error::warn_internal;
use std::fmt::{Debug, Display, Formatter};
use std::hash::{Hash, Hasher};
use std::ops::Deref;
use string_interner::Symbol;

/// stores string for fast access, and index for fast compare
/// fixme InternedStr<'i> has to be cloned which is ech, not sure how to fix this
#[derive(Copy, Clone)]
pub struct InternedStr<'i> {
    str: &'i str,
    index: usize,
}

pub trait Intern<'i> {
    fn intern(self, ctx: &mut Ctx<'i>) -> InternedStr<'i>;
}
impl<'i> Intern<'i> for &'i str {
    fn intern(self, ctx: &mut Ctx<'i>) -> InternedStr<'i> {
        InternedStr {
            str: self,
            index: ctx.interner.get_or_intern(self).to_usize(),
        }
    }
}
impl<'i> Intern<'i> for String {
    fn intern(self, ctx: &mut Ctx<'i>) -> InternedStr<'i> {
        if ctx.interner.get(&self).is_some() {
            // fixme
            warn_internal(format!("interning string {:?} creates a new i in ctx even though it already exists in the interner :(", self), None);
        }
        ctx.new_i(self).intern(ctx)
    }
}

impl Deref for InternedStr<'_> {
    type Target = str;
    fn deref(&self) -> &Self::Target {
        self.str
    }
}

impl Hash for InternedStr<'_> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.index.hash(state)
    }
}
impl PartialEq for InternedStr<'_> {
    fn eq(&self, other: &InternedStr<'_>) -> bool {
        self.index == other.index
    }
}
impl Eq for InternedStr<'_> {}

impl Display for InternedStr<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.str)
    }
}
impl Debug for InternedStr<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("InternedStr")
            .field("str", &self.str)
            .field("index", &self.index)
            .finish()
    }
}

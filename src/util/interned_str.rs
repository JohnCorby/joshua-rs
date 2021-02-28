use crate::context::Ctx;
use std::fmt::{Display, Formatter};
use std::ops::Deref;

/// not actually an interned string lol,
/// but DOES reuse inputs for `String`s
#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq)]
pub struct InternedStr<'i>(&'i str);

pub trait Intern<'i> {
    fn intern(self, ctx: &mut Ctx<'i>) -> InternedStr<'i>;
}
impl<'i> Intern<'i> for &'i str {
    fn intern(self, _: &mut Ctx<'i>) -> InternedStr<'i> {
        InternedStr(self)
    }
}
impl<'i> Intern<'i> for String {
    fn intern(self, ctx: &mut Ctx<'i>) -> InternedStr<'i> {
        for i in ctx.is {
            if i == self {
                return InternedStr(i);
            }
        }
        ctx.new_i(self).intern(ctx)
    }
}

impl Deref for InternedStr<'_> {
    type Target = str;
    fn deref(&self) -> &Self::Target {
        self.0
    }
}

impl Display for InternedStr<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.0)
    }
}

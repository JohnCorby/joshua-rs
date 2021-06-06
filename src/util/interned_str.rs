use crate::context::Ctx;
use std::fmt::{Display, Formatter};
use std::ops::Deref;

/// not actually an interned string lol,
/// but DOES reuse inputs for `String`s
#[derive(Debug, Copy, Clone, Default, Hash, Ord, PartialOrd, Eq, PartialEq)]
pub struct InternedStr<'i>(&'i str);

pub trait Intern<'i> {
    fn intern(self, ctx: &mut Ctx<'i>) -> InternedStr<'i>;
}
impl Intern<'i> for &'i str {
    fn intern(self, _: &mut Ctx<'i>) -> InternedStr<'i> {
        InternedStr(self)
    }
}
impl Intern<'i> for String {
    fn intern(self, ctx: &mut Ctx<'i>) -> InternedStr<'i> {
        ctx.inputs
            .iter()
            .find(|&input| input == self)
            .unwrap_or_else(|| ctx.new_i(self))
            .intern(ctx)
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

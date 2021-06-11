use crate::context::Ctx;
use std::fmt::{Display, Formatter};
use std::ops::Deref;

/// basically a Cow<'i, str>
/// except the Owned Strings are also stored in Ctx as a str,
/// and are reused somewhat
#[derive(Debug, Copy, Clone, Default, Hash, Ord, PartialOrd, Eq, PartialEq)]
pub struct CtxStr<'i>(&'i str);

impl From<&'i str> for CtxStr<'i> {
    fn from(s: &'i str) -> Self {
        Self(s)
    }
}
pub trait IntoCtx<'i> {
    fn into_ctx(self, ctx: &Ctx<'i>) -> CtxStr<'i>;
}
impl IntoCtx<'i> for String {
    fn into_ctx(self, ctx: &Ctx<'i>) -> CtxStr<'i> {
        ctx.inputs
            .iter()
            .find(|&input| input == self)
            .unwrap_or_else(|| ctx.new_i(self))
            .into()
    }
}

impl Deref for CtxStr<'_> {
    type Target = str;
    fn deref(&self) -> &Self::Target {
        self.0
    }
}

impl Display for CtxStr<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.0)
    }
}

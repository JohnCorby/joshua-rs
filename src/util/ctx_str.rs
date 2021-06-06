use crate::context::Ctx;
use std::fmt::{Display, Formatter};
use std::ops::Deref;

/// basically a Cow<'i, str>
/// except the Owned Strings are also stored in Ctx as a str,
/// and are reused somewhat
#[derive(Debug, Copy, Clone, Default, Hash, Ord, PartialOrd, Eq, PartialEq)]
pub struct CtxStr<'i>(&'i str);

pub trait IntoCtx<'i> {
    fn into_ctx(self, ctx: &Ctx<'i>) -> CtxStr<'i>;
}
impl IntoCtx<'i> for &'i str {
    fn into_ctx(self, _: &Ctx<'i>) -> CtxStr<'i> {
        CtxStr(self)
    }
}
impl IntoCtx<'i> for String {
    fn into_ctx(self, ctx: &Ctx<'i>) -> CtxStr<'i> {
        ctx.inputs
            .iter()
            .find(|&input| input == self)
            .unwrap_or_else(|| ctx.new_i(self))
            .into_ctx(ctx)
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

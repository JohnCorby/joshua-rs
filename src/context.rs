use crate::scope::Scopes;
use string_interner::StringInterner;

/// stores general program context
#[derive(Debug)]
pub struct Ctx<'i> {
    pub i: &'i str,
    pub o: String,

    pub scopes: Scopes<'i>,
    pub interner: StringInterner,
}

impl<'i> Ctx<'i> {
    pub fn new(i: &'i str) -> Self {
        let mut ctx = Self {
            i,
            o: Default::default(),
            scopes: Default::default(),
            interner: Default::default(),
        };
        ctx.init_scopes();
        ctx
    }
}

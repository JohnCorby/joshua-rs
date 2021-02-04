use crate::parse::Pair;
use crate::pos::AsPos;
use crate::with::{ToWith, WithPos};

/// take a parser pair an turn it into ourselves
pub trait Visit: Sized {
    fn visit(pair: Pair) -> WithPos<Self> {
        let pos = pair.as_pos();
        pos.set_current();
        let result = Self::visit_impl(pair);
        result.with(pos)
    }

    fn visit_impl(pair: Pair) -> Self;
}

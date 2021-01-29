use crate::error::MyResult;
use crate::parse::Pair;
use crate::pos::AsPos;

/// take a parser pair an turn it into ourselves
pub trait Visit: Sized {
    fn visit(pair: Pair) -> MyResult<Self> {
        pair.as_pos().set_current();
        Self::visit_impl(pair)
    }

    fn visit_impl(pair: Pair) -> MyResult<Self>;
}

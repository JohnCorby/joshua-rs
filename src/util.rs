use crate::error::MyError::UnexpectedRule;
use crate::error::MyResult;
use crate::visit::Visit;
use crate::{Pair, Pairs, Rule};
use std::backtrace::Backtrace;

pub fn _debug_pairs(pairs: &Pairs) -> String {
    format!(
        "[{}]",
        pairs
            .clone()
            .map(|pair| format!(
                "{:?}({:?}) {}",
                pair.as_rule(),
                pair.as_span().as_str(),
                _debug_pairs(&pair.into_inner())
            ))
            .collect::<Vec<_>>()
            .join(", ")
    )
}

pub trait PairExt<'a> {
    /// turns visit into an extension method for pair
    fn visit<T: Visit<'a>>(self) -> MyResult<T>;

    /// check that a pair matches a rule, and then return its inner pairs
    fn into_inner_checked(self, expected: Rule) -> MyResult<Pairs<'a>>;
}
impl<'a> PairExt<'a> for Pair<'a> {
    fn visit<T: Visit<'a>>(self) -> MyResult<T> {
        T::visit(self)
    }

    fn into_inner_checked(self, expected: Rule) -> MyResult<Pairs<'a>> {
        let actual = self.as_rule();
        if expected == actual {
            Ok(self.into_inner())
        } else {
            Err(UnexpectedRule {
                expected,
                actual,
                backtrace: Backtrace::capture(),
            })
        }
    }
}

pub trait PairsExt<'a> {
    /// visits any not iterated pairs,
    /// short circuiting if any of them error
    fn visit_rest<T: Visit<'a>>(self) -> MyResult<Vec<T>>;
}
impl<'a> PairsExt<'a> for Pairs<'a> {
    fn visit_rest<T: Visit<'a>>(self) -> MyResult<Vec<T>> {
        self.map(Pair::visit).collect()
    }
}

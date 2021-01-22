use crate::error::MyError::UnexpectedRule;
use crate::error::MyResult;
use crate::{Pair, Pairs};

pub fn debug_pairs(pairs: &Pairs) -> String {
    format!(
        "[{}]",
        pairs
            .clone()
            .map(|pair| format!(
                "{:?}({:?}) {}",
                pair.as_rule(),
                pair.as_span().as_str(),
                debug_pairs(&pair.into_inner())
            ))
            .collect::<Vec<_>>()
            .join(", ")
    )
}

/// check that a pair matches a rule, and then return its inner pairs
pub fn pair_inner_checked(pair: Pair, rule: crate::Rule) -> MyResult<Pairs> {
    match pair.as_rule() {
        rule => Ok(pair.into_inner()),
        other => Err(UnexpectedRule {
            expected: rule,
            actual: other,
        }),
    }
}

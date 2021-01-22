use crate::error::MyError::UnexpectedRule;
use crate::error::MyResult;
use crate::{Pair, Pairs};
use std::backtrace::Backtrace;

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
pub fn pair_inner_checked(pair: Pair, expected: crate::Rule) -> MyResult<Pairs> {
    let actual = pair.as_rule();
    if expected == actual {
        Ok(pair.into_inner())
    } else {
        Err(UnexpectedRule {
            expected,
            actual,
            backtrace: Backtrace::capture(),
        })
    }
}

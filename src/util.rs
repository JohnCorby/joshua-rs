use crate::Pairs;

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

/// check that the pair matches a certain rule, and return early if it doesnt
/// sanity-preserving mechanism
#[macro_export]
macro_rules! check_pair {
    ($pair: ident, $rule: expr) => {
        if $pair.as_rule() != $rule {
            return Err(format!("expected rule {:?}, but got {:?}", $rule, $pair.as_rule()).into());
        }
    };
}

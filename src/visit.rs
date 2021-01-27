use crate::define::Define;
use crate::error::MyResult;
use crate::parse::{Pair, Rule};
use crate::util::PairExt;

/// take a parser pair an turn it into ourselves
/// call Pair.visit instead of this (hence the name)
/// todo track pos for each node so it will also work in `gen`
pub trait Visit: Sized {
    fn visit_impl(pair: Pair) -> MyResult<Self>;
}

pub type Program = Vec<Define>;

impl Visit for Program {
    fn visit_impl(pair: Pair) -> MyResult<Self> {
        pair.into_inner_checked(Rule::program)?
            .filter_map(|pair| {
                // last rule is EOI. dont visit it
                if pair.as_rule() == Rule::EOI {
                    return None;
                }
                Some(pair.visit())
            })
            .collect()
    }
}

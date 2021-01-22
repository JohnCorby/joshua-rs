use crate::define::Define;
use crate::error::MyResult;
use crate::parse::{Pair, Pairs, Rule};
use crate::util::PairExt;

/// take a parser pair an turn it into ourselves
pub trait Visit: Sized {
    fn visit(pair: Pair) -> MyResult<Self>;
}

pub type Program = Vec<Define>;

/// visit the entire program
pub fn visit_program(pairs: Pairs) -> MyResult<Program> {
    pairs
        .filter_map(|pair| {
            // last rule is EOI. dont visit it
            if pair.as_rule() == Rule::EOI {
                return None;
            }
            Some(pair.visit())
        })
        .collect()
}

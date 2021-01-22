use crate::define::Define;
use crate::error::MyResult;
use crate::util::PairExt;
use crate::{Pair, Rule};

/// take a parser pair an turn it into ourselves
pub trait Visit<'a>: Sized {
    fn visit(pair: Pair<'a>) -> MyResult<Self>;
}

pub type Program = Vec<Define>;

/// visit the entire program
pub fn visit_program(pair: Pair) -> MyResult<Program> {
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

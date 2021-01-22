use crate::define::Define;
use crate::error::MyResult;
use crate::util::pair_inner_checked;
use crate::{Pair, Rule};

/// take a parser pair an turn it into ourselves
pub trait Visit {
    fn visit(pair: Pair) -> MyResult<Self>
    where
        Self: Sized;
}

pub type Program = Vec<Define>;

/// visit the entire program
pub fn visit_program(pair: Pair) -> MyResult<Program> {
    let pairs = pair_inner_checked(pair, Rule::program)?;

    let mut program = Program::new();
    for pair in pairs {
        if pair.as_rule() == Rule::EOI {
            break;
        }
        program.push(Define::visit(pair)?);
    }
    Ok(program)
}

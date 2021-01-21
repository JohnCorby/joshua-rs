use crate::define::{Define, Program};
use crate::error::MyResult;
use crate::{Pair, Rule};

/// take a parser pair an turn it into ourselves
pub trait Visit {
    fn visit(pair: Pair) -> MyResult<Self>
    where
        Self: Sized;
}

/// visit the entire program
pub fn visit_program(pair: Pair) -> MyResult<Program> {
    crate::check_pair!(pair, Rule::program);
    let pairs = pair.into_inner();

    let mut program = Program::new();
    for pair in pairs {
        program.push(Define::visit(pair)?);
    }
    Ok(program)
}

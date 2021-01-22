#![feature(try_trait)]
#![feature(backtrace)]

mod define;
mod error;
mod expr;
mod gen;
mod parser;
mod statement;
mod ty;
mod util;
mod visit;

use crate::error::MyResult;
use crate::gen::gen_program;
use crate::parser::parse_program;
use crate::ty::Type;
use crate::visit::visit_program;

// nice types that will resolve :)
pub type Rule = crate::parser::Rule;
pub type Pair<'a> = pest::iterators::Pair<'a, Rule>;
pub type Pairs<'a> = pest::iterators::Pairs<'a, Rule>;

const PROGRAM: &str = include_str!("../test/test.jo");
fn main() -> MyResult<()> {
    Type::init()?;

    let pairs = parse_program(PROGRAM)?;
    println!("{:?}", pairs);
    let program = visit_program(pairs)?;
    println!("{:?}", program);
    let c_code = gen_program(program)?;
    println!("{:?}", c_code);

    Ok(())
}

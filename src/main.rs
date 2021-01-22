#![feature(try_trait)]
#![feature(backtrace)]

mod define;
mod error;
mod expr;
mod gen;
mod parse;
mod statement;
mod ty;
mod util;
mod visit;

use crate::error::MyResult;
use crate::gen::gen_program;
use crate::parse::parse_program;
use crate::ty::Type;
use crate::visit::visit_program;

const PROGRAM: &str = include_str!("../test/test.jo");
fn main() -> MyResult<()> {
    Type::init()?;

    let pairs = parse_program(PROGRAM)?;
    println!("{}", pairs);
    let program = visit_program(pairs)?;
    println!("{:#?}", program);
    let c_code = gen_program(program)?;
    println!("{}", c_code);

    Ok(())
}

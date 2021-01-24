#![feature(try_trait)]
#![feature(backtrace)]

mod compile;
mod define;
mod error;
mod expr;
mod gen;
mod parse;
mod statement;
mod ty;
mod util;
mod visit;

use crate::compile::compile_program;
use crate::error::MyResult;
use crate::gen::Gen;
use crate::parse::parse_program;
use crate::ty::Type;
use crate::util::PairExt;
use crate::visit::Program;

/// make every ref counted
/// less rewriting if i wanna change it
pub type Ref<T> = std::rc::Rc<T>;

const PROGRAM: &str = include_str!("../test/test.jo");
fn main() -> MyResult<()> {
    Type::init()?;

    let pair = parse_program(PROGRAM)?;
    println!("{}", pair);
    let program = pair.visit::<Program>()?;
    println!("{:#?}", program);
    let c_code = program.gen()?;
    println!("{}", c_code);
    compile_program(c_code)?;

    Ok(())
}

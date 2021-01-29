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
use crate::parse::{parse, Rule};
use crate::util::PairExt;
use crate::visit::Program;
use std::path::Path;
use std::rc::Rc;

/// make every ref counted
/// less rewriting if i wanna change it
pub type Ref<T> = Rc<T>;

fn main() -> MyResult<()> {
    let path = Path::new("test/test.jo");
    let program = std::fs::read_to_string(path)?;

    let pair = parse(Rule::program, &program)?;
    println!("{}", pair.to_pretty_string());
    let program = pair.visit::<Program>()?;
    println!("{:?}", program);
    let c_code = program.gen()?;
    println!("{}", c_code);
    compile_program(c_code, path)?;

    Ok(())
}

#![feature(try_trait)]
#![feature(backtrace)]

mod compile;
mod define;
mod error;
mod expr;
mod gen;
mod parse;
mod pos;
mod scope;
mod statement;
mod ty;
mod util;
mod visit;

use crate::compile::compile_program;
use crate::define::Program;
use crate::error::MyResult;
use crate::gen::Gen;
use crate::parse::{parse, Rule};
use crate::util::PairExt;
use std::path::Path;

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

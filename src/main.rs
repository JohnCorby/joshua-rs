#![feature(try_trait)]
#![feature(backtrace)]
#![feature(once_cell)]
#![feature(panic_info_message)]

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
use crate::error::{MyError, MyResult};
use crate::gen::Gen;
use crate::parse::{parse, Rule};
use crate::util::PairExt;
use std::lazy::SyncOnceCell;
use std::path::Path;

pub static PROGRAM: SyncOnceCell<String> = SyncOnceCell::new();
fn main() -> MyResult<()> {
    MyError::init();

    let path = Path::new("test/test.jo");
    let program = std::fs::read_to_string(path).unwrap();
    PROGRAM.set(program)?;

    let pair = parse(Rule::program, PROGRAM.get().unwrap())?;
    // println!("{}", pair.to_pretty_string());
    let program = pair.visit::<Program>();
    println!("{:?}", program);
    let c_code = program.gen()?;
    println!("{}", c_code);
    compile_program(c_code, path);

    Ok(())
}

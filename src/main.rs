#![feature(try_trait)]
#![feature(backtrace)]
#![feature(once_cell)]
#![feature(panic_info_message)]
#![feature(hash_set_entry)]

mod cached;
mod compile;
mod define;
mod error;
mod expr;
mod parse;
mod pass;
mod scope;
mod span;
mod statement;
mod ty;
mod util;
mod with;

use crate::compile::compile_program;
use crate::define::Program;
use crate::error::{MyError, MyResult};
use crate::parse::{Kind, Node};
use crate::pass::Gen;
use std::lazy::SyncOnceCell;
use std::path::Path;

pub static PROGRAM: SyncOnceCell<String> = SyncOnceCell::new();
fn main() -> MyResult<()> {
    MyError::init();

    let path = Path::new("test/test2.jo");
    let program = std::fs::read_to_string(path).unwrap();
    PROGRAM.set(program).unwrap();

    let node = Node::parse(PROGRAM.get().unwrap(), Kind::program)?;
    println!("{}", node);
    let program = node.visit::<Program>();
    println!("{:?}", program);
    let c_code = program.gen()?;
    println!("{}", c_code);
    compile_program(c_code, path);

    Ok(())
}

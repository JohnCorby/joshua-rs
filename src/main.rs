//!todo
//! - collect globals into 1 context struct, and just pass that around functions like a boss
//! - "mir" thingy for c, might make replacement easier; or at least just a way to gen funcs/structs outside of where we are lol
//!todo
//! - methods
//! - get literals to actually work, you might just have to brute force it, or not idk. you might be able to use generics for this instead :)
//! - pointers (array type can come from this)
//!todo
//! - generics. the lazy replacement way first. constraints way later. thisll require generating methods outside of where we are
//! - functions and structs and stuff inside functions. thisll also require generating methods outside of where we are
//!todo
//! - polymorphism??? inheritance???????
//! - ownership? borrowing? lifetimes? oh goodness

#![feature(try_trait)]
#![feature(backtrace)]
#![feature(once_cell)]
#![feature(panic_info_message)]
#![feature(hash_set_entry)]
#![feature(option_unwrap_none)]
#![feature(result_copied)]

mod cached;
mod compile;
mod define;
mod error;
mod expr;
mod parse;
mod scope;
mod span;
mod statement;
mod ty;
mod util;

use crate::compile::compile_program;
use crate::define::Program;
use crate::error::{Err, Res};
use crate::parse::{Kind, Node};
use std::lazy::SyncOnceCell;
use std::path::Path;

pub static PROGRAM: SyncOnceCell<String> = SyncOnceCell::new();
#[quit::main]
fn main() -> Res<()> {
    Err::init();

    let path = Path::new("test/test2.jo");
    let program = std::fs::read_to_string(path).unwrap();
    PROGRAM.set(program).unwrap();

    let node = Node::parse(PROGRAM.get().unwrap(), Kind::program)?;
    println!("{}", node);
    let program = node.visit::<Program>();
    println!("{:?}", program);
    let mut c_code = String::new();
    program.gen(&mut c_code)?;
    println!("{}", c_code);
    compile_program(c_code, path);

    Ok(())
}

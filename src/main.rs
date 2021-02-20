//!todo
//! - collect globals into 1 context struct, and just pass that around functions like a boss
//! - TESTS
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
#![warn(elided_lifetimes_in_paths)]

mod compile;
mod context;
mod define;
mod error;
mod expr;
mod interned_string;
mod parse;
mod scope;
mod span;
mod statement;
mod ty;
mod util;

use crate::compile::compile_program;
use crate::context::Ctx;
use crate::define::Program;
use crate::error::Err;
use crate::parse::{Kind, Node};
use std::path::Path;

// #[quit::main]
fn main() {
    Err::init();

    let path = Path::new("test/test2.jo");
    let program = std::fs::read_to_string(path).unwrap();
    let mut ctx = Ctx::new(&program);

    // let node = Node::parse(ctx.i, Kind::program)?;
    let node = Node::parse(ctx.i, Kind::program).unwrap();
    println!("{}", node);
    let program = node.visit::<Program<'_>>(&mut ctx);
    println!("{:?}", program);
    // program.gen(&mut ctx)?;
    program.gen(&mut ctx).unwrap();
    println!("{}", ctx.o);
    compile_program(ctx.o, path);
}

//!todo
//! - interned string: a tiny bit more refactoring?
//! - types: don't require g<...> or s<...>, and also merge types and type symbols
//! - generics: make a different thing (i.e. can't call normal func w generic func call and vice versa)
//! - generics inference
//! - generics: `add` function duplication
//! - generics: ...and much more. just remember it, will ya?
//! - TESTS
//! - "mir" thingy for c, might make replacement easier; or at least just a way to gen funcs/structs outside of where we are lol
//!todo
//! - methods
//! - get literals to actually work, you might just have to brute force it, or not idk. you might be able to use generics for this instead :)
//! - pointers (array type can come from this)
//!todo
//! - functions and structs and stuff inside functions. this'll also require generating methods outside of where we are
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
#![feature(try_blocks)]
#![feature(in_band_lifetimes)]
#![warn(elided_lifetimes_in_paths)]

use crate::context::Ctx;
use crate::error::{Err, Res};
use crate::parse::{Kind, Node};
use crate::pass::compile_program;
use crate::pass::define::Program;
use std::env::args;
use std::path::PathBuf;

mod context;
mod error;
mod parse;
mod pass;
mod scope;
mod span;
mod test;
mod util;

#[quit::main]
fn main() {
    Err::init();

    let is = &Default::default();
    let ctx = &mut Ctx::new(is);

    let path = args().nth(1).unwrap();
    let path = &PathBuf::from(path);
    let program = std::fs::read_to_string(path).unwrap();

    let result: Res<'_, ()> = try {
        println!("parsing");
        let node = Node::parse(ctx.new_i(program), Kind::program)?;
        println!("visiting");
        let program = node.visit::<Program<'_>>(ctx);
        ctx.program.init(program.clone());
        println!("type checking");
        program.type_check(ctx)?;
        println!("generating");
        program.clone().gen(ctx);
    };
    if let Err(err) = result {
        return eprintln!("Error: {}", err);
    }

    println!("compiling");
    let status = compile_program(&ctx.o, path);
    if !status.success() {
        quit::with_code(status.code().unwrap())
    }
}

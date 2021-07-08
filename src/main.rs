//!todo
//! - ways to use pointers
//! - generic inference
//!todo
//! - "build scope" pass which will add _____Type and Func symbols to the scope uhhhhh well it's a stack now so we can't pre-build this. darn
//! - get literals to actually work, you might just have to brute force it, or not idk. you might be able to use generics for this instead :)
//!todo
//! - polymorphism??? inheritance???????
//! - ownership? borrowing? lifetimes? oh goodness

#![feature(backtrace)]
#![feature(panic_info_message)]
#![feature(try_blocks)]
#![feature(in_band_lifetimes)]
#![feature(label_break_value)]
#![feature(hash_set_entry)]
#![feature(option_result_unwrap_unchecked)]

#[macro_use]
extern crate pest_derive;
#[macro_use]
extern crate derivative;
#[macro_use]
extern crate derive_new;

use crate::context::{Ctx, Intern};
use crate::error::{Err, Res};
use crate::parse::{Kind, Node};
use crate::pass::ast1::Program;
use crate::pass::compile_program;
use std::env::args;
use std::path::PathBuf;

mod context;
mod error;
mod parse;
mod pass;
mod span;
mod test;
mod util;

#[quit::main]
fn main() {
    Err::init();

    let ctx = &mut Ctx::new();

    let path = args().nth(1).unwrap();
    let path = &PathBuf::from(path);
    let program = std::fs::read_to_string(path).unwrap().intern();

    let result: Res = try {
        println!("parsing");
        let node = Node::parse(program, Kind::program)?;
        println!("visiting");
        let program = node.visit::<Program>(ctx);
        println!("type checking");
        let program = program.type_check(ctx)?;
        println!("generating");
        program.gen(ctx);
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

//!todo immediate
//! - "type-hint"s via `Option<Type>` passed thru `Expr::type_check`
//! - refactor each pass to its own file
//!
//! todo
//! - interned string: a tiny bit more refactoring?
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

#![feature(backtrace)]
#![feature(once_cell)]
#![feature(panic_info_message)]
#![feature(try_blocks)]
#![feature(in_band_lifetimes)]
#![warn(elided_lifetimes_in_paths)]

use crate::context::Ctx;
use crate::error::{Err, Res};
use crate::parse::{Kind, Node};
use crate::pass::compile_program;
use pass::ast::Program;
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
        println!("type checking");
        program.type_check(ctx)?;
        println!("generating");
        for define in std::mem::take(&mut ctx.extra_defines) {
            define.gen(ctx);
        }
        program.gen(ctx);
        debug_assert!(ctx.o.is_empty());
        ctx.o.clear();
        ctx.o.push_str("#pragma region structs\n");
        ctx.o.push_str(&ctx.structs);
        ctx.o.push_str("#pragma endregion structs\n");
        ctx.o.push_str("#pragma region global vars\n");
        ctx.o.push_str(&ctx.global_vars);
        ctx.o.push_str("#pragma endregion global vars\n");
        ctx.o.push_str("#pragma region func declares\n");
        ctx.o.push_str(&ctx.func_declares);
        ctx.o.push_str("#pragma endregion func declares\n");
        ctx.o.push_str("#pragma region func defines\n");
        ctx.o.push_str(&ctx.func_defines);
        ctx.o.push_str("#pragma endregion func defines");
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

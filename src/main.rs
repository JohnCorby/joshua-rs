//!todo
//! - some type inference
//! - generic inference
//! - pointers (array type can come from this)
//! - structs/funcs in funcs, this one will be painful
//! - interned string: a tiny bit more refactoring?
//! - "build scope" pass which will add _____Type and Func symbols to the scope uhhhhh well it's a stack now so we can't pre-build this. darn
//!todo
//! - methods
//! - get literals to actually work, you might just have to brute force it, or not idk. you might be able to use generics for this instead :)
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
use crate::pass::gen::Gen;
use crate::pass::type_check::TypeCheck;
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

    let result: Res<'_> = try {
        println!("parsing");
        let node = Node::parse(ctx.new_i(program), Kind::program)?;
        println!("visiting");
        let program = node.visit::<Program<'_>>(ctx);
        println!("type checking");
        program.type_check(ctx)?;
        debug_assert!(ctx.scopes.0.is_empty());
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

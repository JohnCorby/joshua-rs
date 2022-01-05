//!todo
//! - ways to use pointers
//! - generic inference
//!todo
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
#![feature(let_else)]
#![feature(once_cell)]
#![feature(derive_default_enum)]

#[macro_use]
extern crate pest_derive;
#[macro_use]
extern crate derivative;
#[macro_use]
extern crate derive_new;
#[macro_use]
extern crate extend;
#[macro_use]
extern crate strum;

use crate::context::{Intern, Output};
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

    let path = args().nth(1).unwrap();
    let path = &PathBuf::from(path);
    let program = std::fs::read_to_string(path).unwrap().intern();

    let result: Res<String> = try {
        println!("parsing");
        let node = Node::parse(program, Kind::program)?;
        println!("visiting");
        let program = node.visit::<Program>();
        println!("type checking");
        let mut o = Output::default();
        let program = program.type_check(&mut o)?;
        println!("generating");
        program.gen(o)
    };
    if let Err(err) = result {
        return eprintln!("Error: {}", err);
    }

    println!("compiling");
    let status = compile_program(&result.unwrap(), path);
    if !status.success() {
        quit::with_code(status.code().unwrap())
    }
}

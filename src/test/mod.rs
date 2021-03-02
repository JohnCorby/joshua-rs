#![cfg(test)]

use crate::context::Ctx;
use crate::parse::{Kind, Node};
use crate::pass::compile_program;
use crate::pass::define::Program;
use rand::distributions::Alphanumeric;
use rand::{thread_rng, Rng};
use unindent::Unindent;

/// tries to gen and compile an input program
fn check(i: &str) {
    println!("i = \n{}", i.unindent());

    let is = &Default::default();
    let ctx = &mut Ctx::new(is);
    let program = Node::parse(i, Kind::program)
        .unwrap()
        .visit::<Program<'_>>(ctx);
    program.type_check(ctx);
    program.gen(ctx);
    println!("\no = \n{}", ctx.o.unindent());

    let file = thread_rng()
        .sample_iter(Alphanumeric)
        .take(7)
        .map(char::from)
        .collect::<String>();
    let file = file.as_ref();
    assert!(compile_program(&ctx.o, file).success());
    std::fs::remove_file(file.with_extension("c")).unwrap();
    std::fs::remove_file(file.with_extension("exe")).unwrap();
}

#[test]
fn funcs() {
    check(include_str!("../../test/funcs.jo"))
}

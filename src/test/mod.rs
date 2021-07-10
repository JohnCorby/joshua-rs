#![cfg(test)]

use crate::context::{Intern, Output};
use crate::parse::{Kind, Node};
use crate::pass::ast1::Program;
use crate::pass::compile_program;
use rand::distributions::Alphanumeric;
use rand::{thread_rng, Rng};
use unindent::Unindent;

/// tries to gen and compile an input program
fn check(i: &str) {
    let i = i.to_string().intern();
    println!("i = \n{}", i.unindent());

    let program = Node::parse(i, Kind::program).unwrap().visit::<Program>();
    let mut o = Output::default();
    let program = program.type_check(&mut o).unwrap();
    let o = program.gen(o);
    println!("\no = \n{}", o.unindent());

    let file = thread_rng()
        .sample_iter(Alphanumeric)
        .take(7)
        .map(char::from)
        .collect::<String>();
    let file = file.as_ref();
    let success = compile_program(&o, file).success();
    std::fs::remove_file(file.with_extension("c")).unwrap();
    std::fs::remove_file(file.with_extension("exe")).unwrap();
    assert!(success);
}

#[test]
fn funcs() {
    check(include_str!("../../test/test2.jo"))
}

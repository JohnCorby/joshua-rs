#![feature(try_trait)]
#![feature(backtrace)]
// #![allow(unused)]

mod define;
mod error;
mod expr;
mod gen;
mod statement;
mod util;
mod visit;

use crate::error::MyResult;
use crate::gen::gen_program;
use crate::visit::visit_program;
use anyhow::*;
use pest::Parser;
use pest_derive::Parser;

#[derive(Parser)]
#[grammar = "grammar.pest"]
pub struct MyParser;

pub type Pair<'a> = pest::iterators::Pair<'a, Rule>;
pub type Pairs<'a> = pest::iterators::Pairs<'a, Rule>;

fn parse(input: &str) -> Pair {
    let pairs: Result<Pairs, pest::error::Error<Rule>> = MyParser::parse(Rule::program, input);
    match pairs {
        Ok(mut pairs) => pairs.next().unwrap(),
        Err(e) => {
            println!("{}", e);
            panic!()
        }
    }
}

const PROGRAM: &str = include_str!("../test/expr.jo");
fn main() -> MyResult<()> {
    let pair = parse(PROGRAM);
    println!("{:?}", pair);
    let program = visit_program(pair)?;
    println!("{:?}", program);
    let c_code = gen_program(program);
    println!("{:#?}", c_code);

    // let mut pairs = MyParser::parse(Rule::expr, r#"!1 + 2*-3 + 4 % "hello""#)?;
    // println!("{}", debug_pairs(&pairs));
    // let expr = expr::parse_expr(pairs.next()?)?;
    // println!("{:#?}", expr);

    Ok(())
}

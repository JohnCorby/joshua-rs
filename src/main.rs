#![feature(try_trait)]

mod ast;
mod error;
mod expr;
mod util;

use crate::error::MyResult;
use anyhow::*;
use pest::Parser;
use pest_derive::Parser;
use util::*;

#[derive(Parser)]
#[grammar = "grammar.pest"]
pub struct MyParser;

pub type Pair<'a> = pest::iterators::Pair<'a, Rule>;
pub type Pairs<'a> = pest::iterators::Pairs<'a, Rule>;

fn parse(input: &str) -> Pairs {
    let pairs: Result<Pairs, pest::error::Error<Rule>> = MyParser::parse(Rule::program, input);
    match pairs {
        Ok(pairs) => pairs,
        Err(e) => {
            println!("{}", e);
            panic!()
        }
    }
}

const PROGRAM: &str = include_str!("../test/expr.jo");
fn main() -> MyResult<()> {
    let pairs = parse(PROGRAM);

    let mut pairs = MyParser::parse(Rule::expr, r#"!1 + 2*-3 + 4 % "hello""#)?;
    println!("{}", debug_pairs(&pairs));
    let expr = expr::parse_expr(pairs.next().context("next is none")?)?;
    println!("{:#?}", expr);

    Ok(())
}

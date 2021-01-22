//! tuck this crap away because it doesnt resolve nicely in clion

use crate::error::MyResult;
use pest::iterators::Pairs;
use pest::Parser;
use pest_derive::Parser;

#[derive(Parser)]
#[grammar = "grammar.pest"]
pub struct MyParser;

pub fn parse_program(input: &str) -> MyResult<Pairs<Rule>> {
    MyParser::parse(Rule::program, input).map_err(|e| e.into())
}

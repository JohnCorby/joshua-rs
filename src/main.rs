mod parse_expr;

use pest::Parser;
use pest_derive::Parser;

#[derive(Parser)]
#[grammar = "grammar.pest"]
pub struct MyParser;

pub type Pair<'a> = pest::iterators::Pair<'a, Rule>;
pub type Pairs<'a> = pest::iterators::Pairs<'a, Rule>;
pub type Error = pest::error::Error<Rule>;

fn parse(input: &str) -> Pairs {
    let pairs: Result<Pairs, Error> = MyParser::parse(Rule::program, input);
    match pairs {
        Ok(pairs) => pairs,
        Err(e) => {
            println!("{}", e);
            panic!()
        }
    }
}

const PROGRAM: &str = include_str!("../test/test.jo");
fn main() {
    println!("{}", parse(PROGRAM).to_json())
}

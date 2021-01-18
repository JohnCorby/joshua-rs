use pest::Parser;
use pest_derive::Parser;

#[derive(Parser)]
#[grammar = "grammar.pest"]
pub struct MyParser;

fn main() {
    println!("{:#?}", MyParser::parse(Rule::file, "-273.15,3215\r\n"));
}

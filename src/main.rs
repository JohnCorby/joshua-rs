use pest::iterators::Pairs;
use pest::Parser;
use pest_derive::Parser;

#[derive(Parser)]
#[grammar = "grammar.pest"]
pub struct MyParser;

fn main() {
    let file: Pairs<Rule> =
        MyParser::parse(Rule::file, "-273.15,3215\r\n-273.15,3215\r\n").unwrap();
    for record in file {
        match record.as_rule() {
            Rule::record => {
                let mut inner = record.into_inner();
                let left: f32 = inner.next().unwrap().as_str().parse().unwrap();
                let right: f32 = inner.next().unwrap().as_str().parse().unwrap();
                println!("got record {}, {}", left, right);
            }
            Rule::EOI => (),
            rule => unreachable!("{:?}", rule),
        }
    }
}

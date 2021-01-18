use pest::iterators::Pair;
use pest::Parser;
use pest_derive::Parser;

#[derive(Parser)]
#[grammar = "grammar.pest"]
pub struct MyParser;

fn main() {
    let file: Pair<Rule> = MyParser::parse(Rule::file, "-273.15,3215\r\n")
        .unwrap()
        .next()
        .unwrap();
    for record in file.into_inner() {
        match record.as_rule() {
            Rule::record => {
                let mut inner = record.into_inner();
                let left: f32 = inner.next().unwrap().as_str().parse().unwrap();
                let right: f32 = inner.next().unwrap().as_str().parse().unwrap();
                println!("got record {}, {}", left, right);
            }
            Rule::EOI => (),
            _ => unreachable!(),
        }
    }
}

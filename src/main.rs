use pest::iterators::Pairs;
use pest::Parser;
use pest_derive::Parser;

#[derive(Parser)]
#[grammar = "grammar.pest"]
pub struct MyParser;

fn main() {
    let file: Pairs<Rule> =
        MyParser::parse(Rule::file, "  -273.15, 3215\r\n1,2,3,4,5\r\n").unwrap();
    for record in file {
        match record.as_rule() {
            Rule::record => {
                let fields: Vec<f32> = record
                    .into_inner()
                    .map(|pair| pair.as_str().parse().unwrap())
                    .collect();
                println!("got record with fields {:?}", fields);
            }
            Rule::EOI => (),
            rule => unreachable!("{:?}", rule),
        }
    }
}

use crate::error::MyResult;
use pest::Parser;

// nice types that will resolve :)
pub type Rule = inner::Rule;
pub type Pair<'a> = pest::iterators::Pair<'a, Rule>;
pub type Pairs<'a> = pest::iterators::Pairs<'a, Rule>;

pub fn parse_program(input: &str) -> MyResult<Pairs> {
    inner::Parser::parse(Rule::program, input).map_err(|e| e.into())
}

mod inner {
    //! tuck this crap away because it doesnt resolve nicely in clion

    use pest_derive::Parser;

    #[derive(Parser)]
    #[grammar = "grammar.pest"]
    pub struct Parser;
}

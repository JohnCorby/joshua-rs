use crate::error::MyResult;
use pest::Parser;

// nice types that will resolve :)
pub type Rule = inner::Rule;
pub type Pair<'i> = pest::iterators::Pair<'i, Rule>;
pub type Pairs<'i> = pest::iterators::Pairs<'i, Rule>;

/// parse an input string into a pair based on a rule
pub fn parse(rule: Rule, input: &str) -> MyResult<Pair> {
    Ok(inner::Parser::parse(rule, input)?.next().unwrap())
}

mod inner {
    //! tuck this crap away because it doesnt resolve nicely in clion

    use pest_derive::Parser;

    #[derive(Parser)]
    #[grammar = "grammar.pest"]
    pub struct Parser;
}

use crate::error::{MyResult, Pos};
use crate::parse::{Pair, Pairs, Rule};
use crate::visit::Visit;
use console::style;
use extend::ext;

#[ext(pub, name = PairExt)]
impl<'i> Pair<'i> {
    /// turns visit into an extension method for pair
    fn visit<T: Visit>(self) -> MyResult<T> {
        T::visit(self.track())
    }

    /// update pos (for errors)
    fn track(self) -> Self {
        Pos::update(&self);
        self
    }

    /// check that a pair matches a rule, and then return its inner pairs
    fn into_inner_checked(self, expected: Rule) -> MyResult<Pairs<'i>> {
        let actual = self.as_rule();
        if expected == actual {
            Ok(self.into_inner())
        } else {
            Err(format!("expected rule {:?}, but got {:?}", expected, actual).into())
        }
    }

    fn to_pretty_string(&self) -> String {
        let rule = style(self.as_rule()).red();
        let str = style(self.as_str()).blue();
        let inner = self.clone().into_inner().to_pretty_strings();
        if inner.is_empty() {
            format!("{:?}({:?})", rule, str)
        } else if inner.len() == 1 {
            format!("{:?}.{}", rule, inner[0])
        } else {
            format!("{:?}[{}]", rule, inner.join(", "))
        }
    }
}

#[ext(pub, name = PairsExt)]
impl<'i> Pairs<'i> {
    /// visits any not iterated pairs,
    /// short circuiting if any of them error
    fn visit_rest<T: Visit>(self) -> MyResult<Vec<T>> {
        self.map(Pair::visit).collect()
    }

    fn to_pretty_strings(&self) -> Vec<String> {
        self.clone().map(|pair| pair.to_pretty_string()).collect()
    }
}

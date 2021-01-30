use crate::parse::{Pair, Pairs, Rule};
use crate::visit::Visit;
use console::style;

pub trait PairExt<'i> {
    /// turns visit into an extension method for pair
    fn visit<T: Visit>(self) -> T;

    /// check that a pair matches a rule, and then return its inner pairs
    fn into_inner_checked(self, expected: Rule) -> Pairs<'i>;

    fn to_pretty_string(&self) -> String;
}
impl<'i> PairExt<'i> for Pair<'i> {
    fn visit<T: Visit>(self) -> T {
        T::visit(self)
    }

    fn into_inner_checked(self, expected: Rule) -> Pairs<'i> {
        let actual = self.as_rule();
        if expected == actual {
            self.into_inner()
        } else {
            panic!("expected rule {:?}, but got {:?}", expected, actual)
        }
    }

    fn to_pretty_string(&self) -> String {
        let rule = style(self.as_rule()).red();
        let str = style(self.as_str()).blue();
        let inner = self
            .clone()
            .into_inner()
            .map(|pair| pair.to_pretty_string())
            .collect::<Vec<_>>();
        if inner.is_empty() {
            format!("{:?}({:?})", rule, str)
        } else if inner.len() == 1 {
            format!("{:?}.{}", rule, inner[0])
        } else {
            format!("{:?}[{}]", rule, inner.join(", "))
        }
    }
}

pub trait PairsExt {
    /// visits any not iterated pairs,
    /// short circuiting if any of them error
    fn visit_rest<T: Visit>(self) -> Vec<T>;
}
impl PairsExt for Pairs<'_> {
    fn visit_rest<T: Visit>(self) -> Vec<T> {
        self.map(Pair::visit).collect()
    }
}

pub trait Mangle {
    fn mangle(&self) -> String;
    fn demangle(&self) -> String;
}
impl Mangle for str {
    fn mangle(&self) -> String {
        mangling::mangle(self.as_bytes())
    }
    fn demangle(&self) -> String {
        String::from_utf8_lossy(&mangling::demangle(self).unwrap()).into()
    }
}

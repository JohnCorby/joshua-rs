use crate::cached::CachedString;
use crate::error::MyResult;
use crate::pass::Visit;
use crate::span::Span;
use crate::with::{ToWith, WithSpan};
use console::style;
use pest::iterators::{Pair, Pairs};
use pest::Parser;
use std::fmt::{Debug, Display, Formatter};

pub type Rule = inner::Rule;

/// newtype wrapper for pest pair
#[derive(Clone)]
pub struct Node<'i>(Pair<'i, Rule>);
impl<'i> From<Pair<'i, Rule>> for Node<'i> {
    fn from(p: Pair<'i, Rule>) -> Self {
        Self(p)
    }
}
impl<'i> Node<'i> {
    /// parse an input string into a node based on a rule
    pub fn parse(rule: Rule, input: &'i str) -> MyResult<Self> {
        Ok(inner::Parser::parse(rule, input)?.next().unwrap().into())
    }

    pub fn children(self) -> Nodes<'i> {
        self.0.into_inner().into()
    }
    pub fn rule(&self) -> Rule {
        self.0.as_rule()
    }
    pub fn span(&self) -> Span {
        self.0.as_span().into()
    }
    pub fn as_str(&self) -> &'i str {
        self.0.as_str()
    }

    /// turns visit into an extension method for node
    pub fn visit<T: Visit>(self) -> WithSpan<T> {
        T::visit(self)
    }
    /// check that a node matches a rule, and then return its inner nodes
    pub fn into_inner_checked(self, expected: Rule) -> Nodes<'i> {
        let actual = self.rule();
        if expected == actual {
            self.children()
        } else {
            panic!("expected rule {:?}, but got {:?}", expected, actual)
        }
    }
    pub fn as_cached_str_with_span(&self) -> WithSpan<CachedString> {
        let string = self.as_str();
        let cached = CachedString::from(string);
        cached.with(self.span())
    }
}
impl Debug for Node<'_> {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        Debug::fmt(&self.0, f)
    }
}
impl Display for Node<'_> {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        let rule = style(self.rule()).red();
        let str = style(self.as_str()).blue();
        let children = self
            .clone()
            .children()
            .map(|node| node.to_string())
            .collect::<Vec<_>>();
        if children.is_empty() {
            write!(f, "{:?}({:?})", rule, str)
        } else if children.len() == 1 {
            write!(f, "{:?}.{}", rule, children[0])
        } else {
            write!(f, "{:?}[{}]", rule, children.join(", "))
        }
    }
}

/// newtype wrapper for pest pairs
#[derive(Clone)]
pub struct Nodes<'i>(Pairs<'i, Rule>);
impl<'i> From<Pairs<'i, Rule>> for Nodes<'i> {
    fn from(p: Pairs<'i, Rule>) -> Self {
        Self(p)
    }
}
impl<'i> Iterator for Nodes<'i> {
    type Item = Node<'i>;
    fn next(&mut self) -> Option<Self::Item> {
        self.0.next().map(Into::into)
    }
}
impl<'i> DoubleEndedIterator for Nodes<'i> {
    fn next_back(&mut self) -> Option<Self::Item> {
        self.0.next_back().map(Into::into)
    }
}
impl<'i> Nodes<'i> {
    /// visits any not iterated nodes,
    /// short circuiting if any of them error
    pub fn visit_rest<T: Visit>(self) -> Vec<WithSpan<T>> {
        self.map(Node::visit).collect()
    }
}
impl Display for Nodes<'_> {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(
            f,
            "[{}]",
            self.clone()
                .map(|node| node.to_string())
                .collect::<Vec<_>>()
                .join(", ")
        )
    }
}
impl Debug for Nodes<'_> {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        Debug::fmt(&self.0, f)
    }
}

mod inner {
    #[derive(pest_derive::Parser)]
    #[grammar = "grammar.pest"]
    pub struct Parser;
}

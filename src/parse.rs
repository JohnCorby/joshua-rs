use crate::error::MyResult;
use crate::span::Span;
use crate::util::Visit;
use console::style;
use pest::iterators::{Pair, Pairs};
use pest::Parser;
use std::fmt::{Debug, Display, Formatter};

pub type Kind = inner::Rule;

/// newtype wrapper for pest pair
#[derive(Clone)]
pub struct Node<'i>(Pair<'i, Kind>);
impl<'i> From<Pair<'i, Kind>> for Node<'i> {
    fn from(p: Pair<'i, Kind>) -> Self {
        Self(p)
    }
}
impl<'i> Node<'i> {
    /// parse an input string into a node based on a kind
    pub fn parse(input: &'i str, kind: Kind) -> MyResult<Self> {
        Ok(inner::Parser::parse(kind, input)?.next().unwrap().into())
    }

    pub fn children(self) -> Nodes<'i> {
        self.0.into_inner().into()
    }
    pub fn kind(&self) -> Kind {
        self.0.as_rule()
    }
    pub fn span(&self) -> Span {
        self.0.as_span().into()
    }
    pub fn as_str(&self) -> &'i str {
        self.0.as_str()
    }

    pub fn track(&self) {
        self.span().track()
    }

    /// turns visit into an extension method for node
    pub fn visit<T: Visit>(self) -> T {
        T::visit(self)
    }
    /// check that a node matches a kind, and then return its inner nodes
    pub fn children_checked(self, expected: Kind) -> Nodes<'i> {
        let actual = self.kind();
        if expected == actual {
            self.children()
        } else {
            panic!("expected kind {:?}, but got {:?}", expected, actual)
        }
    }
}
impl Debug for Node<'_> {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        Debug::fmt(&self.0, f)
    }
}
impl Display for Node<'_> {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        let kind = style(self.kind());
        let str = style(self.as_str());
        let children = self
            .clone()
            .children()
            .map(|node| node.to_string())
            .collect::<Vec<_>>();
        if children.is_empty() {
            write!(f, "{:?}({:?})", kind.red(), str.blue())
        } else if children.len() == 1 {
            write!(f, "{:?}.{}", kind.green(), children[0])
        } else {
            write!(f, "{:?}[{}]", kind.red(), children.join(", "))
        }
    }
}

/// newtype wrapper for pest pairs
#[derive(Clone)]
pub struct Nodes<'i>(Pairs<'i, Kind>);
impl<'i> From<Pairs<'i, Kind>> for Nodes<'i> {
    fn from(p: Pairs<'i, Kind>) -> Self {
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
    pub fn visit_rest<T: Visit>(self) -> Vec<T> {
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

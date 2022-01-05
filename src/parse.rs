use crate::error::{IntoErr, Res};
use crate::span::Span;
use crate::util::IterExt;
use console::style;
use pest::iterators::{Pair, Pairs};
use pest::Parser;
use std::fmt::{Debug, Display, Formatter};

pub type Kind = inner::Rule;

/// newtype wrapper for pest pair
#[derive(Clone)]
pub struct Node(Pair<'static, Kind>);
impl From<Pair<'static, Kind>> for Node {
    fn from(p: Pair<'static, Kind>) -> Self {
        Self(p)
    }
}
impl Node {
    /// parse an input string into a node based on a kind
    pub fn parse(input: &'static str, kind: Kind) -> Res<Self> {
        let result = inner::Parser::parse(kind, input);
        result
            .map(|pairs| Nodes::from(pairs).next().unwrap())
            .map_err(|e| e.into_err(None))
    }

    pub fn children(self) -> Nodes {
        self.0.into_inner().into()
    }
    pub fn kind(&self) -> Kind {
        self.0.as_rule()
    }
    pub fn span(&self) -> Span {
        self.0.as_span().into()
    }
    pub fn str(&self) -> &'static str {
        self.0.as_str()
    }

    /// check that a node matches a kind, and then return its inner nodes
    #[cfg(debug_assertions)]
    pub fn children_checked(self, expected: Kind) -> Nodes {
        let actual = self.kind();
        if expected == actual {
            self.children()
        } else {
            panic!("expected kind {:?}, but got {:?}", expected, actual)
        }
    }
    /// check that a node matches a kind, and then return its inner nodes
    #[cfg(not(debug_assertions))]
    pub fn children_checked(self, _expected: Kind) -> Nodes {
        self.children()
    }
}
impl Debug for Node {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        Debug::fmt(&self.0, f)
    }
}
impl Display for Node {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        let kind = style(self.kind());
        let str = style(self.str());
        let children = self.clone().children().map(|node| node.to_string()).vec();
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
pub struct Nodes(Pairs<'static, Kind>);
impl From<Pairs<'static, Kind>> for Nodes {
    fn from(p: Pairs<'static, Kind>) -> Self {
        Self(p)
    }
}
impl Iterator for Nodes {
    type Item = Node;
    fn next(&mut self) -> Option<Self::Item> {
        self.0.next().map(Node::from)
    }
}
impl DoubleEndedIterator for Nodes {
    fn next_back(&mut self) -> Option<Self::Item> {
        self.0.next_back().map(Node::from)
    }
}
impl Display for Nodes {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(
            f,
            "[{}]",
            self.clone().map(|node| node.to_string()).vec().join(", ")
        )
    }
}
impl Debug for Nodes {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        Debug::fmt(&self.0, f)
    }
}

mod inner {
    #[derive(Parser)]
    #[grammar = "grammar.pest"]
    pub struct Parser;
}

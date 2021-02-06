use crate::error::MyResult;
use crate::parse::Node;
use crate::span::Span;
use crate::with::{ToWith, WithSpan};

/// take a parser node an turn it into ourselves
pub trait Visit: Sized {
    fn visit(node: Node) -> WithSpan<Self> {
        let span = node.span();
        span.set_current();
        let result = Self::visit_impl(node);
        result.with(span)
    }

    fn visit_impl(node: Node) -> Self;
}

/// turn self into valid C code
pub trait Gen: Sized {
    /// boilerplate :(
    fn span(&self) -> Span;

    fn gen(self) -> MyResult<String> {
        self.span().set_current();
        self.gen_impl()
    }

    fn gen_impl(self) -> MyResult<String>;
}

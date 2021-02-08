use crate::error::MyResult;
use crate::parse::Node;
use crate::span::Span;
use crate::ty::Type;
use crate::with::{ToWith, With};

pub type WithSpan<T> = With<T, Span>;

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

#[allow(dead_code)]
pub type WithType<T> = With<WithSpan<T>, Option<Type>>;

pub trait InitType: Sized {
    fn span(&self) -> Span;
    fn ty(&self) -> Type;

    fn init_type(self) -> MyResult<Self> {
        self.span().set_current();
        self.init_type_impl()
    }

    fn init_type_impl(self) -> MyResult<Self>;
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

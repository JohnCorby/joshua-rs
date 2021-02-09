use crate::error::MyResult;
use crate::parse::Node;
use crate::span::Span;
use crate::ty::TypeKind;

/// take a parser node an turn it into ourselves
pub trait Visit: Sized {
    fn visit(node: Node) -> Self {
        let span = node.span();
        span.set_current();
        Self::visit_impl(node)
    }

    fn visit_impl(node: Node) -> Self;
}

pub trait InitType: Sized {
    fn span(&self) -> Span;

    fn init_type(self) -> MyResult<TypeKind> {
        self.span().set_current();
        self.init_type_impl()
    }

    fn init_type_impl(self) -> MyResult<TypeKind>;
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

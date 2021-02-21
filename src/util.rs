use crate::context::Ctx;
use crate::parse::{Node, Nodes};

pub trait Visit<'i>: Sized {
    fn visit(node: Node<'i>, ctx: &mut Ctx<'i>) -> Self;
}
impl<'i> Node<'i> {
    pub fn visit<T: Visit<'i>>(self, ctx: &mut Ctx<'i>) -> T {
        T::visit(self, ctx)
    }
}
impl<'i> Nodes<'i> {
    /// visits any not iterated nodes,
    /// short circuiting if any of them error
    pub fn visit_rest<T: Visit<'i>>(self, ctx: &mut Ctx<'i>) -> Vec<T> {
        self.map(|node| node.visit(ctx)).collect()
    }
}

pub trait Mangle {
    fn mangle(&self) -> String;
}
impl Mangle for str {
    fn mangle(&self) -> String {
        // format!("`{}`", self)
        mangling::mangle(self.as_bytes())
    }
}

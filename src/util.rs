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
    pub fn visit_rest<'c, T: Visit<'i>>(self, ctx: &mut Ctx<'i>) -> Vec<T> {
        let mut visited = vec![];
        for node in self {
            visited.push(node.visit(ctx))
        }
        visited
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

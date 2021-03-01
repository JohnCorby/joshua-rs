use crate::context::Ctx;
use crate::parse::{Node, Nodes};
use crate::pass::ty::Type;

pub mod frozen_vec;
pub mod index_string;
pub mod interned_str;
pub mod late_init;

pub trait Visit<'i>: Sized {
    fn visit(node: Node<'i>, ctx: &mut Ctx<'i>) -> Self;
}
impl<'i> Node<'i> {
    pub fn visit<V: Visit<'i>>(self, ctx: &mut Ctx<'i>) -> V {
        V::visit(self, ctx)
    }
}
impl<'i> Nodes<'i> {
    /// visits any not iterated nodes,
    /// short circuiting if any of them error
    pub fn visit_rest<V: Visit<'i>>(self, ctx: &mut Ctx<'i>) -> Vec<V> {
        self.map(|node| node.visit(ctx)).collect()
    }
}

pub trait Mangle {
    fn mangle(&self) -> String;
    fn mangle_func(&self, arg_types: &[Type<'_>]) -> String;
}
impl Mangle for str {
    fn mangle(&self) -> String {
        format!("{}/*{}*/", mangling::mangle(self.as_bytes()), self)
    }
    fn mangle_func(&self, arg_types: &[Type<'_>]) -> String {
        // don't mangle func main (entry point)
        if self == "main" {
            self.to_string()
        } else {
            format!(
                "{}({})",
                self,
                arg_types
                    .iter()
                    .map(|ty| ty.name())
                    .collect::<Vec<_>>()
                    .join(", ")
            )
            .mangle()
        }
    }
}

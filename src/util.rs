use crate::parse::Node;
use crate::span::Span;

pub trait Visit: Sized {
    fn visit(node: Node) -> Self;
}
pub trait Track: Sized {
    fn span(&self) -> Span;
    fn track(self) -> Self {
        self.span().track();
        self
    }
}

pub trait Mangle {
    fn mangle(&self) -> String;
    fn demangle(&self) -> String;
}
impl Mangle for str {
    fn mangle(&self) -> String {
        // fixme hacky workaround for main method
        if self == "main" {
            self.to_string()
        } else {
            mangling::mangle(self.as_bytes())
        }
    }
    fn demangle(&self) -> String {
        String::from_utf8_lossy(&mangling::demangle(self).unwrap()).into()
    }
}

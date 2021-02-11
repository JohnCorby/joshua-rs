use crate::parse::Node;

pub trait Visit: Sized {
    fn visit(node: Node) -> Self;
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

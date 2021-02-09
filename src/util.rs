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

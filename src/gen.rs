use crate::error::MyResult;
use crate::visit::Program;

/// turn self into valid C code
pub trait Gen {
    fn gen(self) -> MyResult<String>;
}

impl Gen for Program {
    fn gen(self) -> MyResult<String> {
        Ok(r#"void main() {
    puts("hello, world!");
}"#
        .into())
    }
}

use crate::error::MyResult;
use crate::visit::Program;

/// turn self into valid C code
pub trait Gen {
    fn gen(self) -> String;
}

/// gen the entire program
pub fn gen_program(_program: Program) -> MyResult<String> {
    Ok(r#"void main() {
    puts("hello, world!");
}"#
    .into())
}

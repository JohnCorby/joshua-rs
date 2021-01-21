use crate::define::Program;
use crate::error::MyResult;

/// turn self into valid C code
pub trait Gen {
    fn gen(self) -> String;
}

/// gen the entire program
pub fn gen_program(program: Program) -> MyResult<String> {
    Ok(r#"void main() {
    puts("hello, world!");
}"#
    .into())
}

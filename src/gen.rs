use crate::error::MyResult;
use crate::ty::Type;
use crate::visit::Program;

/// turn self into valid C code
pub trait Gen {
    fn gen(self) -> MyResult<String>;
}

impl Gen for Program {
    fn gen(self) -> MyResult<String> {
        Type::init()?;
        Ok(r#"int main(){puts("hello, world!";}"#.into())
    }
}

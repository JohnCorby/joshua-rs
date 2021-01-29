use crate::define::Define;
use crate::error::MyResult;
use crate::pos::Pos;
use crate::scope::Scope;
use crate::ty::Type;
use crate::visit::Program;

/// turn self into valid C code
pub trait Gen {
    fn gen(self) -> MyResult<String>;
}

impl Gen for Program {
    fn gen(self) -> MyResult<String> {
        Scope::push();

        Type::init()?;
        Pos::reset();

        let result = Ok(self
            .into_iter()
            .map(Define::gen)
            .collect::<MyResult<Vec<_>>>()?
            .join("\n"));

        Scope::pop()?;
        result
    }
}

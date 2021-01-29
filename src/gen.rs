use crate::define::Define;
use crate::error::MyResult;
use crate::pos::Pos;
use crate::scope::Scope;
use crate::ty::Type;
use crate::visit::Program;

/// turn self into valid C code
pub trait Gen: Sized {
    fn pos(&self) -> Pos;

    fn gen(self) -> MyResult<String> {
        self.pos().set_current();
        self.gen_impl()
    }

    fn gen_impl(self) -> MyResult<String>;
}

impl Gen for Program {
    fn pos(&self) -> Pos {
        Pos::unknown()
    }

    fn gen_impl(self) -> MyResult<String> {
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

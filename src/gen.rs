use crate::error::MyResult;
use crate::pos::Pos;

/// turn self into valid C code
pub trait Gen: Sized {
    fn pos(&self) -> Pos;

    fn gen(self) -> MyResult<String> {
        self.pos().set_current();
        self.gen_impl()
    }

    fn gen_impl(self) -> MyResult<String>;
}

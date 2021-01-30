use crate::error::MyResult;
use crate::pos::HasPos;

/// turn self into valid C code
pub trait Gen: Sized + HasPos {
    fn gen(self) -> MyResult<String> {
        self.pos().set_current();
        self.gen_impl()
    }

    fn gen_impl(self) -> MyResult<String>;
}

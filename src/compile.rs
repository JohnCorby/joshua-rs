//! take c code string, write it to a c file, and compile that file

use crate::error::MyResult;

pub fn compile_program(c_code: impl AsRef<str>) -> MyResult<()> {
    const PATH: &str = "test/test2.c";
    std::fs::write(PATH, c_code.as_ref())?;

    // todo compile

    Ok(())
}

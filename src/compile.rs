//! take c code string, write it to a c file, and compile that file

use crate::error::{MyResult, Pos};
use std::path::Path;

const OUT_PATH: &str = "test/test2.c";

pub fn compile_program(c_code: impl AsRef<str>) -> MyResult<()> {
    let path = Path::new(OUT_PATH);

    Pos::reset();

    std::fs::write(path, c_code.as_ref())?;

    // todo compile

    Ok(())
}

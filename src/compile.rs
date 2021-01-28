//! take c code string, write it to a c file, and compile that file

use crate::error::{MyResult, Pos};

pub fn compile_program(c_code: impl AsRef<str>) -> MyResult<()> {
    Pos::reset();

    const C_FILE: &str = "test/test2.c";
    const OUT_FILE: &str = "test/test2";

    std::fs::write(C_FILE, c_code.as_ref())?;

    // todo compile
    std::env::set_var("OUT_DIR", "");
    std::env::set_var("TARGET", "");
    std::env::set_var("OPT_LEVEL", "");
    std::env::set_var("HOST", "");
    cc::Build::new().file(C_FILE).try_compile(OUT_FILE)?;

    Ok(())
}

//! take c code string, write it to a c file, and compile that file

use crate::error::{MyResult, Pos};
use std::path::Path;
use std::process::Command;

pub fn compile_program(c_code: impl AsRef<str>, path: impl AsRef<Path>) -> MyResult<()> {
    let c_path = &path.as_ref().with_extension("c");
    let out_path = &path.as_ref().with_extension("");

    Pos::reset();

    std::fs::write(c_path, c_code.as_ref())?;

    println!("compiling");
    let status = Command::new("gcc")
        .arg(c_path)
        .arg("-o")
        .arg(out_path)
        .status()?;
    println!("{}", status);
    std::fs::remove_file(c_path)?;

    println!("running");
    let status = Command::new(out_path).status()?;
    println!("{}", status);

    Ok(())
}

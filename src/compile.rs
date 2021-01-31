//! take c code string, write it to a c file, and compile that file

use crate::pos::Pos;
use std::path::Path;
use std::process::Command;

pub fn compile_program(c_code: impl AsRef<str>, path: impl AsRef<Path>) {
    Pos::reset();

    let c_path = &path.as_ref().with_extension("c");
    let out_path = &path.as_ref().with_extension("");

    std::fs::write(c_path, c_code.as_ref()).unwrap();

    println!("compiling");
    let status = Command::new("gcc")
        .arg(c_path)
        .arg("-o")
        .arg(out_path)
        .status()
        .unwrap();
    std::fs::remove_file(c_path).unwrap();
    if !status.success() {
        panic!("{}", status);
    }

    println!("running");
    let status = Command::new(out_path).status().unwrap();
    println!("{}", status);
}

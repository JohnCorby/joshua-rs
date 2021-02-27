use std::path::Path;
use std::process::Command;

pub mod define;
pub mod expr;
pub mod generics;
pub mod statement;
pub mod ty;

/// take c code string, write it to a c file, and compile that file
pub fn compile_program(c_code: impl AsRef<str>, path: impl AsRef<Path>) {
    let c_path = &path.as_ref().with_extension("c");
    let out_path = &path.as_ref().with_extension("exe");

    std::fs::write(c_path, c_code.as_ref()).unwrap();

    println!("compiling");
    let status = Command::new("clang")
        .arg(c_path)
        .arg("-o")
        .arg(out_path)
        // .arg("-Ofast")
        // .arg("-Wall")
        .status()
        .unwrap();
    // std::fs::remove_file(c_path).unwrap();
    if !status.success() {
        quit::with_code(status.code().unwrap());
    }

    println!("running");
    let status = Command::new(out_path).status().unwrap();
    if !status.success() {
        quit::with_code(status.code().unwrap());
    }
}

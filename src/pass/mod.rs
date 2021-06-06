use std::path::Path;
use std::process::{Command, ExitStatus};

pub mod ast;
pub mod gen;
pub mod generics;
pub mod ty;
pub mod type_check;
pub mod visit;

/// take c code string, write it to a c file, and compile that file
pub fn compile_program(c_code: &str, path: &Path) -> ExitStatus {
    let c_path = &path.with_extension("c");
    let out_path = &path.with_extension("exe");

    std::fs::write(c_path, c_code).unwrap();

    let status = Command::new("clang")
        .arg(c_path)
        .arg("-o")
        .arg(out_path)
        .arg("-Wall")
        // .arg("-Ofast")
        .status()
        .unwrap();
    // std::fs::remove_file(c_path).unwrap();
    if status.success() {
        println!("running");
        Command::new(out_path).status().unwrap();
    }
    status
}

use std::path::Path;
use std::process::{Command, ExitStatus};

pub mod define;
pub mod expr;
pub mod generics;
pub mod statement;
pub mod ty;

/// take c code string, write it to a c file, and compile that file
pub fn compile_program(c_code: &str, path: &Path) -> ExitStatus {
    let c_path = &path.with_extension("c");
    let out_path = &path.with_extension("exe");

    std::fs::write(c_path, c_code).unwrap();

    let status = Command::new("clang")
        .arg(c_path)
        .arg("-o")
        .arg(out_path)
        // .arg("-Ofast")
        // .arg("-Wall")
        .status()
        .unwrap();
    // std::fs::remove_file(c_path).unwrap();
    status

    // println!("running");
    // let status = Command::new(out_path).status().unwrap();
    // if !status.success() {
    //     return Err(NonZeroI32::new(status.code().unwrap()).unwrap());
    // }
    // Ok(())
}

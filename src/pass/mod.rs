use std::path::Path;
use std::process::{Command, ExitStatus};

pub mod ast1;
pub mod ast2;
pub mod gen;
pub mod generics_util;
pub mod replace_generics;
pub mod scope;
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
        let status = Command::new(out_path).status().unwrap();
        println!("{}", status);
    }
    status
}

#[derive(Debug, Copy, Clone)]
pub enum Literal {
    Float(f64),
    Int(i64),
    Bool(bool),
    Char(char),
    StrZ(&'static str),
}

#[derive(Debug, Copy, Clone, Hash, PartialEq, Default, EnumString, Display)]
#[strum(serialize_all = "snake_case")]
pub enum PrimitiveKind {
    I8,
    U8,
    I16,
    U16,
    I32,
    U32,
    I64,
    U64,
    F32,
    F64,
    Bool,
    #[default]
    Void,
}
impl PrimitiveKind {
    pub const fn c_type(&self) -> &str {
        use PrimitiveKind::*;
        match self {
            I8 => "signed char",
            U8 => "unsigned char",
            I16 => "signed short",
            U16 => "unsigned short",
            I32 => "signed int",
            U32 => "unsigned int",
            I64 => "signed long long",
            U64 => "unsigned long long",
            F32 => "float",
            F64 => "double",
            Bool => "unsigned char",
            Void => "void",
        }
    }
}

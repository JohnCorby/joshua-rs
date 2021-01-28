//! take c code string, write it to a c file, and compile that file

use crate::error::{MyError, MyResult, Pos};
use std::cell::Cell;
use std::path::Path;
use std::rc::Rc;
use tcc::{Symbol, Tcc};

pub fn compile_program(c_code: impl AsRef<str>) -> MyResult<()> {
    const OUT_PATH: &str = "test/test2.c";
    let path = Path::new(OUT_PATH);

    Pos::reset();

    std::fs::write(path, c_code.as_ref())?;

    let mut tcc = Tcc::new();
    let errored = Rc::new(Cell::new(false));
    {
        let errored = errored.clone();
        tcc.set_error_func(Some(Box::new(move |str| {
            println!("compiler error: {}", str);
            errored.set(true);
        })));
    }

    // let _ = tcc.add_file(path);
    // let _ = tcc.output_file(path.with_extension(""));
    let _ = tcc.compile_string(c_code.as_ref());
    let mut relocated = tcc
        .relocate()
        .map_err(|_| MyError::from("error relocating tcc".to_string()))?;
    let symbol = relocated
        .get_symbol("main")
        .map_err(|_| MyError::from("error getting main symbol".to_string()))?;
    unsafe {
        let ptr = symbol.as_ptr();
        dbg!(ptr);
        let ptr: fn() -> i32 = std::mem::transmute(ptr);
        let return_code = ptr();
        println!("return code is {}", return_code);
    }

    if errored.get() {
        Err("".to_string().into())
    } else {
        Ok(())
    }
}

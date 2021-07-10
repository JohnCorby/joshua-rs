use crate::parse::{Kind, Node};
use crate::pass::ast1::Program;
use crate::pass::scope::Scopes;
use crate::pass::ty::PrimitiveType;
use crate::util::RcExt;
use parking_lot::Mutex;
use std::collections::HashSet;

#[derive(Default)]
pub struct Output {
    /// general buffer, eventually this should be empty as everything is transferred to the Strings below
    pub o: String,
    pub struct_declares: String,
    pub func_declares: String,
    pub global_vars: String,
    pub struct_defines: String,
    pub func_defines: String,
}

pub fn type_check_prelude(scopes: &mut Scopes, o: &mut Output) {
    let mut i = String::new();

    fn op_funcs(
        i: &mut String,
        ops: &[&str],
        num_args: usize,
        arg_tys: &[PrimitiveType],
        ret_ty: Option<PrimitiveType>,
    ) {
        for &op in ops {
            for &arg_ty in arg_tys {
                let ret_ty = ret_ty.unwrap_or(arg_ty);
                i.push_str(&match num_args {
                    1 => format!(
                        "{} `{}`({} a) ret <{{ {} ${{ a }} }}> as {}\n",
                        ret_ty, op, arg_ty, op, ret_ty
                    ),
                    2 => format!(
                        "{} `{}`({} a, {} b) ret <{{ ${{ a }} {} ${{ b }} }}> as {}\n",
                        ret_ty, op, arg_ty, arg_ty, op, ret_ty
                    ),
                    _ => unreachable!(),
                });
            }
        }
    }

    use PrimitiveType::*;
    let num_prims = &[I8, U8, I16, U16, I32, U32, I64, U64, F32, F64];

    // binary
    op_funcs(&mut i, &["+", "-", "*", "/"], 2, num_prims, None);
    op_funcs(
        &mut i,
        &["%"],
        2,
        &[I8, U8, I16, U16, I32, U32, I64, U64],
        None,
    );
    op_funcs(
        &mut i,
        &["<", "<=", ">", ">=", "==", "!="],
        2,
        num_prims,
        Some(Bool),
    );
    op_funcs(&mut i, &["==", "!="], 2, &[Bool], Some(Bool));

    // unary
    op_funcs(&mut i, &["-"], 1, num_prims, None);
    op_funcs(&mut i, &["!"], 1, &[Bool], None);

    // cast
    for &ret_ty in num_prims {
        for &arg_ty in num_prims {
            i.push_str(&format!(
                "{} `as {}`({} a) ret <{{ ${{ a }} }}> as {}\n",
                ret_ty, ret_ty, arg_ty, ret_ty,
            ));
        }
    }

    let defines = Node::parse(i.intern(), Kind::program)
        .unwrap()
        .visit::<Program>()
        .0
        .into_inner();
    for define in defines {
        let define = define.type_check(scopes, Default::default()).unwrap();
        define.gen(o)
    }
}

pub trait Intern {
    /// insert self into static list of strings, or return already existing
    ///
    /// prevents duplicates and lets us simply copy
    fn intern(self) -> &'static str;
}
impl Intern for String {
    fn intern(self) -> &'static str {
        static LOCK: Mutex<()> = Mutex::new(());
        let lock = LOCK.lock();

        let str = unsafe {
            // thread safe because we do locking above
            static mut STRINGS: Option<HashSet<String>> = None;
            if STRINGS.is_none() {
                STRINGS = Some(Default::default())
            }
            let strings = STRINGS.as_mut().unwrap_unchecked();
            // deref is safe because hashset only grows, meaning the strings only move, but not the underlying pointer to the data
            strings.get_or_insert(self).as_str()
        };

        drop(lock);
        str
    }
}

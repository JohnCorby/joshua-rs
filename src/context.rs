use crate::parse::{Kind, Node};
use crate::pass::ast1::Program;
use crate::pass::scope::Scopes;
use crate::pass::PrimitiveKind;
use crate::util::RcExt;
use by_address::ByAddress;
use parking_lot::Mutex;
use std::collections::HashSet;
use std::lazy::SyncLazy;

#[derive(Default)]
pub struct Output {
    pub prelude: String,

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
        arg_tys: &[PrimitiveKind],
        ret_ty: Option<PrimitiveKind>,
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

    use PrimitiveKind::*;
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
        let define = define.type_check(scopes, o, Default::default()).unwrap();
        define.gen(o)
    }

    o.func_declares.clear();
    o.prelude = std::mem::take(&mut o.func_defines);
}

#[ext(name = Intern)]
pub impl String {
    /// insert self into static list of strings, or return already existing
    ///
    /// prevents duplicates and lets us simply copy
    fn intern(self) -> &'static str {
        let str: &'static str = Box::leak(self.into_boxed_str());

        static STRINGS: SyncLazy<Mutex<HashSet<&'static str>>> =
            SyncLazy::new(|| Mutex::new(HashSet::new()));
        let existing: &'static str = STRINGS.lock().get_or_insert(str);

        if ByAddress(existing) != ByAddress(str) {
            let str = str as *const str as *mut str;
            let str = unsafe { Box::from_raw(str) };
            drop(str);
        }
        existing
    }
}

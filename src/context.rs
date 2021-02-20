use crate::scope::Scopes;
use crate::ty::PrimitiveType;
use string_interner::StringInterner;

/// stores general program context
#[derive(Debug)]
pub struct Ctx<'i> {
    pub i: &'i str,
    pub o: String,

    pub scopes: Scopes<'i>,
    pub interner: StringInterner,
}

impl<'i> Ctx<'i> {
    pub fn new(i: &'i str) -> Self {
        Self {
            i,
            o: Default::default(),
            scopes: Scopes::new(),
            interner: Default::default(),
        }
    }

    /// super hacky way to slap a "runtime" onto the input
    pub fn attach_rt(i: &mut String) {
        let pre_i = &mut String::new();

        fn op_funcs<Str: AsRef<str>>(
            i: &mut String,
            ops: impl AsRef<[Str]>,
            num_args: usize,
            arg_tys: impl AsRef<[PrimitiveType]>,
            ret_tys: impl Into<Option<PrimitiveType>> + Copy,
        ) {
            for op in ops.as_ref() {
                let op = op.as_ref();
                for &arg_ty in arg_tys.as_ref() {
                    let ret_ty = ret_tys.into().unwrap_or(arg_ty);
                    i.push_str(&match num_args {
                        1 => format!(
                            "{} `{}`({} a) return <{{ {} ${{ a }} }}>",
                            ret_ty, op, arg_ty, op
                        ),
                        2 => format!(
                            "{} `{}`({} a, {} b) return <{{ ${{ a }} {} ${{ b }} }}>",
                            ret_ty, op, arg_ty, arg_ty, op
                        ),
                        _ => unreachable!(),
                    });
                }
            }
        }

        use PrimitiveType::*;
        let num_prims = [I8, U8, I16, U16, I32, U32, I64, U64, F32, F64];

        // binary
        op_funcs(pre_i, ["+", "-", "*", "/"], 2, num_prims, None);
        op_funcs(
            pre_i,
            ["%"],
            2,
            [I8, U8, I16, U16, I32, U32, I64, U64],
            None,
        );
        op_funcs(pre_i, ["<", "<=", ">", ">="], 2, num_prims, Bool);
        op_funcs(pre_i, ["==", "!="], 2, [Bool], Bool);

        // unary
        op_funcs(pre_i, ["-"], 1, num_prims, None);
        op_funcs(pre_i, ["!"], 1, [Bool], Bool);

        // cast
        for &ret_ty in &num_prims {
            for &arg_ty in &num_prims {
                pre_i.push_str(&format!(
                    "{} `as {}`({} a) return <{{ ({}) ${{ a }} }}>",
                    ret_ty,
                    ret_ty,
                    arg_ty,
                    ret_ty.c_type()
                ));
            }
        }

        i.insert_str(0, pre_i);
    }
}

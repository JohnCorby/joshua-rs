use crate::define::Program;
use crate::frozen_vec::FrozenVec;
use crate::parse::{Kind, Node};
use crate::scope::Scopes;
use crate::ty::PrimitiveType;
use string_interner::StringInterner;

/// stores general program context
#[derive(Debug)]
pub struct Ctx<'i> {
    is: &'i FrozenVec<String>,
    pub o: String,
    pub scopes: Scopes<'i>,
    pub interner: StringInterner,

    // ugly hack used for generic codegen
    pub prelude_end_index: usize,
}

impl<'i> Ctx<'i> {
    pub fn new(is: &'i FrozenVec<std::string::String>) -> Self {
        let mut ctx = Self {
            is,
            o: "".to_string(),
            scopes: Default::default(),
            interner: Default::default(),
            prelude_end_index: 0,
        };
        ctx.gen_prelude();
        ctx
    }

    pub fn new_i(&self, i: String) -> &'i str {
        self.is.push_get(i)
    }
}

impl<'i> Ctx<'i> {
    pub fn gen_prelude(&mut self) {
        let mut i = String::new();

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
                            "{} `{}`({} a) return <{{ {} ${{ a }} }}> as {}\n",
                            ret_ty, op, arg_ty, op, ret_ty
                        ),
                        2 => format!(
                            "{} `{}`({} a, {} b) return <{{ ${{ a }} {} ${{ b }} }}> as {}\n",
                            ret_ty, op, arg_ty, arg_ty, op, ret_ty
                        ),
                        _ => unreachable!(),
                    });
                }
            }
        }

        use PrimitiveType::*;
        let num_prims = [I8, U8, I16, U16, I32, U32, I64, U64, F32, F64];

        // binary
        op_funcs(&mut i, ["+", "-", "*", "/"], 2, num_prims, None);
        op_funcs(
            &mut i,
            ["%"],
            2,
            [I8, U8, I16, U16, I32, U32, I64, U64],
            None,
        );
        op_funcs(&mut i, ["<", "<=", ">", ">="], 2, num_prims, Bool);
        op_funcs(&mut i, ["==", "!="], 2, [Bool], Bool);

        // unary
        op_funcs(&mut i, ["-"], 1, num_prims, None);
        op_funcs(&mut i, ["!"], 1, [Bool], Bool);

        // cast
        for &ret_ty in &num_prims {
            for &arg_ty in &num_prims {
                i.push_str(&format!(
                    "{} `as {}`({} a) return <{{ ({}) ${{ a }} }}> as {}\n",
                    ret_ty,
                    ret_ty,
                    arg_ty,
                    ret_ty.c_type(),
                    ret_ty,
                ));
            }
        }

        self.o.push_str("// prelude\n");
        Node::parse(self.new_i(i), Kind::program)
            .unwrap()
            .visit::<Program<'i>>(self)
            .gen(self)
            .unwrap();
        self.o.push('\n');
        self.o.push_str("// end prelude\n");

        self.prelude_end_index = self.o.len();
    }
}

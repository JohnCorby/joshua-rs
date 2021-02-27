use crate::parse::{Kind, Node};
use crate::pass::define::Program;
use crate::pass::ty::PrimitiveType;
use crate::scope::Scopes;
use crate::util::frozen_vec::FrozenVec;
use crate::util::index_string::IndexString;
use string_interner::StringInterner;

/// stores general program context
#[derive(Debug)]
pub struct Ctx<'i> {
    is: &'i FrozenVec<String>,
    pub o: IndexString,
    pub scopes: Scopes<'i>,
    pub interner: StringInterner,

    // ugly internal used for generic codegen
    pub prelude_end_index: usize,
}

impl<'i> Ctx<'i> {
    pub fn new(is: &'i FrozenVec<std::string::String>) -> Self {
        let mut ctx = Self {
            is,
            o: Default::default(),
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
        op_funcs(&mut i, &["<", "<=", ">", ">="], 2, num_prims, Some(Bool));
        op_funcs(&mut i, &["==", "!="], 2, &[Bool], Some(Bool));

        // unary
        op_funcs(&mut i, &["-"], 1, num_prims, None);
        op_funcs(&mut i, &["!"], 1, &[Bool], Some(Bool));

        // cast
        for &ret_ty in num_prims {
            for &arg_ty in num_prims {
                i.push_str(&format!(
                    "{} `as {}`({} a) return <{{ ${{ a }} }}> as {}\n",
                    ret_ty, ret_ty, arg_ty, ret_ty,
                ));
            }
        }

        self.o.push_str("// begin prelude\n");
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

use crate::parse::{Kind, Node};
use crate::pass::ast::{Define, Program};
use crate::pass::ty::PrimitiveType;
use crate::scope::Scopes;
use crate::util::frozen_vec::FrozenVec;

/// stores general program context
#[derive(Debug)]
pub struct Ctx<'i> {
    pub inputs: &'i FrozenVec<String>,

    pub scopes: Scopes<'i>,
    pub extra_defines: Vec<Define<'i>>, // fixme maybe make this a program, or put program in here?

    pub o: String,
    pub structs: String,
    pub global_vars: String,
    pub func_declares: String,
    pub func_defines: String,
}

impl Ctx<'i> {
    pub fn new(inputs: &'i FrozenVec<String>) -> Self {
        Self {
            inputs,
            scopes: Default::default(),
            extra_defines: Default::default(),

            o: Default::default(),
            structs: Default::default(),
            global_vars: Default::default(),
            func_declares: Default::default(),
            func_defines: Default::default(),
        }
    }

    pub fn new_i(&self, i: String) -> &'i str {
        self.inputs.push_get(i)
    }
}

impl Ctx<'_> {
    pub fn type_check_prelude(&mut self) {
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
        op_funcs(&mut i, &["<", "<=", ">", ">="], 2, num_prims, Some(Bool));
        op_funcs(&mut i, &["==", "!="], 2, &[Bool], Some(Bool));

        // unary
        op_funcs(&mut i, &["-"], 1, num_prims, None);
        op_funcs(&mut i, &["!"], 1, &[Bool], Some(Bool));

        // cast
        for &ret_ty in num_prims {
            for &arg_ty in num_prims {
                i.push_str(&format!(
                    "{} `as {}`({} a) ret <{{ ${{ a }} }}> as {}\n",
                    ret_ty, ret_ty, arg_ty, ret_ty,
                ));
            }
        }

        let defines = Node::parse(self.new_i(i), Kind::program)
            .unwrap()
            .visit::<Program<'_>>(self)
            .0;
        for define in &defines {
            define.type_check(self).unwrap();
        }

        self.extra_defines.extend(defines);
    }
}

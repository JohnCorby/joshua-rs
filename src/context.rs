use crate::parse::{Kind, Node};
use crate::pass::ast::{CCode, CCodePart, Define, DefineKind, Program};
use crate::pass::ty::PrimitiveType;
use crate::scope::Scopes;
use crate::span::Span;
use crate::util::frozen_vec::FrozenVec;

/// stores general program context
#[derive(Debug)]
pub struct Ctx<'i> {
    pub inputs: &'i FrozenVec<String>,

    pub scopes: Scopes<'i>,
    pub extra_defines: Vec<Define<'i>>,

    pub struct_protos: String,
    pub func_protos: String,
    pub o: String,
}

impl Ctx<'i> {
    pub fn new(inputs: &'i FrozenVec<String>) -> Self {
        Self {
            inputs,
            scopes: Default::default(),
            extra_defines: Default::default(),

            struct_protos: Default::default(),
            func_protos: Default::default(),
            o: Default::default(),
        }
    }

    pub fn new_i(&self, i: String) -> &'i str {
        self.inputs.push_get(i)
    }
}

impl Ctx<'i> {
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

        let mut defines = Node::parse(self.new_i(i), Kind::program)
            .unwrap()
            .visit::<Program<'i>>(self)
            .0;
        for define in &defines {
            define.type_check(self).unwrap();
        }

        fn c_code(span: Span<'i>, string: &'i str) -> Define<'i> {
            Define {
                span,
                kind: DefineKind::CCode(CCode(vec![CCodePart::String(string)])),
            }
        }
        defines.insert(
            0,
            c_code(defines.first().unwrap().span, "#pragma region prelude"),
        );
        defines.push(c_code(
            defines.last().unwrap().span,
            "#pragma endregion prelude",
        ));
        self.extra_defines.extend(defines);
    }
}

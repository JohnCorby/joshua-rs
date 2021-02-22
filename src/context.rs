use crate::define::{Define, Program};
use crate::error::Res;
use crate::parse::{Kind, Node};
use crate::scope::Scopes;
use crate::ty::PrimitiveType;
use elsa::FrozenVec;
use std::mem::transmute;
use string_interner::StringInterner;

/// stores general program context
pub struct Ctx<'i> {
    is: FrozenVec<String>,
    pub o: String,

    pub scopes: Scopes<'i>,
    pub interner: StringInterner,
}

impl<'i> Ctx<'i> {
    pub fn new() -> Self {
        let mut ctx = Ctx {
            is: Default::default(),
            o: Default::default(),

            scopes: Scopes::new(),
            interner: Default::default(),
        };
        ctx.init_rt();
        ctx
    }

    pub fn new_i(&mut self, i: String) -> &'i str {
        self.is.push(i);
        let i = self.is.iter().last().unwrap();
        // safety: this will live for 'i because the Strings never reallocate
        // even newly added Strings can be considered having the lifetime 'i
        // because once theyre added, theyre there forever
        unsafe { transmute::<&str, &'i str>(i) }
    }
}

impl<'i> Ctx<'i> {
    pub fn init_rt(&mut self) {
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
                    "{} `as {}`({} a) return <{{ ({}) ${{ a }} }}>",
                    ret_ty,
                    ret_ty,
                    arg_ty,
                    ret_ty.c_type()
                ));
            }
        }

        Node::parse(self.new_i(i), Kind::program)
            .unwrap()
            .visit::<Program<'i>>(self)
            .gen(self)
            .unwrap();
    }

    #[allow(warnings)]
    pub fn make_func(&mut self, i: String) -> Res<'i, ()> {
        Node::parse(self.new_i(i), Kind::func_define)?
            .visit::<Define<'i>>(self)
            .gen(self)?;
        Ok(())
    }
}

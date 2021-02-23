use crate::define::{Define, Program};
use crate::error::Res;
use crate::parse::{Kind, Node};
use crate::scope::Scopes;
use crate::ty::PrimitiveType;
use std::mem::transmute;
use string_interner::StringInterner;

/// stores general program context
#[derive(Debug)]
pub struct Ctx<'i> {
    dropped: bool,

    is: Vec<Box<str>>,
    pub o: String,

    pub scopes: Scopes<'i>,
    pub interner: StringInterner,
}

impl<'i> Ctx<'i> {
    pub fn new() -> Self {
        let mut ctx = Ctx {
            dropped: false,

            is: Default::default(),
            o: Default::default(),

            scopes: Scopes::new(),
            interner: Default::default(),
        };
        ctx.init_rt();
        ctx
    }

    /// this makes sure we must manually drop ctx instead of by accident
    pub fn drop(mut self) {
        self.dropped = true;
    }

    /// # safety
    /// you must keep self alive as long as the returned &str
    ///
    /// todo at some point maybe use a safer method
    pub fn new_i(&mut self, i: String) -> &'i str {
        self.is.push(i.into_boxed_str());
        let i = &**self.is.last().unwrap();
        unsafe { transmute::<&str, &'i str>(i) }
    }
}

impl Drop for Ctx<'_> {
    fn drop(&mut self) {
        if !self.dropped {
            panic!("ctx must be manually dropped")
        }
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
        Node::parse(&self.new_i(i), Kind::func_define)?
            .visit::<Define<'i>>(self)
            .gen(self)?;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use crate::context::Ctx;

    #[test]
    #[should_panic]
    fn ub_prevention() {
        let mut ctx = Ctx::new();
        let i = ctx.o.clone(); // just get some random string
        let _i = ctx.new_i(i);
        // we should get a drop error
    }
}

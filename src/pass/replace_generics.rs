//! replaces generic placeholders with real types
//! converting back so ast1 so we have to type check again

use crate::context::Ctx;
use crate::pass::ast1::*;
use crate::pass::scope::Symbol;
use crate::util::ctx_str::CtxStr;
use crate::util::RcExt;
use std::collections::HashMap;

pub type GenericMap<'i> = HashMap<CtxStr<'i>, Type<'i>>;

impl Program<'i> {
    pub fn replace_generics(&mut self, ctx: &mut Ctx<'i>, generic_map: &GenericMap<'i>) {
        self.0.modify(|defines| {
            for define in defines {
                define.replace_generics(ctx, generic_map)
            }
        })
    }
}

impl Define<'i> {
    pub fn replace_generics(&mut self, ctx: &mut Ctx<'i>, generic_map: &GenericMap<'i>) {
        use DefineKind::*;
        match &mut self.kind {
            Struct { body, .. } => body.modify(|body| {
                for define in body {
                    define.replace_generics(ctx, generic_map)
                }
            }),
            Func {
                ty,
                generic_placeholders,
                args,
                body,
                ..
            } => {
                ty.replace_generics(ctx, generic_map);
                args.modify(|args| {
                    for arg in args {
                        arg.replace_generics(ctx, generic_map)
                    }
                });
                body.replace_generics(ctx, generic_map);

                *generic_placeholders = Default::default()
            }
            Var(var_define) => var_define.replace_generics(ctx, generic_map),
            CCode(c_code) => c_code.replace_generics(ctx, generic_map),
        };
    }
}

impl VarDefine<'i> {
    pub fn replace_generics(&mut self, ctx: &mut Ctx<'i>, generic_map: &GenericMap<'i>) {
        self.ty.replace_generics(ctx, generic_map);
        if let Some(value) = &mut self.value {
            value.replace_generics(ctx, generic_map)
        }
    }
}

impl Statement<'i> {
    pub fn replace_generics(&mut self, ctx: &mut Ctx<'i>, generic_map: &GenericMap<'i>) {
        use StatementKind::*;
        match &mut self.kind {
            Return(value) => {
                if let Some(value) = value {
                    value.replace_generics(ctx, generic_map)
                }
            }
            Break => {}
            Continue => {}
            If {
                cond,
                then,
                otherwise,
            } => {
                cond.replace_generics(ctx, generic_map);
                then.replace_generics(ctx, generic_map);
                if let Some(otherwise) = otherwise {
                    otherwise.replace_generics(ctx, generic_map)
                }
            }
            Until { cond, block } => {
                cond.replace_generics(ctx, generic_map);
                block.replace_generics(ctx, generic_map)
            }
            For {
                init,
                cond,
                update,
                block,
            } => {
                init.replace_generics(ctx, generic_map);
                cond.replace_generics(ctx, generic_map);
                update.modify(|update| update.replace_generics(ctx, generic_map));
                block.replace_generics(ctx, generic_map);
            }
            ExprAssign { lvalue, rvalue } => {
                lvalue.replace_generics(ctx, generic_map);
                rvalue.replace_generics(ctx, generic_map)
            }
            Define(define) => define.replace_generics(ctx, generic_map),
            Expr(expr) => expr.replace_generics(ctx, generic_map),
        }
    }
}

impl Block<'i> {
    pub fn replace_generics(&mut self, ctx: &mut Ctx<'i>, generic_map: &GenericMap<'i>) {
        self.0.modify(|statements| {
            for statement in statements {
                statement.replace_generics(ctx, generic_map)
            }
        })
    }
}

impl CCode<'i> {
    pub fn replace_generics(&mut self, ctx: &mut Ctx<'i>, generic_map: &GenericMap<'i>) {
        self.0.modify(|parts| {
            for part in parts {
                match part {
                    CCodePart::String(_) => {}
                    CCodePart::Expr(expr) => expr.replace_generics(ctx, generic_map),
                }
            }
        })
    }
}

impl Expr<'i> {
    pub fn replace_generics(&mut self, ctx: &mut Ctx<'i>, generic_map: &GenericMap<'i>) {
        use ExprKind::*;
        match &mut self.kind {
            Cast { thing, ty, .. } => {
                thing.modify(|thing| thing.replace_generics(ctx, generic_map));
                ty.replace_generics(ctx, generic_map);
            }
            MethodCall {
                receiver,
                func_call,
            } => {
                receiver.modify(|receiver| receiver.replace_generics(ctx, generic_map));
                func_call.replace_generics(ctx, generic_map);
            }
            Field { receiver, .. } => {
                receiver.modify(|receiver| receiver.replace_generics(ctx, generic_map))
            }
            Literal(_) => {}
            FuncCall(func_call) => func_call.replace_generics(ctx, generic_map),
            Var(_) => {}
            CCode(c_code) => c_code.replace_generics(ctx, generic_map),
        }
    }
}

impl FuncCall<'i> {
    pub fn replace_generics(&mut self, ctx: &mut Ctx<'i>, generic_map: &GenericMap<'i>) {
        self.generic_replacements.modify(|replacements| {
            for replacement in replacements {
                replacement.replace_generics(ctx, generic_map)
            }
        });
        self.args.modify(|args| {
            for arg in args {
                arg.replace_generics(ctx, generic_map)
            }
        });
    }
}

impl Type<'i> {
    pub fn replace_generics(&mut self, ctx: &mut Ctx<'i>, generic_map: &GenericMap<'i>) {
        if let TypeKind::Named { name, .. } = self.kind {
            if ctx
                .scopes
                .find(&Symbol::new_generic_placeholder_type(name), None)
                .is_err()
            {
                return;
            }
            *self = generic_map[&name].clone();
        }
    }
}

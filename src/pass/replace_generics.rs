//! replaces generic placeholders with real types

use crate::pass::ast1::*;
use crate::util::RcExt;
use std::collections::HashMap;

/// maps placeholder names to replacement types
pub type GenericMap = HashMap<&'static str, Type>;

impl Define {
    pub fn replace_generics(&mut self, generic_map: &GenericMap) {
        use DefineKind::*;
        match &mut self.kind {
            Struct { body, .. } => body.modify(|body| {
                for define in body {
                    define.replace_generics(generic_map)
                }
            }),
            Func {
                ty,
                receiver_ty,
                args,
                body,
                ..
            } => {
                ty.replace_generics(generic_map);
                if let Some(receiver_ty) = receiver_ty {
                    receiver_ty.replace_generics(generic_map)
                }
                args.modify(|args| {
                    for arg in args {
                        arg.replace_generics(generic_map)
                    }
                });
                body.replace_generics(generic_map);
            }
            Var(var_define) => var_define.replace_generics(generic_map),
            CCode(c_code) => c_code.replace_generics(generic_map),
        };
    }
}

impl VarDefine {
    pub fn replace_generics(&mut self, generic_map: &GenericMap) {
        self.ty.replace_generics(generic_map);
        if let Some(value) = &mut self.value {
            value.replace_generics(generic_map)
        }
    }
}

impl Statement {
    pub fn replace_generics(&mut self, generic_map: &GenericMap) {
        use StatementKind::*;
        match &mut self.kind {
            Return(value) => {
                if let Some(value) = value {
                    value.replace_generics(generic_map)
                }
            }
            Break => {}
            Continue => {}
            If {
                cond,
                then,
                otherwise,
            } => {
                cond.replace_generics(generic_map);
                then.replace_generics(generic_map);
                if let Some(otherwise) = otherwise {
                    otherwise.replace_generics(generic_map)
                }
            }
            Until { cond, block } => {
                cond.replace_generics(generic_map);
                block.replace_generics(generic_map)
            }
            For {
                init,
                cond,
                update,
                block,
            } => {
                init.replace_generics(generic_map);
                cond.replace_generics(generic_map);
                update.modify(|update| update.replace_generics(generic_map));
                block.replace_generics(generic_map);
            }
            ExprAssign { lvalue, rvalue } => {
                lvalue.replace_generics(generic_map);
                rvalue.replace_generics(generic_map)
            }
            Define(define) => define.replace_generics(generic_map),
            Expr(expr) => expr.replace_generics(generic_map),
        }
    }
}

impl Block {
    pub fn replace_generics(&mut self, generic_map: &GenericMap) {
        self.0.modify(|statements| {
            for statement in statements {
                statement.replace_generics(generic_map)
            }
        })
    }
}

impl CCode {
    pub fn replace_generics(&mut self, generic_map: &GenericMap) {
        self.0.modify(|parts| {
            for part in parts {
                match part {
                    CCodePart::String(_) => {}
                    CCodePart::Expr(expr) => expr.replace_generics(generic_map),
                }
            }
        })
    }
}

impl Expr {
    pub fn replace_generics(&mut self, generic_map: &GenericMap) {
        use ExprKind::*;
        match &mut self.kind {
            Cast { thing, ty, .. } => {
                thing.modify(|thing| thing.replace_generics(generic_map));
                ty.replace_generics(generic_map);
            }
            MethodCall {
                receiver,
                func_call,
            } => {
                receiver.modify(|receiver| receiver.replace_generics(generic_map));
                func_call.replace_generics(generic_map);
            }
            Field { receiver, .. } => {
                receiver.modify(|receiver| receiver.replace_generics(generic_map))
            }
            Literal(_) => {}
            FuncCall(func_call) => func_call.replace_generics(generic_map),
            Var(_) => {}
            CCode(c_code) => c_code.replace_generics(generic_map),
        }
    }
}

impl FuncCall {
    pub fn replace_generics(&mut self, generic_map: &GenericMap) {
        if let Some(receiver_ty) = &mut self.receiver_ty {
            receiver_ty.replace_generics(generic_map)
        }
        self.generic_replacements.modify(|replacements| {
            for replacement in replacements {
                replacement.replace_generics(generic_map)
            }
        });
        self.args.modify(|args| {
            for arg in args {
                arg.replace_generics(generic_map)
            }
        });
    }
}

impl Type {
    pub fn replace_generics(&mut self, generic_map: &GenericMap) {
        use TypeKind::*;
        match &mut self.kind {
            Ptr(inner) => inner.modify(|inner| inner.replace_generics(generic_map)),
            Named {
                name,
                generic_replacements,
            } => {
                if generic_map.contains_key(name) {
                    // we are a generic placeholder
                    *self = generic_map[name].clone();
                } else {
                    // we are a struct
                    generic_replacements.modify(|replacements| {
                        for replacement in replacements {
                            replacement.replace_generics(generic_map)
                        }
                    });
                }
            }
            _ => {}
        }
    }
}

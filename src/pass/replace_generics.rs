//! replaces generic placeholders with real types

use crate::pass::ast1::*;
use crate::pass::Ident;
use crate::util::RcExt;
use std::collections::HashMap;

/// maps placeholder names to replacement types
pub type GenericMap = HashMap<Ident, TypeName>;

impl Define {
    pub fn replace_generics(&mut self, generic_map: &GenericMap) {
        use Define::*;
        match self {
            Struct { body, .. } => body.modify(|x| {
                for define in x {
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
                args.modify(|x| {
                    for arg in x {
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
        use Statement::*;
        match self {
            Return(.., value) => {
                if let Some(value) = value {
                    value.replace_generics(generic_map)
                }
            }
            Break(..) => {}
            Continue(..) => {}
            If {
                cond,
                then,
                otherwise,
                ..
            } => {
                cond.replace_generics(generic_map);
                then.replace_generics(generic_map);
                if let Some(otherwise) = otherwise {
                    otherwise.replace_generics(generic_map)
                }
            }
            Until { cond, block, .. } => {
                cond.replace_generics(generic_map);
                block.replace_generics(generic_map)
            }
            For {
                init,
                cond,
                update,
                block,
                ..
            } => {
                init.replace_generics(generic_map);
                cond.replace_generics(generic_map);
                update.modify(|it| it.replace_generics(generic_map));
                block.replace_generics(generic_map);
            }
            ExprAssign { lvalue, rvalue, .. } => {
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
        self.1.modify(|statements| {
            for statement in statements {
                statement.replace_generics(generic_map)
            }
        })
    }
}

impl CCode {
    pub fn replace_generics(&mut self, generic_map: &GenericMap) {
        self.1.modify(|parts| {
            for part in parts {
                match part {
                    CCodePart::String(..) => {}
                    CCodePart::Expr(expr) => expr.replace_generics(generic_map),
                }
            }
        })
    }
}

impl Expr {
    pub fn replace_generics(&mut self, generic_map: &GenericMap) {
        use Expr::*;
        match self {
            Cast { thing, ty, .. } => {
                thing.modify(|x| x.replace_generics(generic_map));
                ty.replace_generics(generic_map);
            }
            MethodCall {
                receiver,
                func_call,
                ..
            } => {
                receiver.modify(|x| x.replace_generics(generic_map));
                func_call.replace_generics(generic_map);
            }
            Field { receiver, .. } => receiver.modify(|x| x.replace_generics(generic_map)),
            Literal(..) => {}
            FuncCall(func_call) => func_call.replace_generics(generic_map),
            Var(..) => {}
            CCode(.., c_code) => c_code.replace_generics(generic_map),
        }
    }
}

impl FuncCall {
    pub fn replace_generics(&mut self, generic_map: &GenericMap) {
        if let Some(receiver_ty) = &mut self.receiver_ty {
            receiver_ty.replace_generics(generic_map)
        }
        self.generic_replacements.modify(|it| {
            for replacement in it {
                replacement.replace_generics(generic_map)
            }
        });
        self.args.modify(|it| {
            for arg in it {
                arg.replace_generics(generic_map)
            }
        });
    }
}

impl TypeName {
    pub fn replace_generics(&mut self, generic_map: &GenericMap) {
        use TypeName::*;
        match self {
            Ptr(.., inner) => inner.modify(|x| x.replace_generics(generic_map)),
            Named {
                name,
                generic_replacements,
                ..
            } => {
                if generic_map.contains_key(name) {
                    // we are a generic placeholder
                    *self = generic_map[name].clone();
                } else {
                    // we are a struct
                    generic_replacements.modify(|x| {
                        for replacement in x {
                            replacement.replace_generics(generic_map)
                        }
                    });
                }
            }
            _ => {}
        }
    }
}

//! helper stuff for generic handling

use crate::context::Ctx;
use crate::define::{Define, DefineKind, VarDefine};
use crate::error::{err, warn_internal, Res};
use crate::expr::{Expr, FuncCall};
use crate::interned_string::InternedStr;
use crate::scope::Symbol;
use crate::statement::{Block, Statement};
use crate::ty::{Type, TypeNode};
use std::collections::HashMap;

type GenericMap<'i> = HashMap<InternedStr<'i>, Type<'i>>;

impl<'i> Ctx<'i> {
    /// create a template for a generic func using a func_define
    pub fn make_generic_func(&mut self, func_define: Define<'i>) -> Res<'i, ()> {
        if let Define {
            span,
            kind:
                DefineKind::Func {
                    ty_node,
                    name,
                    generic_placeholders,
                    args,
                    body,
                },
        } = func_define
        {
            assert!(
                !generic_placeholders.is_empty(),
                "tried to make a generic func from a normal func"
            );

            // hack so generic types resolve in args and ret type
            self.scopes.push(false, None);
            for &placeholder in &generic_placeholders {
                self.scopes
                    .add(Symbol::GenericPlaceholderType(placeholder), span)?;
            }
            let _ty = ty_node.init_ty(self)?;
            let _arg_types = args
                .iter()
                .map(|var_define| var_define.ty_node.init_ty(self))
                .collect::<Res<'i, Vec<_>>>()?;
            self.scopes.pop();

            self.scopes.add(
                Symbol::GenericFunc {
                    _ty,
                    name,
                    generic_placeholders,
                    _arg_types,

                    span,
                    ty_node,
                    args,
                    body,
                },
                span,
            )
        } else {
            unreachable!()
        }
    }

    /// use a func call to find a generic (or normal) function
    /// then specializes it using the func_call's replacements
    /// returns specialized ret type and arg types
    pub fn find_generic_func(
        &mut self,
        func_call: &FuncCall<'i>,
    ) -> Res<'i, (Type<'i>, Vec<Type<'i>>)> {
        assert!(
            !func_call.generic_replacements.is_empty(),
            "tried to specialize a non-generic func"
        );

        // already-specialized arg types from the func call
        let call_arg_types = func_call
            .args
            .iter()
            .map(|it| it.init_ty(self))
            .collect::<Res<'i, Vec<_>>>()?;

        Ok(
            match self
                .scopes
                .get_func(func_call.name, &call_arg_types, func_call.span)?
            {
                Symbol::Func {
                    ty: ret_type,
                    arg_types,
                    ..
                } => {
                    // we're actually calling a normal func, so just use that one
                    // and don't do any other special generic stuff
                    (ret_type, arg_types)
                }

                Symbol::GenericFunc {
                    name,
                    generic_placeholders,

                    span,
                    ty_node,
                    args,
                    body,
                    ..
                } => {
                    // get mapping from placeholder names to replacement types
                    let generic_map = {
                        if generic_placeholders.len() != func_call.generic_replacements.len() {
                            return err(
                                format!(
                                    "expected {} generic parameters, but got {}",
                                    generic_placeholders.len(),
                                    func_call.generic_replacements.len()
                                ),
                                func_call.span,
                            );
                        }

                        let replacement_types = func_call
                            .generic_replacements
                            .iter()
                            .map(|it| it.init_ty(self))
                            .collect::<Res<'i, Vec<_>>>()?;
                        generic_placeholders
                            .iter()
                            .copied()
                            .zip(replacement_types)
                            .collect::<GenericMap<'i>>()
                    };

                    // make specialized func
                    let mut def = Define {
                        span,
                        kind: DefineKind::Func {
                            ty_node,
                            name,
                            generic_placeholders,
                            args,
                            body,
                        },
                    };
                    def.replace_generics(&generic_map);
                    let (ret_type, arg_types) = match &def.kind {
                        DefineKind::Func { ty_node, args, .. } => (
                            ty_node.ty,
                            args.iter().map(|it| it.ty_node.ty).collect::<Vec<_>>(),
                        ),
                        _ => unreachable!(),
                    };

                    // check that specialization matches with func call
                    if arg_types != call_arg_types {
                        return err(
                            format!(
                                "specialized func args ({}) doesn't match with func call args ({})",
                                arg_types
                                    .iter()
                                    .map(|it| it.name())
                                    .collect::<Vec<_>>()
                                    .join(", "),
                                call_arg_types
                                    .iter()
                                    .map(|it| it.name())
                                    .collect::<Vec<_>>()
                                    .join(", "),
                            ),
                            func_call.span,
                        );
                    }

                    // fixme
                    warn_internal("FIXME: have this generate at the top of the output instead of at call site :P", func_call.span);
                    let mut old_o = std::mem::take(&mut self.o);
                    def.gen(self)?;
                    eprintln!("we WOULD have generated:\n{}", self.o);
                    std::mem::swap(&mut self.o, &mut old_o);

                    (ret_type, arg_types)
                }
                _ => unreachable!(),
            },
        )
    }
}

impl<'i> Define<'i> {
    fn replace_generics(&mut self, generic_map: &GenericMap<'i>) {
        use crate::define::DefineKind::*;
        match &mut self.kind {
            Struct { body, .. } => {
                for define in body {
                    define.replace_generics(generic_map)
                }
            }
            Func {
                ty_node,
                args,
                body,
                ..
            } => {
                ty_node.replace_generics(generic_map);
                for arg in args {
                    arg.replace_generics(generic_map)
                }
                body.replace_generics(generic_map)
            }
            Var(var_define) => var_define.replace_generics(generic_map),
            CCode(_) => {}
        }
    }
}
impl<'i> VarDefine<'i> {
    fn replace_generics(&mut self, generic_map: &GenericMap<'i>) {
        self.ty_node.replace_generics(generic_map);
        if let Some(value) = &mut self.value {
            value.replace_generics(generic_map)
        }
    }
}
impl<'i> Statement<'i> {
    fn replace_generics(&mut self, generic_map: &GenericMap<'i>) {
        use crate::statement::StatementKind::*;
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
                update.replace_generics(generic_map);
                block.replace_generics(generic_map);
            }
            ExprAssign { lvalue, rvalue } => {
                lvalue.replace_generics(generic_map);
                rvalue.replace_generics(generic_map)
            }
            VarDefine(var_define) => var_define.replace_generics(generic_map),
            Expr(expr) => expr.replace_generics(generic_map),
        }
    }
}
impl<'i> Block<'i> {
    fn replace_generics(&mut self, generic_map: &GenericMap<'i>) {
        for statement in &mut self.0 {
            statement.replace_generics(generic_map)
        }
    }
}
impl<'i> Expr<'i> {
    fn replace_generics(&mut self, generic_map: &GenericMap<'i>) {
        use crate::expr::ExprKind::*;
        match &mut self.kind {
            Binary { left, right, .. } => {
                left.replace_generics(generic_map);
                right.replace_generics(generic_map)
            }
            Unary { thing, .. } => thing.replace_generics(generic_map),
            Cast { thing, ty_node } => {
                thing.replace_generics(generic_map);
                ty_node.replace_generics(generic_map)
            }
            MethodCall {
                receiver,
                func_call,
            } => {
                receiver.replace_generics(generic_map);
                func_call.replace_generics(generic_map)
            }
            Field { receiver, .. } => receiver.replace_generics(generic_map),
            Literal(_) => {}
            FuncCall(func_call) => func_call.replace_generics(generic_map),
            Var(_) => {}
            CCode(_) => {}
        }
        self._ty = Default::default()
    }
}
impl<'i> FuncCall<'i> {
    fn replace_generics(&mut self, generic_map: &GenericMap<'i>) {
        for replacement in &mut self.generic_replacements {
            replacement.replace_generics(generic_map)
        }
        for arg in &mut self.args {
            arg.replace_generics(generic_map)
        }
        self._cached = Default::default()
    }
}
impl<'i> TypeNode<'i> {
    fn replace_generics(&mut self, generic_map: &GenericMap<'i>) {
        if let Type::GenericPlaceholder(name) = self.ty {
            self.ty = generic_map[&name];
            self._ty = Default::default()
        }
    }
}

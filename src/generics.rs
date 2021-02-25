//! helper stuff for generic handling

use crate::context::Ctx;
use crate::define::{Define, DefineKind, VarDefine};
use crate::error::{err, Res};
use crate::expr::{Expr, FuncCall};
use crate::interned_string::InternedStr;
use crate::scope::{Scopes, Symbol};
use crate::statement::{Block, Statement};
use crate::ty::{Type, TypeNode};
use std::collections::HashMap;

type GenericMap<'i> = HashMap<InternedStr<'i>, Type<'i>>;

impl<'i> Define<'i> {
    /// create a template for a generic func using a func_define
    pub fn gen_generic(self, ctx: &mut Ctx<'i>) -> Res<'i, ()> {
        if let Self {
            span,
            kind:
                DefineKind::Func {
                    ty_node,
                    name,
                    generic_placeholders,
                    args,
                    body,
                },
        } = self
        {
            assert!(!generic_placeholders.is_empty());

            // hack so generic types resolve in args and ret type
            ctx.scopes.push(false, None);
            for &placeholder in &generic_placeholders {
                ctx.scopes
                    .add(Symbol::GenericPlaceholderType(placeholder), span)?;
            }
            let _ty = ty_node.init_ty(ctx)?;
            let _arg_types = args
                .iter()
                .map(|var_define| var_define.ty_node.init_ty(ctx))
                .collect::<Res<'i, Vec<_>>>()?;
            ctx.scopes.pop();

            ctx.scopes.add(
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
}

impl<'i> FuncCall<'i> {
    /// use a func call to find a generic (or normal) function
    /// then specializes it using the func_call's replacements
    /// returns specialized ret type and arg types
    pub fn init_ty_generic(&self, ctx: &mut Ctx<'i>) -> Res<'i, Type<'i>> {
        assert!(!self.generic_replacements.is_empty(),);

        // already-specialized arg types from the func call
        let call_arg_types = self
            .args
            .iter()
            .map(|it| it.init_ty(ctx))
            .collect::<Res<'i, Vec<_>>>()?;

        Ok(
            match ctx.scopes.get_func(self.name, &call_arg_types, self.span)? {
                Symbol::Func { ty: ret_type, .. } => {
                    // we're actually calling a normal func, so just use that one
                    // and don't do any other special generic stuff
                    ret_type
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
                        if generic_placeholders.len() != self.generic_replacements.len() {
                            return err(
                                format!(
                                    "expected {} generic parameters, but got {}",
                                    generic_placeholders.len(),
                                    self.generic_replacements.len()
                                ),
                                self.span,
                            );
                        }

                        let replacement_types = self
                            .generic_replacements
                            .iter()
                            .map(|it| it.init_ty(ctx))
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
                                    .map(Type::to_string)
                                    .collect::<Vec<_>>()
                                    .join(", "),
                                call_arg_types
                                    .iter()
                                    .map(Type::to_string)
                                    .collect::<Vec<_>>()
                                    .join(", "),
                            ),
                            self.span,
                        );
                    }

                    let scopes_after_top_level = ctx.scopes.0.split_off(1);
                    let o_after_prelude = ctx.o.split_off(ctx.prelude_end_index);
                    ctx.o.push_str("// specialized generic func\n");
                    def.gen(ctx)?;
                    ctx.o.push('\n');
                    ctx.o.push_str("// end specialized generic func\n");
                    ctx.scopes.0.extend(scopes_after_top_level);
                    ctx.o.push_str(&o_after_prelude);

                    ret_type
                }
                _ => unreachable!(),
            },
        )
    }
}

impl<'i> Scopes<'i> {
    /// find a generic func fuzzily
    pub fn find_generic_func(&self, symbol: &Symbol<'i>) -> Option<Symbol<'i>> {
        if let Symbol::Func {
            name: name1,
            arg_types: arg_types1,
            ..
        } = symbol
        {
            if let Some(symbol) = self.0[0].symbols.iter().find(|&s| {
                if let Symbol::GenericFunc {
                    name: name2,
                    _arg_types: arg_types2,
                    ..
                } = s
                {
                    if name1 != name2 {
                        return false;
                    }
                    for (&ty1, &ty2) in arg_types1.iter().zip(arg_types2.iter()) {
                        if let Type::GenericPlaceholder(_) = ty2 {
                            // generic ty2 will always match ty1, so don't return false
                        } else if ty1 != ty2 {
                            return false;
                        }
                    }
                    true
                } else {
                    false
                }
            }) {
                Some(symbol.clone())
            } else {
                None
            }
        } else {
            unreachable!()
        }
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
                generic_placeholders,
                args,
                body,
                ..
            } => {
                ty_node.replace_generics(generic_map);
                for arg in args {
                    arg.replace_generics(generic_map)
                }
                body.replace_generics(generic_map);
                generic_placeholders.clear()
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
        self._ty = Default::default()
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

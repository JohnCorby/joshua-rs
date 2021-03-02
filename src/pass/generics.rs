//! helper stuff for generic handling

use crate::context::Ctx;
use crate::error::{err, Res};
use crate::pass::define::{Define, DefineKind, VarDefine};
use crate::pass::expr::{Expr, ExprKind, FuncCall};
use crate::pass::statement::{Block, Statement, StatementKind};
use crate::pass::ty::{Type, TypeKind, TypeNode};
use crate::scope::{Scopes, Symbol};
use crate::util::interned_str::InternedStr;
use std::collections::HashMap;
use std::ops::Deref;

type GenericMap<'i> = HashMap<InternedStr<'i>, Type<'i>>;

/*impl<'i> FuncCall<'i> {
    /// use a func call to find a generic (or normal) function
    /// then specializes it using the func_call's replacements
    /// returns specialized ret type and arg types
    pub fn type_check_generic(&self, ctx: &mut Ctx<'i>) -> Res<'i, Type<'i>> {
        assert!(!self.generic_replacements.is_empty());

        // already-specialized arg types from the func call
        let call_arg_types = self.args.iter().map(|it| *it.ty).collect::<Vec<_>>();

        if let Ok(Symbol::Func { ty: ret_type, .. }) =
            ctx.scopes
                .get_func(self.name, &call_arg_types, Some(self.span))
        {
            // we're actually calling a normal func, so just use that one
            // and don't do any other special generic stuff
            Ok(ret_type)
        } else if let Some(Symbol::GenericFunc {
            span,
            ty_node,
            name,
            generic_placeholders,
            args,
            body,

            scopes_index,
            o_index,
            ..
        }) = ctx.scopes.find_generic_func(self.name, &call_arg_types)
        {
            // get mapping from placeholder names to replacement types
            let generic_map = {
                if generic_placeholders.len() != self.generic_replacements.len() {
                    return err(
                        &format!(
                            "expected {} generic parameters, but got {}",
                            generic_placeholders.len(),
                            self.generic_replacements.len()
                        ),
                        Some(self.span),
                    );
                }

                let replacement_types = self
                    .generic_replacements
                    .iter()
                    .map(|it| *it.ty)
                    .collect::<Vec<_>>();
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
                    body: body.deref().clone(),
                },
            };
            def.replace_generics(&generic_map);
            let (ret_type, arg_types) = match &def.kind {
                DefineKind::Func { ty_node, args, .. } => (
                    ty_node.init_ty(ctx)?,
                    args.iter()
                        .map(|it| it.ty_node.init_ty(ctx))
                        .collect::<Res<'i, Vec<_>>>()?,
                ),
                _ => unreachable!(),
            };

            // check that specialization matches with func call
            if arg_types != call_arg_types {
                return err(
                    &format!(
                        "specialized generic func args ({}) doesn't match with func call args ({})",
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
                    Some(self.span),
                );
            }

            let scopes_after = ctx.scopes.0.split_off(scopes_index);
            let current_o = std::mem::take(&mut ctx.o);
            ctx.o.push_str("#pragma region specialized generic func\n");
            def.gen(ctx);
            ctx.o.push('\n');
            ctx.o
                .push_str("#pragma endregion specialized generic func\n");
            let generated_o = std::mem::replace(&mut ctx.o, current_o);
            ctx.o.insert_str(o_index, &generated_o);
            ctx.scopes.0.extend(scopes_after);

            Ok(ret_type)
        } else {
            err(
                &format!(
                    "could not find {}",
                    Symbol::Func {
                        ty: Default::default(),
                        name: self.name,
                        arg_types: call_arg_types,
                    }
                ),
                Some(self.span),
            )
        }
    }
}

impl<'i> Scopes<'i> {
    /// find a generic func fuzzily
    pub fn find_generic_func(
        &self,
        name: InternedStr<'i>,
        arg_types: &[Type<'i>],
    ) -> Option<Symbol<'i>> {
        self.0[0]
            .symbols
            .iter()
            .find(|&s| {
                if let Symbol::GenericFunc {
                    name: other_name,
                    arg_types: other_arg_types,
                    ..
                } = s
                {
                    if &name != other_name {
                        return false;
                    }
                    if arg_types.len() != other_arg_types.len() {
                        return false;
                    }
                    for (&ty, &other_ty) in arg_types.iter().zip(other_arg_types.iter()) {
                        if let Type::GenericPlaceholder(_) = other_ty {
                            // generic other_ty will always match ty, so don't return false
                        } else if ty != other_ty {
                            return false;
                        }
                    }
                    true
                } else {
                    false
                }
            })
            .cloned()
    }
}*/

// impl replace generics
impl<'i> Define<'i> {
    fn replace_generics(&mut self, generic_map: &GenericMap<'i>) {
        use DefineKind::*;
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
        use ExprKind::*;
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
        self.ty = Default::default()
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
        self.ty = Default::default()
    }
}
impl<'i> TypeNode<'i> {
    fn replace_generics(&mut self, generic_map: &GenericMap<'i>) {
        if let Type::GenericPlaceholder(name) = *self.ty {
            self.kind = match &generic_map[&name] {
                Type::Primitive(ty) => TypeKind::Primitive(*ty),
                Type::Struct(name) => TypeKind::Named(*name),
                ty => unreachable!("replacing generic with {}", ty),
            };
            self.ty = Default::default()
        }
    }
}

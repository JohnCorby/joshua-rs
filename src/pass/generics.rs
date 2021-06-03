//! helper stuff for generic handling

use crate::context::Ctx;
use crate::error::{err, Res};
use crate::pass::ast::*;
use crate::pass::ty::Type;
use crate::scope::{Scopes, Symbol};
use crate::span::Span;
use crate::util::interned_str::InternedStr;
use std::collections::HashMap;
use std::ops::Deref;

impl Define<'i> {
    pub fn type_check_generic(&self, ctx: &mut Ctx<'i>) -> Res<'i, ()> {
        match &self.kind {
            DefineKind::Func {
                ty_node,
                name,
                generic_placeholders,
                args,
                body,
            } => {
                debug_assert!(!generic_placeholders.is_empty());

                ctx.scopes.push(false, None);
                // add placeholders
                for &placeholder in generic_placeholders {
                    ctx.scopes
                        .add(Symbol::GenericPlaceholderType(placeholder), Some(self.span))?;
                }
                ty_node.type_check(ctx)?;
                ctx.scopes.push(false, Some(ty_node.ty.deref().clone()));
                for arg in args {
                    arg.type_check(ctx)?
                }
                body.type_check(ctx)?;
                ctx.scopes.check_return_called(Some(self.span))?;
                ctx.scopes.pop();
                ctx.scopes.pop();

                // add symbol
                ctx.scopes.add(
                    Symbol::GenericFunc {
                        ty: ty_node.ty.deref().clone(),
                        arg_types: args
                            .iter()
                            .map(|it| it.ty_node.ty.deref().clone())
                            .collect::<Vec<_>>(),

                        span: self.span,
                        ty_node: ty_node.clone(),
                        name: *name,
                        generic_placeholders: generic_placeholders.clone(),
                        args: args.clone(),
                        body: body.clone().into(),

                        scopes_index: ctx.scopes.0.len(),
                    },
                    Some(self.span),
                )
            }
            _ => unreachable!(),
        }
    }
}

type GenericMap<'i> = HashMap<InternedStr<'i>, Type<'i>>;

impl FuncCall<'i> {
    pub fn type_check_generic(&self, ctx: &mut Ctx<'i>) -> Res<'i, ()> {
        debug_assert!(!self.generic_replacements.is_empty());

        for replacement in &self.generic_replacements {
            replacement.type_check(ctx)?
        }
        for arg in &self.args {
            arg.type_check(ctx)?
        }

        // find an associated generic func
        let symbol = ctx
            .scopes
            .find_generic_func(
                self.name,
                &self
                    .generic_replacements
                    .iter()
                    .map(|it| it.ty.deref())
                    .collect::<Vec<_>>(),
                &self.args.iter().map(|it| it.ty.deref()).collect::<Vec<_>>(),
                Some(self.span),
            )?
            .clone();
        if let Symbol::GenericFunc {
            span,
            ty_node,
            name,
            generic_placeholders,
            args,
            body,

            scopes_index,
            ..
        } = symbol
        {
            // get mapping from placeholder names to replacement types
            let generic_map = {
                let generic_replacements = self
                    .generic_replacements
                    .iter()
                    .map(|it| it.ty.deref().clone())
                    .collect::<Vec<_>>();
                generic_placeholders
                    .iter()
                    .copied()
                    .zip(generic_replacements)
                    .collect::<GenericMap<'i>>()
            };

            // make specialized func
            let mut func_define = Define {
                span,
                kind: DefineKind::Func {
                    ty_node,
                    name,
                    generic_placeholders,
                    args,
                    body: body.deref().clone(),
                },
            };
            func_define.replace_generics(&generic_map);

            // type check
            let scopes_after = ctx.scopes.0.split_off(scopes_index);
            func_define.type_check(ctx)?;
            ctx.scopes.0.extend(scopes_after);

            // init type
            self.ty.init(match &func_define.kind {
                DefineKind::Func { ty_node, .. } => ty_node.ty.deref().clone(),
                _ => unreachable!(),
            });

            ctx.extra_defines.push(func_define);

            Ok(())
        } else {
            unreachable!()
        }
    }
}

impl Scopes<'i> {
    /// find a generic func fuzzily
    pub fn find_generic_func(
        &self,
        name: InternedStr<'i>,
        generic_replacements: &[&Type<'i>],
        arg_types: &[&Type<'i>],
        span: Option<Span<'i>>,
    ) -> Res<'i, &Symbol<'i>> {
        for scope in self.0.iter().rev() {
            let symbol = scope.symbols.iter().find(|&s| {
                if let Symbol::GenericFunc {
                    name: other_name,
                    generic_placeholders,
                    arg_types: other_arg_types,
                    ..
                } = s
                {
                    if &name != other_name {
                        return false;
                    }

                    if generic_placeholders.len() != generic_replacements.len() {
                        return false;
                    }

                    if arg_types.len() != other_arg_types.len() {
                        return false;
                    }
                    for (&ty, other_ty) in arg_types.iter().zip(other_arg_types.iter()) {
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
            });
            if let Some(symbol) = symbol {
                return Ok(symbol);
            }
        }
        err(
            &format!(
                "could not find generic func name `{}` and arg_types ({}) \
                that matches generic replacements ({})",
                name,
                arg_types
                    .iter()
                    .map(|it| it.to_string())
                    .collect::<Vec<_>>()
                    .join(", "),
                generic_replacements
                    .iter()
                    .map(|it| it.to_string())
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            span,
        )
    }
}

// impl replace generics
// note: this resets types, so you gotta type_check again
impl Define<'i> {
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
impl VarDefine<'i> {
    fn replace_generics(&mut self, generic_map: &GenericMap<'i>) {
        self.ty_node.replace_generics(generic_map);
        if let Some(value) = &mut self.value {
            value.replace_generics(generic_map)
        }
    }
}
impl Statement<'i> {
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
impl Block<'i> {
    fn replace_generics(&mut self, generic_map: &GenericMap<'i>) {
        for statement in &mut self.0 {
            statement.replace_generics(generic_map)
        }
    }
}
impl Expr<'i> {
    fn replace_generics(&mut self, generic_map: &GenericMap<'i>) {
        use ExprKind::*;
        match &mut self.kind {
            Cast { thing, ty_node } => {
                thing.replace_generics(generic_map);
                ty_node.replace_generics(generic_map);
            }
            Field { receiver, .. } => receiver.replace_generics(generic_map),
            Literal(_) => {}
            FuncCall(func_call) => func_call.replace_generics(generic_map),
            Var(_) => {}
            CCode(_) => {}
        }

        self.ty = Default::default();
    }
}
impl FuncCall<'i> {
    fn replace_generics(&mut self, generic_map: &GenericMap<'i>) {
        for replacement in &mut self.generic_replacements {
            replacement.replace_generics(generic_map)
        }
        for arg in &mut self.args {
            arg.replace_generics(generic_map)
        }

        self.ty = Default::default();
    }
}
impl TypeNode<'i> {
    fn replace_generics(&mut self, generic_map: &GenericMap<'i>) {
        if let Type::GenericPlaceholder(name) = *self.ty {
            self.kind = match &generic_map[&name] {
                Type::Primitive(ty) => TypeKind::Primitive(*ty),
                Type::Struct(name) => TypeKind::Named(*name),
                ty => unreachable!("replacing generic with {}", ty),
            };
        }

        self.ty = Default::default();
    }
}

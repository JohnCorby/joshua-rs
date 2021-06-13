//! type init, nesting prefix init, type check, symbol add, symbol check

use crate::context::Ctx;
use crate::error::{err, Res};
use crate::pass::ast1::*;
use crate::pass::ast2;
use crate::pass::scope::{Scope, Symbol};
use crate::pass::ty::PrimitiveType;
use crate::util::ctx_str::IntoCtx;
use crate::util::{IterExt, IterResExt, RcExt};
use std::collections::HashMap;
use std::ops::Deref;

impl Program<'i> {
    pub fn type_check(self, ctx: &mut Ctx<'i>) -> Res<'i, ast2::Program<'i>> {
        ctx.scopes.push(Scope::new(None, false, None));
        ctx.type_check_prelude();
        let defines = self
            .0
            .iter()
            .cloned()
            .map(|define| define.type_check(ctx))
            .res_vec()?;
        ctx.scopes.pop();
        Ok(ast2::Program(defines.into()))
    }
}

impl Define<'i> {
    pub fn type_check(self, ctx: &mut Ctx<'i>) -> Res<'i, ast2::Define<'i>> {
        use DefineKind::*;
        Ok(match self.kind.clone() {
            Struct {
                name,
                generic_placeholders,
                body,
            } => {
                if generic_placeholders.is_empty() {
                    let (var_defines, func_defines) =
                        body.iter()
                            .cloned()
                            .partition::<Vec<_>, _>(|define| match define.kind {
                                Var(_) => true,
                                Func { .. } => false,
                                _ => panic!("struct body shouldn't have {:?}", define),
                            });

                    ctx.scopes.push(Scope::new(None, false, None));
                    let var_defines = var_defines
                        .into_iter()
                        .map(|define| define.type_check(ctx))
                        .res_vec()?;
                    ctx.scopes.pop();

                    // add symbol
                    let nesting_prefix = ctx.scopes.nesting_prefix().into_ctx(ctx);
                    ctx.scopes.add(
                        Symbol::StructType {
                            nesting_prefix,
                            name,
                            generic_replacements: Default::default(),
                            field_types: var_defines
                                .iter()
                                .cloned()
                                .map(|define| match define {
                                    ast2::Define::Var(ast2::VarDefine { name, ty, .. }) => {
                                        (name, ty)
                                    }
                                    _ => unreachable!(),
                                })
                                .collect::<HashMap<_, _>>()
                                .into(),
                        },
                        Some(self.span),
                    )?;

                    let func_defines = func_defines
                        .into_iter()
                        .map(|mut define| {
                            // attach struct name to func
                            if let Func {
                                name: func_name, ..
                            } = &mut define.kind
                            {
                                *func_name = format!("{}::{}", name, func_name).into_ctx(ctx)
                            }

                            define.type_check(ctx)
                        })
                        .res_vec()?;

                    let mut body = var_defines;
                    body.extend(func_defines);

                    ast2::Define::Struct {
                        full_name: format!("{}{}", nesting_prefix, name).into_ctx(ctx),
                        generic_replacements: Default::default(),
                        body: body.into(),
                    }
                } else {
                    self.type_check_generic(ctx)?
                }
            }
            Func {
                ty,
                name,
                generic_placeholders,
                args,
                body,
            } => {
                if generic_placeholders.is_empty() {
                    let ty = ty.type_check(ctx)?;
                    ctx.scopes
                        .push(Scope::new(Some(name), false, Some(ty.clone())));
                    let args = args
                        .iter()
                        .cloned()
                        .map(|arg| arg.type_check(ctx))
                        .res_vec()?;

                    // add symbol
                    let scope = ctx.scopes.pop();
                    let nesting_prefix = ctx.scopes.nesting_prefix().into_ctx(ctx);
                    ctx.scopes.add(
                        Symbol::Func {
                            ty: ty.clone(),
                            nesting_prefix,
                            name,
                            generic_replacements: Default::default(),
                            arg_types: args.iter().cloned().map(|it| it.ty).vec().into(),
                        },
                        Some(self.span),
                    )?;
                    ctx.scopes.push(scope);

                    let body = body.type_check(ctx)?;
                    ctx.scopes.check_return_called(Some(self.span))?;
                    ctx.scopes.pop();

                    ast2::Define::Func {
                        ty,
                        full_name: format!("{}{}", nesting_prefix, name).into_ctx(ctx),
                        generic_replacements: Default::default(),
                        args: args.into(),
                        body,
                    }
                } else {
                    self.type_check_generic(ctx)?
                }
            }
            Var(var_define) => ast2::Define::Var(var_define.type_check(ctx)?),
            CCode(c_code) => ast2::Define::CCode(c_code.type_check(ctx)?),
        })
    }
}

impl VarDefine<'i> {
    pub fn type_check(self, ctx: &mut Ctx<'i>) -> Res<'i, ast2::VarDefine<'i>> {
        // special case for auto type, where lvalue is inferred from rvalue instead of the other way around
        let (ty, value) = if let TypeKind::Auto = self.ty.kind {
            if let Some(value) = self.value {
                let value = value.type_check(ctx, None)?;
                let ty = value.ty.clone();
                (ty, Some(value))
            } else {
                return err("cannot infer type", Some(self.span));
            }
        } else {
            let ty = self.ty.type_check(ctx)?;
            if let Some(value) = self.value {
                let value = value.type_check(ctx, Some(&ty))?;

                // check matching
                value.ty.check(&ty, Some(self.span))?;
                (ty, Some(value))
            } else {
                (ty, None)
            }
        };

        // add symbol
        ctx.scopes.add(
            Symbol::Var {
                ty: ty.clone(),
                name: self.name,
            },
            Some(self.span),
        )?;

        Ok(ast2::VarDefine {
            ty,
            name: self.name,
            value,
        })
    }
}

impl Statement<'i> {
    pub fn type_check(self, ctx: &mut Ctx<'i>) -> Res<'i, ast2::Statement<'i>> {
        use StatementKind::*;
        Ok(match self.kind {
            Return(value) => {
                let value = if let Some(value) = value {
                    Some(value.type_check(ctx, Some(&ctx.scopes.func_return_type().clone()))?)
                } else {
                    None
                };

                // check return type
                ctx.scopes.return_called();
                value
                    .as_ref()
                    .map(|it| &it.ty)
                    .unwrap_or(&PrimitiveType::Void.ty())
                    .check(ctx.scopes.func_return_type(), Some(self.span))?;

                ast2::Statement::Return(value)
            }
            Break => {
                if !ctx.scopes.in_loop() {
                    return err("break can't be used outside of loops", Some(self.span));
                }
                ast2::Statement::Break
            }
            Continue => {
                if !ctx.scopes.in_loop() {
                    return err("continue can't be used outside of loops", Some(self.span));
                }
                ast2::Statement::Continue
            }
            If {
                cond,
                then,
                otherwise,
            } => {
                let cond = cond.type_check(ctx, Some(&PrimitiveType::Bool.ty()))?;
                ctx.scopes.push(Scope::new(None, false, None));
                let then = then.type_check(ctx)?;
                ctx.scopes.pop();
                let otherwise = if let Some(otherwise) = otherwise {
                    ctx.scopes.push(Scope::new(None, false, None));
                    let otherwise = otherwise.type_check(ctx)?;
                    ctx.scopes.pop();
                    Some(otherwise)
                } else {
                    None
                };

                // check condition
                cond.ty.check(&PrimitiveType::Bool.ty(), Some(self.span))?;

                ast2::Statement::If {
                    cond,
                    then,
                    otherwise,
                }
            }
            Until { cond, block } => {
                let cond = cond.type_check(ctx, Some(&PrimitiveType::Bool.ty()))?;
                ctx.scopes.push(Scope::new(None, true, None));
                let block = block.type_check(ctx)?;
                ctx.scopes.pop();

                // check condition
                cond.ty.check(&PrimitiveType::Bool.ty(), Some(self.span))?;

                ast2::Statement::Until { cond, block }
            }
            For {
                init,
                cond,
                update,
                block,
            } => {
                ctx.scopes.push(Scope::new(None, true, None));
                let init = init.type_check(ctx)?;
                let cond = cond.type_check(ctx, Some(&PrimitiveType::Bool.ty()))?;
                let update = update.deref().clone().type_check(ctx)?;
                let block = block.type_check(ctx)?;
                ctx.scopes.pop();

                // check condition
                cond.ty.check(&PrimitiveType::Bool.ty(), Some(self.span))?;

                ast2::Statement::For {
                    init,
                    cond,
                    update: update.into(),
                    block,
                }
            }
            ExprAssign { lvalue, rvalue } => {
                lvalue.check_assignable(Some(self.span))?;

                let lvalue = lvalue.type_check(ctx, None)?;
                let rvalue = rvalue.type_check(ctx, Some(&lvalue.ty))?;

                // check matching
                rvalue.ty.check(&lvalue.ty, Some(self.span))?;

                ast2::Statement::ExprAssign { lvalue, rvalue }
            }
            Define(define) => ast2::Statement::Define(define.type_check(ctx)?),
            Expr(expr) => ast2::Statement::Expr(expr.type_check(ctx, None)?),
        })
    }
}

impl Block<'i> {
    pub fn type_check(self, ctx: &mut Ctx<'i>) -> Res<'i, ast2::Block<'i>> {
        self.0
            .iter()
            .cloned()
            .map(|statement| statement.type_check(ctx))
            .res_vec()
            .map(|statements| ast2::Block(statements.into()))
    }
}

impl CCode<'i> {
    pub fn type_check(self, ctx: &mut Ctx<'i>) -> Res<'i, ast2::CCode<'i>> {
        self.0
            .iter()
            .cloned()
            .map(|part| {
                Ok(match part {
                    CCodePart::String(str) => ast2::CCodePart::String(str),
                    CCodePart::Expr(expr) => ast2::CCodePart::Expr(expr.type_check(ctx, None)?),
                })
            })
            .res_vec()
            .map(|parts| ast2::CCode(parts.into()))
    }
}

impl Expr<'i> {
    pub fn type_check(
        self,
        ctx: &mut Ctx<'i>,
        type_hint: Option<&ast2::Type<'i>>,
    ) -> Res<'i, ast2::Expr<'i>> {
        use ExprKind::*;
        Ok(match self.kind {
            Cast { thing, ty } => {
                let ty = ty.type_check(ctx)?;
                let thing = thing.deref().clone().type_check(ctx, Some(&ty))?;

                // symbol check
                // fixme hacky as shit
                let nesting_prefix = if let ast2::Type::Literal(_) | ast2::Type::CCode = thing.ty {
                    // casting will always work
                    Default::default()
                } else {
                    let name = format!("as {}", ty.encoded_name()).into_ctx(ctx);
                    let symbol = ctx.scopes.find(
                        &Symbol::new_func(name, Default::default(), vec![thing.ty.clone()].into()),
                        Some(self.span),
                    )?;
                    debug_assert_eq!(ty, symbol.ty());
                    match symbol {
                        Symbol::Func {
                            nesting_prefix: symbol_nesting_prefix,
                            ..
                        } => *symbol_nesting_prefix,
                        _ => unreachable!(),
                    }
                };

                ast2::Expr {
                    kind: ast2::ExprKind::Cast {
                        nesting_prefix,
                        thing: thing.into(),
                    },
                    ty,
                }
            }
            MethodCall {
                receiver,
                mut func_call,
            } => {
                let receiver = receiver.deref().clone();
                func_call
                    .args
                    .modify(|args| args.insert(0, receiver.clone()));

                let receiver = receiver.type_check(ctx, None)?;
                func_call.name =
                    format!("{}::{}", receiver.ty.encoded_name(), func_call.name).into_ctx(ctx);

                func_call.type_check(ctx, type_hint)?
            }
            Field { receiver, var } => {
                let receiver = receiver.deref().clone().type_check(ctx, None)?;

                // field check
                let symbol = match receiver.ty.clone() {
                    ast2::Type::Struct {
                        name,
                        generic_replacements,
                        ..
                    } => Symbol::new_struct_type(name, generic_replacements),
                    ty => {
                        return err(
                            &format!("expected struct type, but got {}", ty),
                            Some(self.span),
                        )
                    }
                };
                let symbol = ctx.scopes.find(&symbol, Some(self.span))?;
                let field_types = match symbol {
                    Symbol::StructType { field_types, .. } => field_types,
                    _ => unreachable!(),
                };
                let ty = match field_types.get(&var) {
                    Some(field_type) => field_type.clone(),
                    None => {
                        return err(
                            &format!("no field named {} in {}", var, symbol),
                            Some(self.span),
                        )
                    }
                };

                ast2::Expr {
                    kind: ast2::ExprKind::Field {
                        receiver: receiver.into(),
                        var,
                    },
                    ty,
                }
            }
            Literal(literal) => ast2::Expr {
                kind: ast2::ExprKind::Literal(literal),
                ty: literal.ty(),
            },
            FuncCall(func_call) => func_call.type_check(ctx, type_hint)?,
            Var(name) => {
                // symbol check
                let ty = ctx
                    .scopes
                    .find(&Symbol::new_var(name), Some(self.span))?
                    .ty();
                ast2::Expr {
                    kind: ast2::ExprKind::Var(name),
                    ty,
                }
            }
            CCode(c_code) => ast2::Expr {
                kind: ast2::ExprKind::CCode(c_code.type_check(ctx)?),
                ty: ast2::Type::CCode,
            },
        })
    }
}

impl FuncCall<'i> {
    pub fn type_check(
        self,
        ctx: &mut Ctx<'i>,
        type_hint: Option<&ast2::Type<'i>>,
    ) -> Res<'i, ast2::Expr<'i>> {
        if self.generic_replacements.is_empty() {
            let args = self
                .args
                .iter()
                .cloned()
                .map(|arg| arg.type_check(ctx, None))
                .res_vec()?;

            // symbol check
            let symbol = ctx.scopes.find(
                &Symbol::new_func(
                    self.name,
                    Default::default(),
                    args.clone().into_iter().map(|it| it.ty).vec().into(),
                ),
                Some(self.span),
            )?;
            let nesting_prefix = match symbol {
                Symbol::Func { nesting_prefix, .. } => nesting_prefix,
                _ => unreachable!(),
            };
            Ok(ast2::Expr {
                kind: ast2::ExprKind::FuncCall {
                    full_name: format!("{}{}", nesting_prefix, self.name).into_ctx(ctx),
                    generic_replacements: Default::default(),
                    args: args.into(),
                },
                ty: symbol.ty(),
            })
        } else {
            self.type_check_generic(ctx)
        }
    }
}

impl Type<'i> {
    pub fn type_check(self, ctx: &mut Ctx<'i>) -> Res<'i, ast2::Type<'i>> {
        let span = self.span;
        use TypeKind::*;
        Ok(match self.kind.clone() {
            Primitive(ty) => ast2::Type::Primitive(ty),
            Ptr(inner) => ast2::Type::Ptr(inner.deref().clone().type_check(ctx)?.into()),
            Named {
                name,
                generic_replacements,
            } => {
                if generic_replacements.is_empty() {
                    // symbol check
                    ctx.scopes
                        .find(
                            &Symbol::new_struct_type(name, Default::default()),
                            Some(span),
                        )
                        .map(|symbol| symbol)
                        .or_else(|_| {
                            ctx.scopes
                                .find(&Symbol::new_generic_placeholder_type(name), Some(span))
                        })
                        .or_else(|_| {
                            err(
                                &format!("could not find type symbol `{}`", name),
                                Some(span),
                            )
                        })?
                        .ty()
                } else {
                    self.type_check_generic(ctx)?;
                    todo!("Type::type_check_generic")
                }
            }
            Auto => ast2::Type::Auto,
        })
    }
}

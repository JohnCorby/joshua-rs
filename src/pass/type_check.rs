use crate::context::Ctx;
use crate::error::{err, Res};
use crate::pass::ast::*;
use crate::pass::ty::{PrimitiveType, Type};
use crate::scope::Symbol;
use crate::util::ctx_str::IntoCtx;
use crate::util::IterExt;
use std::collections::HashMap;
use std::ops::Deref;

pub trait TypeCheck<'i> {
    /// type init, type checking, scope handling
    fn type_check(&self, ctx: &mut Ctx<'i>) -> Res<'i>;
}

impl TypeCheck<'i> for Program<'i> {
    fn type_check(&self, ctx: &mut Ctx<'i>) -> Res<'i> {
        ctx.scopes.push(None, false, None);
        ctx.type_check_prelude();
        for define in &*self.0 {
            define.type_check(ctx)?
        }
        ctx.scopes.pop();
        Ok(())
    }
}

impl TypeCheck<'i> for Define<'i> {
    fn type_check(&self, ctx: &mut Ctx<'i>) -> Res<'i> {
        use DefineKind::*;
        match &self.kind {
            Struct {
                nesting_prefix,
                name,
                generic_placeholders,
                body,
            } => {
                if generic_placeholders.is_empty() {
                    ctx.scopes.push(Some(*name), false, None);
                    for define in &**body {
                        if matches!(define.kind, Var(_)) {
                            define.type_check(ctx)?
                        }
                    }

                    // add symbol
                    let scope = ctx.scopes.0.pop().unwrap();
                    nesting_prefix.init(ctx.scopes.nesting_prefix().into());
                    ctx.scopes.add(
                        Symbol::StructType {
                            nesting_prefix: nesting_prefix.deref().clone(),
                            name: *name,
                            generic_replacements: Default::default(),
                            field_types: body
                                .iter()
                                .filter_map(|define| {
                                    if let Var(VarDefine { name, ty_node, .. }) = &define.kind {
                                        Some((*name, ty_node.ty.deref().clone()))
                                    } else {
                                        None
                                    }
                                })
                                .collect::<HashMap<_, _>>()
                                .into(),
                        },
                        Some(self.span),
                    )?;
                    ctx.scopes.0.push(scope);

                    for define in &**body {
                        if matches!(define.kind, Func { .. }) {
                            define.type_check(ctx)?
                        }
                    }
                    ctx.scopes.pop();
                } else {
                    todo!("generic struct define type check")
                }
            }
            Func {
                ty_node,
                nesting_prefix,
                name,
                generic_placeholders,
                args,
                body,
            } => {
                if generic_placeholders.is_empty() {
                    ty_node.type_check(ctx)?;
                    ctx.scopes
                        .push(Some(*name), false, Some(ty_node.ty.deref().clone()));
                    for arg in &**args {
                        arg.type_check(ctx)?
                    }

                    // add symbol
                    let scope = ctx.scopes.0.pop().unwrap();
                    nesting_prefix.init(ctx.scopes.nesting_prefix().into());
                    ctx.scopes.add(
                        Symbol::Func {
                            ty: ty_node.ty.deref().clone(),
                            nesting_prefix: nesting_prefix.deref().clone(),
                            name: *name,
                            generic_replacements: Default::default(),
                            arg_types: args
                                .iter()
                                .map(|it| it.ty_node.ty.deref().clone())
                                .vec()
                                .into(),
                        },
                        Some(self.span),
                    )?;
                    ctx.scopes.0.push(scope);

                    body.type_check(ctx)?;
                    ctx.scopes.check_return_called(Some(self.span))?;
                    ctx.scopes.pop();
                } else {
                    self.type_check_generic(ctx)?;
                }
            }
            Var(var_define) => var_define.type_check(ctx)?,
            CCode(c_code) => c_code.type_check(ctx)?,
        }
        Ok(())
    }
}

impl TypeCheck<'i> for VarDefine<'i> {
    fn type_check(&self, ctx: &mut Ctx<'i>) -> Res<'i> {
        // special case for auto type, where lvalue is inferred from rvalue instead of the other way around
        if let TypeKind::Auto = self.ty_node.kind {
            if let Some(value) = &self.value {
                value.type_check(ctx, None)?;
                self.ty_node.ty.init(value.ty.deref().clone());

                // check matching
                value.ty.check(&self.ty_node.ty, Some(self.span))?;
            } else {
                return err("cannot infer type", Some(self.span));
            }
        } else {
            self.ty_node.type_check(ctx)?;
            if let Some(value) = &self.value {
                value.type_check(ctx, Some(&self.ty_node.ty))?;

                // check matching
                value.ty.check(&self.ty_node.ty, Some(self.span))?;
            }
        }

        // add symbol
        ctx.scopes.add(
            Symbol::Var {
                ty: self.ty_node.ty.deref().clone(),
                name: self.name,
            },
            Some(self.span),
        )?;

        Ok(())
    }
}

impl TypeCheck<'i> for Statement<'i> {
    fn type_check(&self, ctx: &mut Ctx<'i>) -> Res<'i> {
        use StatementKind::*;
        match &self.kind {
            Return(value) => {
                if let Some(value) = value {
                    value.type_check(ctx, Some(&ctx.scopes.func_return_type().clone()))?
                }

                // check return type
                ctx.scopes.return_called();
                value
                    .as_ref()
                    .map(|it| &*it.ty)
                    .unwrap_or(&Type::Primitive(PrimitiveType::Void))
                    .check(ctx.scopes.func_return_type(), Some(self.span))?;
            }
            Break => {
                if !ctx.scopes.in_loop() {
                    return err("break can't be used outside of loops", Some(self.span));
                }
            }
            Continue => {
                if !ctx.scopes.in_loop() {
                    return err("continue can't be used outside of loops", Some(self.span));
                }
            }
            If {
                cond,
                then,
                otherwise,
            } => {
                cond.type_check(ctx, Some(&PrimitiveType::Bool.ty()))?;
                ctx.scopes.push(None, false, None);
                then.type_check(ctx)?;
                ctx.scopes.pop();
                if let Some(otherwise) = otherwise {
                    ctx.scopes.push(None, false, None);
                    otherwise.type_check(ctx)?;
                    ctx.scopes.pop();
                }

                // check condition
                cond.ty.check(&PrimitiveType::Bool.ty(), Some(self.span))?;
            }
            Until { cond, block } => {
                cond.type_check(ctx, Some(&PrimitiveType::Bool.ty()))?;
                ctx.scopes.push(None, true, None);
                block.type_check(ctx)?;
                ctx.scopes.pop();

                // check condition
                cond.ty.check(&PrimitiveType::Bool.ty(), Some(self.span))?;
            }
            For {
                init,
                cond,
                update,
                block,
            } => {
                ctx.scopes.push(None, true, None);
                init.type_check(ctx)?;
                cond.type_check(ctx, Some(&PrimitiveType::Bool.ty()))?;
                update.type_check(ctx)?;
                block.type_check(ctx)?;
                ctx.scopes.pop();

                // check condition
                cond.ty.check(&PrimitiveType::Bool.ty(), Some(self.span))?;
            }
            ExprAssign { lvalue, rvalue } => {
                lvalue.type_check(ctx, None)?;
                rvalue.type_check(ctx, Some(&lvalue.ty))?;

                // check matching
                lvalue.check_assignable(Some(self.span))?;
                rvalue.ty.check(&lvalue.ty, Some(self.span))?;
            }
            Define(define) => define.type_check(ctx)?,
            Expr(expr) => expr.type_check(ctx, None)?,
        }
        Ok(())
    }
}

impl TypeCheck<'i> for Block<'i> {
    fn type_check(&self, ctx: &mut Ctx<'i>) -> Res<'i> {
        for statement in &*self.0 {
            statement.type_check(ctx)?
        }
        Ok(())
    }
}

impl TypeCheck<'i> for CCode<'i> {
    fn type_check(&self, ctx: &mut Ctx<'i>) -> Res<'i> {
        for part in &*self.0 {
            match part {
                CCodePart::String(_) => {}
                CCodePart::Expr(expr) => expr.type_check(ctx, None)?,
            }
        }
        Ok(())
    }
}

impl Expr<'i> {
    pub fn type_check(&self, ctx: &mut Ctx<'i>, type_hint: Option<&Type<'i>>) -> Res<'i> {
        use ExprKind::*;
        self.ty.init(match &self.kind {
            Cast {
                nesting_prefix,
                thing,
                ty_node,
            } => {
                ty_node.type_check(ctx)?;
                thing.type_check(ctx, Some(&ty_node.ty))?;

                // symbol check
                // fixme hacky as shit
                if matches!(*thing.ty, Type::Literal(_) | Type::CCode) {
                    // casting will always work
                    ty_node.ty.deref().clone()
                } else {
                    let name = format!("as {}", ty_node.ty.encoded_name()).into_ctx(ctx);
                    let symbol = ctx.scopes.find(
                        &Symbol::new_func(
                            name,
                            Default::default(),
                            vec![thing.ty.deref().clone()].into(),
                        ),
                        Some(self.span),
                    )?;
                    match symbol {
                        Symbol::Func {
                            nesting_prefix: other_nesting_prefix,
                            ..
                        } => nesting_prefix.init(other_nesting_prefix.clone()),
                        _ => unreachable!(),
                    }
                    symbol.ty()
                }
            }
            MethodCall {
                receiver,
                func_call,
            } => {
                todo!("type check method call")
            }
            Field { receiver, var } => {
                receiver.type_check(ctx, None)?;

                // field check
                let symbol = match &*receiver.ty {
                    Type::Struct {
                        name,
                        generic_replacements,
                    } => Symbol::new_struct_type(*name, generic_replacements.clone()),
                    Type::GenericPlaceholder(_) => {
                        self.ty.init(Type::GenericUnknown);
                        return Ok(());
                    }
                    ty => {
                        return err(
                            &format!("expected struct type, but got {}", ty),
                            Some(self.span),
                        )
                    }
                };
                let symbol = ctx.scopes.find(&symbol, Some(self.span))?;
                let field_types = match &symbol {
                    Symbol::StructType { field_types, .. } => field_types,
                    _ => unreachable!(),
                };
                match field_types.get(&var) {
                    Some(field_type) => field_type.clone(),
                    None => {
                        return err(
                            &format!("no field named {} in {}", var, symbol),
                            Some(self.span),
                        )
                    }
                }
            }
            Literal(literal) => literal.ty(),
            FuncCall(func_call) => {
                func_call.type_check(ctx)?;
                func_call.ty.deref().clone()
            }
            Var(name) => {
                // symbol check
                ctx.scopes
                    .find(&Symbol::new_var(*name), Some(self.span))?
                    .ty()
            }
            CCode(c_code) => {
                c_code.type_check(ctx)?;
                Type::CCode
            }
        });
        Ok(())
    }
}

impl TypeCheck<'i> for FuncCall<'i> {
    fn type_check(&self, ctx: &mut Ctx<'i>) -> Res<'i> {
        if self.generic_replacements.is_empty() {
            for replacement in &*self.generic_replacements {
                replacement.type_check(ctx)?
            }
            for arg in &*self.args {
                arg.type_check(ctx, None)?;
                // very lol
                if matches!(*arg.ty, Type::GenericUnknown | Type::GenericPlaceholder(_)) {
                    self.ty.init(Type::GenericUnknown);
                    return Ok(());
                }
            }

            // symbol check
            let symbol = ctx.scopes.find(
                &Symbol::new_func(
                    self.name,
                    Default::default(),
                    self.args
                        .iter()
                        .map(|it| it.ty.deref().clone())
                        .vec()
                        .into(),
                ),
                Some(self.span),
            )?;
            match symbol {
                Symbol::Func { nesting_prefix, .. } => {
                    self.nesting_prefix.init(nesting_prefix.clone())
                }
                _ => unreachable!(),
            };
            self.ty.init(symbol.ty());
            Ok(())
        } else {
            self.type_check_generic(ctx)
        }
    }
}

impl TypeCheck<'i> for TypeNode<'i> {
    fn type_check(&self, ctx: &mut Ctx<'i>) -> Res<'i> {
        use TypeKind::*;
        self.ty.init(match &self.kind {
            Primitive(ty) => Type::Primitive(*ty),
            Ptr(inner) => {
                inner.type_check(ctx)?;
                Type::Ptr(inner.ty.deref().clone().into())
            }
            Named {
                name,
                generic_replacements,
            } => {
                // symbol check
                ctx.scopes
                    .find(
                        &Symbol::new_struct_type(
                            *name,
                            Default::default(), // todo generic find or add
                        ),
                        Some(self.span),
                    )
                    .or_else(|_| {
                        ctx.scopes.find(
                            &Symbol::new_generic_placeholder_type(*name),
                            Some(self.span),
                        )
                    })
                    .or_else(|_| {
                        err(
                            &format!("could not find type symbol `{}`", name),
                            Some(self.span),
                        )
                    })?
                    .ty()
            }
            Auto => Type::Auto,
        });
        Ok(())
    }
}

//! type init, type checking, scope handling
//! todo impl

use crate::context::Ctx;
use crate::error::{err, Res};
use crate::pass::define::{Define, DefineKind, Program, VarDefine};
use crate::pass::expr::{Expr, ExprKind, FuncCall};
use crate::pass::statement::{Block, CCode, CCodePart, Statement, StatementKind};
use crate::pass::ty::{PrimitiveType, Type, TypeKind, TypeNode};
use crate::scope::Symbol;
use crate::util::interned_str::Intern;
use std::collections::HashMap;
use std::ops::Deref;
use std::rc::Rc;

impl<'i> Program<'i> {
    pub fn type_check(&self, ctx: &mut Ctx<'i>) -> Res<'i, ()> {
        for define in &self.0 {
            define.type_check(ctx)?
        }
        Ok(())
    }
}

impl<'i> Define<'i> {
    pub fn type_check(&self, ctx: &mut Ctx<'i>) -> Res<'i, ()> {
        use DefineKind::*;
        match &self.kind {
            Struct { name, body } => {
                // not really a scope, but makes it so these symbols don't show up
                ctx.scopes.push(false, None);
                for define in body {
                    define.type_check(ctx)?
                }
                ctx.scopes.pop();

                // add symbol
                ctx.scopes.add(
                    Symbol::StructType {
                        name: *name,
                        field_types: {
                            let mut field_types = HashMap::new();
                            for define in body {
                                if let Var(VarDefine { name, ty_node, .. }) = &define.kind {
                                    field_types
                                        .insert(*name, ty_node.ty.deref().clone())
                                        .unwrap_none();
                                }
                            }
                            field_types
                        },
                    },
                    Some(self.span),
                )?;
            }
            Func {
                ty_node,
                name,
                generic_placeholders,
                args,
                body,
            } => {
                ty_node.type_check(ctx)?;
                ctx.scopes.push(false, Some(ty_node.ty.deref().clone()));
                for placeholder in generic_placeholders {
                    ctx.scopes.add(
                        Symbol::GenericPlaceholderType(*placeholder),
                        Some(self.span),
                    )?
                }
                for arg in args {
                    arg.type_check(ctx)?
                }
                body.type_check(ctx)?;
                ctx.scopes.check_return_called(Some(self.span))?;
                ctx.scopes.pop();

                // add symbol
                ctx.scopes.add(
                    if generic_placeholders.is_empty() {
                        Symbol::Func {
                            ty: ty_node.ty.deref().clone(),
                            name: *name,
                            arg_types: args
                                .iter()
                                .map(|it| it.ty_node.ty.deref().clone())
                                .collect(),
                        }
                    } else {
                        Symbol::GenericFunc {
                            ty: ty_node.ty.deref().clone(),
                            arg_types: args
                                .iter()
                                .map(|it| it.ty_node.ty.deref().clone())
                                .collect(),

                            span: self.span,
                            ty_node: ty_node.clone(),
                            name: *name,
                            generic_placeholders: generic_placeholders.clone(),
                            args: args.clone(),
                            body: body.clone().into(),

                            scopes_index: ctx.scopes.0.len(),
                            o_index: ctx.o.make_index(ctx.o.len()),
                        }
                    },
                    Some(self.span),
                )?;
            }
            Var(var_define) => var_define.type_check(ctx)?,
            CCode(c_code) => c_code.type_check(ctx)?,
        }
        Ok(())
    }
}

impl<'i> VarDefine<'i> {
    pub fn type_check(&self, ctx: &mut Ctx<'i>) -> Res<'i, ()> {
        self.ty_node.type_check(ctx)?;
        if let Some(value) = &self.value {
            value.type_check(ctx)?;

            // check matching
            value.ty.check(self.ty_node.ty.deref(), Some(self.span))?;
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

impl<'i> Statement<'i> {
    pub fn type_check(&self, ctx: &mut Ctx<'i>) -> Res<'i, ()> {
        use StatementKind::*;
        match &self.kind {
            Return(value) => {
                if let Some(value) = value {
                    value.type_check(ctx)?
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
                cond.type_check(ctx)?;
                ctx.scopes.push(false, None);
                then.type_check(ctx)?;
                ctx.scopes.pop();
                if let Some(otherwise) = otherwise {
                    ctx.scopes.push(false, None);
                    otherwise.type_check(ctx)?;
                    ctx.scopes.pop();
                }

                // check condition
                cond.ty.check(&PrimitiveType::Bool.ty(), Some(self.span))?;
            }
            Until { cond, block } => {
                cond.type_check(ctx)?;
                ctx.scopes.push(true, None);
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
                ctx.scopes.push(true, None);
                init.type_check(ctx)?;
                cond.type_check(ctx)?;
                update.type_check(ctx)?;
                block.type_check(ctx)?;
                ctx.scopes.pop();

                // check condition
                cond.ty.check(&PrimitiveType::Bool.ty(), Some(self.span))?;
            }
            ExprAssign { lvalue, rvalue } => {
                lvalue.type_check(ctx)?;
                rvalue.type_check(ctx)?;

                // check matching
                lvalue.check_assignable(Some(self.span))?;
                rvalue.ty.check(&lvalue.ty, Some(self.span))?;
            }
            VarDefine(var_define) => var_define.type_check(ctx)?,
            Expr(expr) => expr.type_check(ctx)?,
        }
        Ok(())
    }
}

impl<'i> Block<'i> {
    pub fn type_check(&self, ctx: &mut Ctx<'i>) -> Res<'i, ()> {
        for statement in &self.0 {
            statement.type_check(ctx)?
        }
        Ok(())
    }
}

impl<'i> CCode<'i> {
    pub fn type_check(&self, ctx: &mut Ctx<'i>) -> Res<'i, ()> {
        for part in &self.0 {
            match part {
                CCodePart::String(_) => {}
                CCodePart::Expr(expr) => {
                    expr.type_check(ctx)?;
                }
            }
        }
        Ok(())
    }
}

impl<'i> Expr<'i> {
    pub fn type_check(&self, ctx: &mut Ctx<'i>) -> Res<'i, ()> {
        use ExprKind::*;
        self.ty.init(match &self.kind {
            Binary { left, op, right } => {
                left.type_check(ctx)?;
                right.type_check(ctx)?;

                // symbol check
                ctx.scopes
                    .find(
                        &Symbol::new_func(
                            *op,
                            [left.ty.deref().clone(), right.ty.deref().clone()].into(),
                        ),
                        Some(self.span),
                    )?
                    .ty()
            }
            Unary { op, thing } => {
                thing.type_check(ctx)?;

                // symbol check
                ctx.scopes
                    .find(
                        &Symbol::new_func(*op, [thing.ty.deref().clone()].into()),
                        Some(self.span),
                    )?
                    .ty()
            }
            Cast { thing, ty_node } => {
                thing.type_check(ctx)?;
                ty_node.type_check(ctx)?;

                // symbol check
                // fixme literal casting is hacky as shit
                if let Type::Literal(_) = *thing.ty {
                    // casting will always work for literals
                    ty_node.ty.deref().clone()
                } else {
                    let name = format!("as {}", ty_node.ty.name()).intern(ctx);
                    ctx.scopes
                        .find(
                            &Symbol::new_func(name, [thing.ty.deref().clone()].into()),
                            Some(self.span),
                        )?
                        .ty()
                }
            }
            MethodCall {
                receiver,
                func_call,
            } => {
                receiver.type_check(ctx)?;

                // desugar into func call and type init
                // kinda hacky, but should work
                let mut real_func_call = func_call.clone();
                real_func_call.args.insert(0, *receiver.clone());
                real_func_call.type_check(ctx)?;
                real_func_call.ty.deref().clone()
            }
            Field { receiver, var } => {
                receiver.type_check(ctx)?;

                // field check
                let struct_name = *match receiver.ty.deref() {
                    Type::Struct(struct_name) => struct_name,
                    ty => {
                        return err(
                            &format!("expected struct type, but got {}", ty),
                            Some(self.span),
                        )
                    }
                };
                let symbol = ctx
                    .scopes
                    .find(&Symbol::new_struct_type(struct_name), Some(self.span))?;
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
                c_code.ty()
            }
        });
        Ok(())
    }
}

impl<'i> FuncCall<'i> {
    pub fn type_check(&self, ctx: &mut Ctx<'i>) -> Res<'i, ()> {
        for replacement in &self.generic_replacements {
            replacement.type_check(ctx)?
        }
        for arg in &self.args {
            arg.type_check(ctx)?
        }

        // todo generics
        self.ty.init(
            // symbol check
            ctx.scopes
                .find(
                    &Symbol::new_func(
                        self.name,
                        self.args.iter().map(|it| it.ty.deref().clone()).collect(),
                    ),
                    Some(self.span),
                )?
                .ty(),
        );

        Ok(())
    }
}

impl<'i> TypeNode<'i> {
    pub fn type_check(&self, ctx: &Ctx<'i>) -> Res<'i, ()> {
        use TypeKind::*;
        self.ty.init(match &self.kind {
            Primitive(ty) => Type::Primitive(*ty),
            Ptr(ty) => {
                ty.type_check(ctx)?;
                Type::Ptr(ty.deref().clone().ty.deref().clone().into())
            }
            Named(name) => {
                // symbol check
                ctx.scopes
                    .find(&Symbol::new_struct_type(*name), Some(self.span))
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
        });
        Ok(())
    }
}

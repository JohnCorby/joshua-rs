//! helper stuff for generic handling

use crate::context::Ctx;
use crate::error::{err, Res};
use crate::pass::ast1::*;
use crate::pass::ast2;
use crate::pass::replace_generics::GenericMap;
use crate::pass::scope::{Scope, Scopes, Symbol};
use crate::span::Span;
use crate::util::ctx_str::{CtxStr, IntoCtx};
use crate::util::{IterExt, IterResExt, StrExt};
use std::collections::HashMap;
use std::rc::Rc;

impl Define<'i> {
    /// `Define::type_check` but for generic funcs and structs
    pub fn type_check_generic(self, ctx: &mut Ctx<'i>) -> Res<'i, ast2::Define<'i>> {
        use DefineKind::*;
        match self.kind {
            Func {
                ty,
                name,
                generic_placeholders,
                args,
                body,
            } => {
                debug_assert!(!generic_placeholders.is_empty());

                ctx.scopes.push(Scope::new(None, false, None));
                // add placeholders
                for &placeholder in &*generic_placeholders {
                    ctx.scopes
                        .add(Symbol::GenericPlaceholderType(placeholder), Some(self.span))?;
                }
                let arg_types = args
                    .iter()
                    .cloned()
                    .map(|arg| arg.type_check(ctx).map(|arg| arg.ty))
                    .res_vec()?
                    .into();
                ctx.scopes.pop();

                // add symbol
                ctx.scopes.add(
                    Symbol::GenericFunc {
                        arg_types,

                        ty,
                        name,
                        generic_placeholders,
                        args,
                        body,

                        scopes_index: ctx.scopes.0.len(),
                    },
                    Some(self.span),
                )?;
            }

            Struct {
                name,
                generic_placeholders,
                body,
            } => {
                debug_assert!(!generic_placeholders.is_empty());

                // add symbol
                ctx.scopes.add(
                    Symbol::GenericStruct {
                        name,
                        generic_placeholders,
                        body,

                        scopes_index: ctx.scopes.0.len(),
                    },
                    Some(self.span),
                )?;
            }

            _ => unreachable!(),
        }
        Ok(ast2::Define::NoGen)
    }
}

impl FuncCall<'i> {
    /// `FuncCall::type_check` but for generic funcs
    pub fn type_check_generic(self, ctx: &mut Ctx<'i>) -> Res<'i, ast2::FuncCall<'i>> {
        debug_assert!(!self.generic_replacements.is_empty());

        let generic_replacements: Rc<Vec<ast2::Type<'i>>> = self
            .generic_replacements
            .iter()
            .cloned()
            .map(|replacement| replacement.type_check(ctx))
            .res_vec()?
            .into();
        let args = self
            .args
            .iter()
            .cloned()
            .map(|arg| arg.type_check(ctx, None))
            .res_vec()?;

        // find an associated generic func
        let generic_symbol = ctx.scopes.find_generic_func(
            self.name,
            &generic_replacements.iter().vec(),
            &args.iter().map(|arg| &arg.ty).vec(),
            Some(self.span),
        )?;
        if let Symbol::GenericFunc {
            mut ty,

            name,
            generic_placeholders,
            args: symbol_args_,
            mut body,

            scopes_index,
            ..
        } = generic_symbol.clone()
        {
            // go to where the generic func was defined
            let scopes_after = ctx.scopes.0.split_off(scopes_index);

            // get mapping from placeholder names to replacement types
            let generic_map = generic_placeholders
                .iter()
                .copied()
                .zip(self.generic_replacements.iter().cloned())
                .collect::<GenericMap<'i>>();

            // a decent chunk of this is just duplicated code from Define::type_check
            // that's slightly modified to replace_generics and to not always Scopes::add
            // oh well

            ty.replace_generics(ctx, &generic_map);
            let ty = ty.type_check(ctx)?;
            let nesting_prefix = ctx.scopes.nesting_prefix().into_ctx(ctx);
            ctx.scopes
                .push(Scope::new(Some(name), false, Some(ty.clone())));
            let mut symbol_args = Vec::with_capacity(symbol_args_.len());
            for (mut symbol_arg, (arg, ast1_arg)) in symbol_args_
                .iter()
                .cloned()
                .zip(args.iter().zip(self.args.iter()))
            {
                symbol_arg.replace_generics(ctx, &generic_map);
                let symbol_arg = symbol_arg.type_check(ctx)?;

                // make sure the args actually match
                arg.ty.check(&symbol_arg.ty, Some(ast1_arg.span))?;

                symbol_args.push(symbol_arg);
            }

            // add symbol if non-existent
            let specialized_symbol = Symbol::Func {
                ty: ty.clone(),
                nesting_prefix,
                name,
                generic_replacements: generic_replacements.clone(),
                arg_types: args.iter().cloned().map(|it| it.ty).vec().into(),
            };
            if ctx
                .scopes
                .find(&specialized_symbol, Some(self.span))
                .is_err()
            {
                // add symbol
                let scope = ctx.scopes.pop();
                ctx.scopes.add(specialized_symbol, Some(self.span))?;
                ctx.scopes.push(scope);

                body.replace_generics(ctx, &generic_map);
                let body = body.type_check(ctx)?;
                ctx.scopes.check_return_called(Some(self.span))?;

                // push a define ast so it will be generated properly
                let define = ast2::Define::Func {
                    ty: ty.clone(),
                    full_name: format!("{}{}", nesting_prefix, name).into_ctx(ctx),
                    generic_replacements: generic_replacements.clone(),
                    args: symbol_args.into(),
                    body,
                };
                ctx.extra_defines.push(define);
            } else {
                unreachable!()
            }

            ctx.scopes.pop();

            ctx.scopes.0.extend(scopes_after);
            Ok(ast2::FuncCall {
                full_name: format!("{}{}", nesting_prefix, name).into_ctx(ctx),
                generic_replacements,
                args: args.into(),
                ty,
            })
        } else {
            unreachable!()
        }
    }
}

impl Type<'i> {
    /// `Type::type_check` but for generic structs
    pub fn type_check_generic(self, ctx: &mut Ctx<'i>) -> Res<'i, ast2::Type<'i>> {
        use DefineKind::*;
        use TypeKind::*;
        if let Named {
            name,
            generic_replacements: generic_replacements_,
        } = self.kind
        {
            debug_assert!(generic_replacements_.is_empty());

            let generic_replacements: Rc<Vec<ast2::Type<'i>>> = generic_replacements_
                .iter()
                .cloned()
                .map(|replacement| replacement.type_check(ctx))
                .res_vec()?
                .into();

            // find an associated generic func
            let generic_symbol = ctx.scopes.find_generic_struct(
                name,
                &generic_replacements.iter().vec(),
                Some(self.span),
            )?;
            if let Symbol::GenericStruct {
                name,
                generic_placeholders,
                body,

                scopes_index,
                ..
            } = generic_symbol.clone()
            {
                // go to where the generic func was defined
                let scopes_after = ctx.scopes.0.split_off(scopes_index);

                // get mapping from placeholder names to replacement types
                let generic_map = generic_placeholders
                    .iter()
                    .copied()
                    .zip(generic_replacements_.iter().cloned())
                    .collect::<GenericMap<'i>>();

                // a decent chunk of this is just duplicated code from Define::type_check
                // that's slightly modified to replace_generics and to not always Scopes::add
                // oh well

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
                    .map(|mut define| {
                        define.replace_generics(ctx, &generic_map);
                        define.type_check(ctx)
                    })
                    .res_vec()?;
                ctx.scopes.pop();

                // add symbol if non-existent
                let nesting_prefix = ctx.scopes.nesting_prefix().into_ctx(ctx);
                let specialized_symbol = Symbol::StructType {
                    nesting_prefix,
                    name,
                    generic_replacements: generic_replacements.clone(),
                    field_types: var_defines
                        .iter()
                        .cloned()
                        .map(|define| match define {
                            ast2::Define::Var(ast2::VarDefine { name, ty, .. }) => (name, ty),
                            _ => unreachable!(),
                        })
                        .collect::<HashMap<_, _>>()
                        .into(),
                };
                if ctx
                    .scopes
                    .find(&specialized_symbol, Some(self.span))
                    .is_err()
                {
                    // add symbol
                    ctx.scopes.add(specialized_symbol, Some(self.span))?;

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

                            define.replace_generics(ctx, &generic_map);
                            define.type_check(ctx)
                        })
                        .res_vec()?;

                    let mut body = var_defines;
                    body.extend(func_defines);

                    // push a define ast so it will be generated properly
                    let define = ast2::Define::Struct {
                        full_name: format!("{}{}", nesting_prefix, name).into_ctx(ctx),
                        generic_replacements: generic_replacements.clone(),
                        body: body.into(),
                    };
                    ctx.extra_defines.push(define);
                } else {
                    unreachable!()
                }

                ctx.scopes.0.extend(scopes_after);
                Ok(ast2::Type::Struct {
                    nesting_prefix,
                    name,
                    generic_replacements,
                })
            } else {
                unreachable!()
            }
        } else {
            unreachable!()
        }
    }
}

impl Scopes<'i> {
    /// find a generic func fuzzily
    pub fn find_generic_func(
        &self,
        name: CtxStr<'i>,
        generic_replacements: &[&ast2::Type<'i>],
        arg_types: &[&ast2::Type<'i>],
        span: Option<Span<'i>>,
    ) -> Res<'i, &Symbol<'i>> {
        for scope in self.0.iter().rev() {
            let symbol = scope.symbols.iter().find(|&s| {
                if let Symbol::GenericFunc {
                    name: symbol_name,
                    generic_placeholders,
                    arg_types: symbol_arg_types,
                    ..
                } = s
                {
                    if name != *symbol_name {
                        return false;
                    }

                    if generic_placeholders.len() != generic_replacements.len() {
                        return false;
                    }

                    if arg_types.len() != symbol_arg_types.len() {
                        return false;
                    }
                    for (&ty, symbol_ty) in arg_types.iter().zip(symbol_arg_types.iter()) {
                        if let ast2::Type::GenericPlaceholder(_) = symbol_ty {
                            // generic symbol_ty will always match ty, so don't return false
                        } else if ty != symbol_ty {
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
                "could not find {}",
                name.to_display("generic func symbol", generic_replacements, Some(arg_types))
            ),
            span,
        )
    }

    /// find a generic struct fuzzily
    pub fn find_generic_struct(
        &self,
        name: CtxStr<'i>,
        generic_replacements: &[&ast2::Type<'i>],
        span: Option<Span<'i>>,
    ) -> Res<'i, &Symbol<'i>> {
        for scope in self.0.iter().rev() {
            let symbol = scope.symbols.iter().find(|&s| {
                if let Symbol::GenericStruct {
                    name: symbol_name,
                    generic_placeholders,
                    ..
                } = s
                {
                    if name != *symbol_name {
                        return false;
                    }

                    if generic_placeholders.len() != generic_replacements.len() {
                        return false;
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
                "could not find {}",
                name.to_display("generic struct symbol", generic_replacements, None)
            ),
            span,
        )
    }
}

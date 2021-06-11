//! helper stuff for generic handling

use crate::context::Ctx;
use crate::error::{err, Res};
use crate::pass::ast::*;
use crate::pass::ty::Type;
use crate::pass::type_check::TypeCheck;
use crate::scope::{Scopes, Symbol};
use crate::span::Span;
use crate::util::ctx_str::{CtxStr, IntoCtx};
use crate::util::{IterExt, StrExt};
use std::collections::HashMap;
use std::ops::Deref;

impl Define<'i> {
    pub fn type_check_generic(&self, ctx: &mut Ctx<'i>) -> Res<'i> {
        match &self.kind {
            DefineKind::Func {
                ty_node,
                nesting_prefix,
                name,
                generic_placeholders,
                args,
                body,
            } => {
                debug_assert!(!generic_placeholders.is_empty());

                ctx.scopes.push(None, false, None);
                // add placeholders
                for &placeholder in &**generic_placeholders {
                    ctx.scopes
                        .add(Symbol::GenericPlaceholderType(placeholder), Some(self.span))?;
                }

                ty_node.type_check(ctx)?;
                ctx.scopes
                    .push(Some(*name), false, Some(ty_node.ty.deref().clone()));
                for arg in &**args {
                    arg.type_check(ctx)?
                }

                body.type_check(ctx)?;
                ctx.scopes.check_return_called(Some(self.span))?;
                ctx.scopes.pop();
                ctx.scopes.pop();

                nesting_prefix.init(ctx.scopes.nesting_prefix().into());

                // add symbol
                ctx.scopes.add(
                    Symbol::GenericFunc {
                        ty: ty_node.ty.deref().clone(),
                        arg_types: args
                            .iter()
                            .map(|it| it.ty_node.ty.deref().clone())
                            .vec()
                            .into(),

                        span: self.span,
                        ty_node: ty_node.clone(),
                        nesting_prefix: nesting_prefix.deref().clone(),
                        name: *name,
                        generic_placeholders: generic_placeholders.clone(),
                        args: args.clone(),
                        body: body.clone(),

                        scopes_index: ctx.scopes.0.len(),
                    },
                    Some(self.span),
                )
            }
            _ => unreachable!(),
        }
    }
}

type GenericMap<'i> = HashMap<CtxStr<'i>, TypeNode<'i>>;

impl FuncCall<'i> {
    pub fn type_check_generic(&self, ctx: &mut Ctx<'i>) -> Res<'i> {
        debug_assert!(!self.generic_replacements.is_empty());

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

        // find an associated generic func
        let generic_symbol = ctx
            .scopes
            .find_generic_func(
                self.name,
                &self.generic_replacements.iter().map(|it| &*it.ty).vec(),
                &self.args.iter().map(|it| &*it.ty).vec(),
                Some(self.span),
            )?
            .clone();
        if let Symbol::GenericFunc {
            span,
            mut ty_node,
            nesting_prefix,
            name,
            generic_placeholders,
            args,
            mut body,

            scopes_index,
            ..
        } = generic_symbol
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
            ty_node.replace_generics(&generic_map);
            ty_node.type_check(ctx)?;
            ctx.scopes
                .push(Some(name), false, Some(ty_node.ty.deref().clone()));
            let mut args = args.deref().clone();
            for (arg, call_arg) in args.iter_mut().zip(&*self.args) {
                arg.replace_generics(&generic_map);
                arg.type_check(ctx)?;

                // make sure the args actually match
                call_arg.ty.check(&*arg.ty_node.ty, Some(call_arg.span))?;
            }

            // add symbol if non-existent
            let specialized_symbol = Symbol::Func {
                ty: ty_node.ty.deref().clone(),
                nesting_prefix: nesting_prefix.clone(),
                name,
                generic_replacements: self
                    .generic_replacements
                    .iter()
                    .map(|it| it.ty.deref().clone())
                    .vec()
                    .into(),
                arg_types: args
                    .iter()
                    .map(|it| it.ty_node.ty.deref().clone())
                    .vec()
                    .into(),
            };
            if ctx
                .scopes
                .find(&specialized_symbol, Some(self.span))
                .is_err()
            {
                // add symbol
                let scope = ctx.scopes.0.pop().unwrap();
                ctx.scopes.add(specialized_symbol, Some(self.span))?;
                ctx.scopes.0.push(scope);

                body.replace_generics(&generic_map);
                body.type_check(ctx)?;
                ctx.scopes.check_return_called(Some(self.span))?;

                // push a define ast so it will be generated properly
                let define = Define {
                    span,
                    kind: DefineKind::Func {
                        ty_node: ty_node.clone(),
                        nesting_prefix: nesting_prefix.clone().into(),
                        name: name
                            .encode(
                                &[],
                                &self.generic_replacements.iter().map(|it| &*it.ty).vec(),
                                None,
                            )
                            .into_ctx(ctx),
                        generic_placeholders: Default::default(),
                        args: args.into(),
                        body,
                    },
                };
                ctx.extra_defines.push(define);
            }

            ctx.scopes.pop();
            self.nesting_prefix.init(nesting_prefix);
            self.ty.init(ty_node.ty.deref().clone());

            ctx.scopes.0.extend(scopes_after);
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
        name: CtxStr<'i>,
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
                    for (&ty, other_ty) in arg_types.iter().zip(&**other_arg_types) {
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
                "could not find {}",
                name.to_display("generic func symbol", generic_replacements, Some(arg_types))
            ),
            span,
        )
    }
}

trait ReplaceGenerics<'i> {
    /// replaces generic placeholders with real types
    ///
    /// since this is mutable, it actually clones stuff out of Rc, modifying it and then putting it back in new Rc
    /// therefore, it is the only pass to do real cloning, because it legit edits the ast
    ///
    /// note: this resets types, so you gotta type_check again
    fn replace_generics(&mut self, generic_map: &GenericMap<'i>);
}

impl ReplaceGenerics<'i> for Program<'i> {
    fn replace_generics(&mut self, generic_map: &GenericMap<'i>) {
        let mut new_defines = self.0.deref().clone();
        for define in &mut new_defines {
            define.replace_generics(generic_map)
        }
        self.0 = new_defines.into()
    }
}

impl ReplaceGenerics<'i> for Define<'i> {
    fn replace_generics(&mut self, generic_map: &GenericMap<'i>) {
        use DefineKind::*;
        match &mut self.kind {
            Struct { body, .. } => {
                let mut new_body = body.deref().deref().clone();
                for define in &mut new_body {
                    define.replace_generics(generic_map)
                }
                *body = new_body.into();
            }
            Func {
                ty_node,
                generic_placeholders,
                args,
                body,
                ..
            } => {
                ty_node.replace_generics(generic_map);
                let mut new_args = args.deref().deref().clone();
                for arg in &mut new_args {
                    arg.replace_generics(generic_map)
                }
                *args = new_args.into();
                body.replace_generics(generic_map);

                *generic_placeholders = Default::default()
            }
            Var(var_define) => var_define.replace_generics(generic_map),
            CCode(c_code) => c_code.replace_generics(generic_map),
        }
    }
}

impl ReplaceGenerics<'i> for VarDefine<'i> {
    fn replace_generics(&mut self, generic_map: &GenericMap<'i>) {
        self.ty_node.replace_generics(generic_map);
        if let Some(value) = &mut self.value {
            value.replace_generics(generic_map)
        }
    }
}

impl ReplaceGenerics<'i> for Statement<'i> {
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
                let mut new_update = update.deref().deref().clone();
                new_update.replace_generics(generic_map);
                *update = new_update.into();
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

impl ReplaceGenerics<'i> for Block<'i> {
    fn replace_generics(&mut self, generic_map: &GenericMap<'i>) {
        let mut new_statements = self.0.deref().clone();
        for statement in &mut new_statements {
            statement.replace_generics(generic_map)
        }
        self.0 = new_statements.into()
    }
}

impl ReplaceGenerics<'i> for CCode<'i> {
    fn replace_generics(&mut self, generic_map: &GenericMap<'i>) {
        let mut new_parts = self.0.deref().clone();
        for part in &mut new_parts {
            match part {
                CCodePart::String(_) => {}
                CCodePart::Expr(expr) => expr.replace_generics(generic_map),
            }
        }
        self.0 = new_parts.into()
    }
}

impl ReplaceGenerics<'i> for Expr<'i> {
    fn replace_generics(&mut self, generic_map: &GenericMap<'i>) {
        use ExprKind::*;
        match &mut self.kind {
            Cast { thing, ty_node, .. } => {
                let mut new_thing = thing.deref().deref().clone();
                new_thing.replace_generics(generic_map);
                *thing = new_thing.into();
                ty_node.replace_generics(generic_map);
            }
            MethodCall {
                receiver,
                func_call,
            } => {
                todo!("replace generics method call")
            }
            Field { receiver, .. } => {
                let mut new_receiver = receiver.deref().deref().clone();
                new_receiver.replace_generics(generic_map);
                *receiver = new_receiver.into()
            }
            Literal(_) => {}
            FuncCall(func_call) => func_call.replace_generics(generic_map),
            Var(_) => {}
            CCode(c_code) => c_code.replace_generics(generic_map),
        }

        self.ty = Default::default();
    }
}

impl ReplaceGenerics<'i> for FuncCall<'i> {
    fn replace_generics(&mut self, generic_map: &GenericMap<'i>) {
        let mut new_replacements = self.generic_replacements.deref().clone();
        for replacement in &mut new_replacements {
            replacement.replace_generics(generic_map)
        }
        self.generic_replacements = new_replacements.into();
        let mut new_args = self.args.deref().clone();
        for arg in &mut new_args {
            arg.replace_generics(generic_map)
        }
        self.args = new_args.into();

        self.ty = Default::default();
    }
}

impl ReplaceGenerics<'i> for TypeNode<'i> {
    fn replace_generics(&mut self, generic_map: &GenericMap<'i>) {
        if let Type::GenericPlaceholder(name) = *self.ty {
            self.kind = generic_map[&name].kind.clone();
        }

        self.ty = Default::default();
    }
}

//! helper stuff for generic handling

use crate::context::Ctx;
use crate::error::{err, Res};
use crate::pass::ast::*;
use crate::pass::ty::Type;
use crate::pass::type_check::TypeCheck;
use crate::scope::{Scopes, Symbol};
use crate::span::Span;
use crate::util::interned_str::{Intern, InternedStr};
use crate::util::{code_name, to_string};
use std::collections::HashMap;
use std::ops::Deref;

impl Define<'i> {
    pub fn type_check_generic(&self, ctx: &mut Ctx<'i>) -> Res<'i> {
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

type GenericMap<'i> = HashMap<InternedStr<'i>, TypeNode<'i>>;

impl FuncCall<'i> {
    pub fn type_check_generic(&self, ctx: &mut Ctx<'i>) -> Res<'i> {
        debug_assert!(!self.generic_replacements.is_empty());

        for replacement in &self.generic_replacements {
            replacement.type_check(ctx)?
        }
        for arg in &self.args {
            arg.type_check(ctx, None)?;
            // very lol
            if matches!(*arg.ty, Type::GenericUnknown | Type::GenericPlaceholder(_)) {
                self.ty.init(Type::GenericUnknown);
                return Ok(());
            }
        }

        // find an associated generic func
        let symbol = ctx
            .scopes
            .find_generic_func(
                self.name,
                &self
                    .generic_replacements
                    .iter()
                    .map(|it| &*it.ty)
                    .collect::<Vec<_>>(),
                &self.args.iter().map(|it| &*it.ty).collect::<Vec<_>>(),
                Some(self.span),
            )?
            .clone();
        if let Symbol::GenericFunc {
            span,
            mut ty_node,
            name,
            mut generic_placeholders,
            mut args,
            body,

            scopes_index,
            ..
        } = symbol
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
            ctx.scopes.push(false, Some(ty_node.ty.deref().clone()));
            for (arg, call_arg) in args.iter_mut().zip(&self.args) {
                arg.replace_generics(&generic_map);
                arg.type_check(ctx)?;

                // make sure the args actually match
                call_arg.ty.check(&*arg.ty_node.ty, Some(call_arg.span))?;
            }
            generic_placeholders.clear();

            // add symbol if non-existent
            let symbol_name = code_name(
                &name,
                &self
                    .generic_replacements
                    .iter()
                    .map(|it| &*it.ty)
                    .collect::<Vec<_>>(),
                Some(&args.iter().map(|it| &*it.ty_node.ty).collect::<Vec<_>>()),
            )
            .intern(ctx);
            if ctx
                .scopes
                .find(&Symbol::new_func(symbol_name), Some(self.span))
                .is_err()
            {
                // add symbol
                let scope = ctx.scopes.0.pop().unwrap();
                ctx.scopes.add(
                    Symbol::Func {
                        ty: ty_node.ty.deref().clone(),
                        name: symbol_name,
                    },
                    Some(self.span),
                )?;
                ctx.scopes.0.push(scope);

                let mut body = body.deref().clone(); // this clones the Block, not Rc, oh well
                body.replace_generics(&generic_map);
                body.type_check(ctx)?;
                ctx.scopes.check_return_called(Some(self.span))?;

                // push a define ast so it will be generated properly
                let define_name = code_name(
                    &name,
                    &self
                        .generic_replacements
                        .iter()
                        .map(|it| &*it.ty)
                        .collect::<Vec<_>>(),
                    None,
                )
                .intern(ctx);
                ctx.extra_defines.push(Define {
                    span,
                    kind: DefineKind::Func {
                        ty_node: ty_node.clone(),
                        name: define_name,
                        generic_placeholders,
                        args,
                        body,
                    },
                });
            }

            ctx.scopes.pop();
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
    /// todo integrate this into Scopes::find so we can just use that
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
                    for (&ty, other_ty) in arg_types.iter().zip(other_arg_types) {
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
                to_string(
                    "generic func symbol",
                    &name,
                    generic_replacements,
                    Some(arg_types)
                )
            ),
            span,
        )
    }
}

trait ReplaceGenerics<'i> {
    /// replaces generic placeholders with real types
    /// note: this resets types, so you gotta type_check again
    fn replace_generics(&mut self, generic_map: &GenericMap<'i>);
}

impl ReplaceGenerics<'i> for Program<'i> {
    fn replace_generics(&mut self, generic_map: &GenericMap<'i>) {
        for define in &mut self.0 {
            define.replace_generics(generic_map)
        }
    }
}

impl ReplaceGenerics<'i> for Define<'i> {
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

impl ReplaceGenerics<'i> for Block<'i> {
    fn replace_generics(&mut self, generic_map: &GenericMap<'i>) {
        for statement in &mut self.0 {
            statement.replace_generics(generic_map)
        }
    }
}

impl ReplaceGenerics<'i> for CCode<'i> {
    fn replace_generics(&mut self, generic_map: &GenericMap<'i>) {
        for part in &mut self.0 {
            match part {
                CCodePart::String(_) => {}
                CCodePart::Expr(expr) => expr.replace_generics(generic_map),
            }
        }
    }
}

impl ReplaceGenerics<'i> for Expr<'i> {
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
            CCode(c_code) => c_code.replace_generics(generic_map),
        }

        self.ty = Default::default();
    }
}

impl ReplaceGenerics<'i> for FuncCall<'i> {
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

impl ReplaceGenerics<'i> for TypeNode<'i> {
    fn replace_generics(&mut self, generic_map: &GenericMap<'i>) {
        if let Type::GenericPlaceholder(name) = *self.ty {
            self.kind = generic_map[&name].kind.clone();
        }

        self.ty = Default::default();
    }
}

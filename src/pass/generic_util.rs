//! generic helper stuff

use crate::context::{Ctx, Intern};
use crate::error::{err, Res};
use crate::pass::ast1::*;
use crate::pass::ast2;
use crate::pass::replace_generics::GenericMap;
use crate::pass::scope::{Scopes, Symbol};
use crate::span::Span;
use crate::util::IterExt;
use std::collections::HashMap;
use std::ops::Deref;

// impl Type<'i> {
//     /// `Type::type_check` but for generic structs
//     ///
//     /// mostly duplicated from `Define::type_check` and `Type::type_check`
//     pub fn type_check_generic(self, ctx: &mut Ctx<'i>) -> Res<'i, ast2::Type<'i>> {
//         use DefineKind::*;
//         use TypeKind::*;
//         if let Named {
//             name,
//             generic_replacements: ref generic_replacements_,
//         } = self.kind
//         {
//             debug_assert!(!generic_replacements_.is_empty());
//
//             let generic_replacements: Rc<Vec<ast2::Type<'i>>> = generic_replacements_
//                 .iter()
//                 .cloned()
//                 .map(|replacement| replacement.type_check(ctx))
//                 .res_vec()?
//                 .into();
//
//             // return early if replacements contains a placeholder
//             // this should only happen with a generic func that has an arg of generic struct with a replacement of the func's placeholder
//             if generic_replacements
//                 .iter()
//                 .any(ast2::Type::contains_placeholder)
//             {
//                 return Ok(ast2::Type::Struct {
//                     nesting_prefix: Default::default(),
//                     name,
//                     generic_replacements,
//                 });
//             }
//
//             // find an associated generic func
//             let generic_symbol = ctx.scopes.find_generic_struct(
//                 name,
//                 &generic_replacements.iter().vec(),
//                 Some(self.span),
//             )?;
//             if let Symbol::GenericStruct {
//                 name,
//                 generic_placeholders,
//                 body,
//
//                 scopes_index,
//             } = generic_symbol.clone()
//             {
//                 // go to where the generic func was defined
//                 let scopes_after = ctx.scopes.0.split_off(scopes_index);
//
//                 // get mapping from placeholder names to replacement types
//                 let generic_map = generic_placeholders
//                     .iter()
//                     .copied()
//                     .zip(generic_replacements_.iter().cloned())
//                     .collect::<GenericMap<'i>>();
//
//                 let (var_defines, func_defines) =
//                     body.iter()
//                         .cloned()
//                         .partition::<Vec<_>, _>(|define| match define.kind {
//                             Var(_) => true,
//                             Func { .. } => false,
//                             _ => panic!("struct body shouldn't have {:?}", define),
//                         });
//
//                 ctx.scopes.push(Scope::new(None, false, None));
//                 let var_defines = var_defines
//                     .into_iter()
//                     .map(|mut define| {
//                         define.replace_generics(ctx, &generic_map);
//                         define.type_check(ctx)
//                     })
//                     .res_vec()?;
//                 ctx.scopes.pop();
//
//                 // add symbol if non-existent
//                 let nesting_prefix = ctx.scopes.nesting_prefix().intern(ctx);
//                 let specialized_symbol = Symbol::Struct {
//                     nesting_prefix,
//                     name,
//                     generic_replacements: generic_replacements.clone(),
//                     field_types: var_defines
//                         .iter()
//                         .cloned()
//                         .map(|define| match define {
//                             ast2::Define::Var(ast2::VarDefine { name, ty, .. }) => (name, ty),
//                             _ => unreachable!(),
//                         })
//                         .collect::<HashMap<_, _>>()
//                         .into(),
//                 };
//                 if ctx
//                     .scopes
//                     .find(&specialized_symbol, Some(self.span))
//                     .is_err()
//                 {
//                     // add symbol
//                     ctx.scopes.add(specialized_symbol, Some(self.span))?;
//
//                     let func_defines = func_defines
//                         .into_iter()
//                         .map(|mut define| {
//                             // set func receiver ty to struct
//                             if let Func { receiver_ty, .. } = &mut define.kind {
//                                 *receiver_ty = Some(Type {
//                                     span: self.span,
//                                     kind: TypeKind::Named {
//                                         name,
//                                         generic_replacements: generic_replacements_.clone(),
//                                     },
//                                 })
//                             }
//
//                             define.replace_generics(ctx, &generic_map);
//                             define.type_check(ctx)
//                         })
//                         .res_vec()?;
//
//                     let mut body = var_defines;
//                     body.extend(func_defines);
//
//                     // make and generate the define
//                     ast2::Define::Struct {
//                         full_name: format!("{}{}", nesting_prefix, name).intern(ctx),
//                         generic_replacements: generic_replacements.clone(),
//                         body: body.into(),
//                     }
//                     .gen(ctx);
//                 }
//
//                 ctx.scopes.0.extend(scopes_after);
//                 Ok(ast2::Type::Struct {
//                     nesting_prefix,
//                     name,
//                     generic_replacements,
//                 })
//             } else {
//                 unreachable!()
//             }
//         } else {
//             unreachable!()
//         }
//     }
// }

// impl FuncCall<'i> {
//     /// `FuncCall::type_check` but for generic funcs
//     ///
//     /// mostly duplicated from `Define::type_check` and `FuncCall::type_check`
//     pub fn type_check_generic(self, ctx: &mut Ctx<'i>) -> Res<'i, ast2::Expr<'i>> {
//         debug_assert!(!self.generic_replacements.is_empty());
//
//         let receiver_ty = if let Some(receiver_ty) = &self.receiver_ty {
//             Some(receiver_ty.clone().type_check(ctx)?)
//         } else {
//             None
//         };
//         let generic_replacements: Rc<Vec<ast2::Type<'i>>> = self
//             .generic_replacements
//             .iter()
//             .cloned()
//             .map(|replacement| replacement.type_check(ctx))
//             .res_vec()?
//             .into();
//         let args = self
//             .args
//             .iter()
//             .cloned()
//             .map(|arg| arg.type_check(ctx, None))
//             .res_vec()?;
//
//         // find an associated generic func
//         let generic_symbol = ctx.scopes.find_generic_func(
//             receiver_ty.as_ref(),
//             self.name,
//             &generic_replacements.iter().vec(),
//             &args.iter().map(|arg| &arg.ty).vec(),
//             Some(self.span),
//         )?;
//         if let Symbol::GenericFunc {
//             ty_ast1: mut ty,
//             receiver_ty_ast1: symbol_receiver_ty,
//             mut name,
//             generic_placeholders,
//             args: symbol_args_,
//             mut body,
//
//             scopes_index,
//             ..
//         } = generic_symbol.clone()
//         {
//             // go to where the generic func was defined
//             let scopes_after = ctx.scopes.0.split_off(scopes_index);
//
//             // get mapping from placeholder names to replacement types
//             let generic_map = generic_placeholders
//                 .iter()
//                 .copied()
//                 .zip(self.generic_replacements.iter().cloned())
//                 .collect::<GenericMap<'i>>();
//
//             ty.replace_generics(ctx, &generic_map);
//             let ty = ty.type_check(ctx)?;
//             if let Some(mut symbol_receiver_ty) = symbol_receiver_ty {
//                 symbol_receiver_ty.replace_generics(ctx, &generic_map);
//                 let symbol_receiver_ty = symbol_receiver_ty.type_check(ctx)?;
//
//                 // make sure the receiver tys actually match
//                 receiver_ty
//                     .as_ref()
//                     .unwrap()
//                     .check(&symbol_receiver_ty, Some(self.receiver_ty.unwrap().span))?;
//
//                 // attach receiver ty to name
//                 name = format!("{}::{}", receiver_ty.unwrap(), name).intern(ctx)
//             }
//             let nesting_prefix = ctx.scopes.nesting_prefix().intern(ctx);
//             let full_name = format!("{}{}", nesting_prefix, name).intern(ctx);
//
//             ctx.scopes
//                 .push(Scope::new(Some(name), false, Some(ty.clone())));
//             let mut symbol_args = Vec::with_capacity(symbol_args_.len());
//             for (mut symbol_arg, (arg, ast1_arg)) in symbol_args_
//                 .iter()
//                 .cloned()
//                 .zip(args.iter().zip(self.args.iter()))
//             {
//                 symbol_arg.replace_generics(ctx, &generic_map);
//                 let symbol_arg = symbol_arg.type_check(ctx, true, false)?;
//
//                 // make sure the args actually match
//                 arg.ty.check(&symbol_arg.ty, Some(ast1_arg.span))?;
//
//                 symbol_args.push(symbol_arg);
//             }
//
//             // add symbol if non-existent
//             let specialized_symbol = Symbol::Func {
//                 ty: ty.clone(),
//                 nesting_prefix,
//                 name,
//                 generic_replacements: generic_replacements.clone(),
//                 arg_types: args.iter().cloned().map(|it| it.ty).vec().into(),
//             };
//             if ctx
//                 .scopes
//                 .find(&specialized_symbol, Some(self.span))
//                 .is_err()
//             {
//                 // add symbol
//                 let scope = ctx.scopes.pop();
//                 ctx.scopes.add(specialized_symbol, Some(self.span))?;
//                 ctx.scopes.push(scope);
//
//                 body.replace_generics(ctx, &generic_map);
//                 let body = body.type_check(ctx)?;
//                 ctx.scopes.check_return_called(Some(self.span))?;
//
//                 // make and generate the define
//                 ast2::Define::Func {
//                     ty: ty.clone(),
//                     full_name,
//                     generic_replacements: generic_replacements.clone(),
//                     args: symbol_args.into(),
//                     body,
//                 }
//                 .gen(ctx)
//             }
//
//             ctx.scopes.pop();
//
//             ctx.scopes.0.extend(scopes_after);
//             Ok(ast2::Expr {
//                 kind: ast2::ExprKind::FuncCall {
//                     full_name,
//                     generic_replacements,
//                     args: args.into(),
//                 },
//                 ty,
//             })
//         } else {
//             unreachable!()
//         }
//     }
// }

impl ast2::Type<'i> {
    /// check for eq fuzzily
    /// where `other` is possibly generic
    fn generic_eq(&self, other: &Self) -> bool {
        use ast2::Type::*;
        match (self, other) {
            (_, GenericPlaceholder(_)) => true,
            (
                Struct {
                    name,
                    generic_replacements,
                    ..
                },
                Struct {
                    name: other_name,
                    generic_replacements: other_replacements,
                    ..
                },
            ) => {
                name == other_name
                    && generic_replacements.len() == other_replacements.len()
                    && generic_replacements
                        .iter()
                        .zip(other_replacements.iter())
                        .all(|(replacement, other_replacement)| {
                            replacement.generic_eq(other_replacement)
                        })
            }
            (Ptr(inner), Ptr(other_inner)) => inner.generic_eq(other_inner),

            _ => self == other,
        }
    }

    /// revert an ast2 Type back into ast1
    pub fn into_ast1(self, span: Span<'i>) -> Type<'i> {
        Type {
            span,
            kind: match self {
                ast2::Type::Primitive(p) => TypeKind::Primitive(p),
                ast2::Type::Struct {
                    name,
                    generic_replacements,
                    ..
                } => TypeKind::Named {
                    name,
                    generic_replacements: generic_replacements
                        .iter()
                        .cloned()
                        .map(|it| it.into_ast1(span))
                        .vec()
                        .into(),
                },
                ast2::Type::Ptr(inner) => inner.deref().clone().into_ast1(span).kind,
                ast2::Type::GenericPlaceholder(name) => TypeKind::Named {
                    name,
                    generic_replacements: Default::default(),
                },
                ast2::Type::Auto => TypeKind::Auto,
                _ => panic!("can't turn ast2 ty {:?} back into ast1", self),
            },
        }
    }

    /// check if self has a placeholder type in it
    #[allow(dead_code)]
    fn contains_placeholder(&self) -> bool {
        use ast2::Type::*;
        match self {
            GenericPlaceholder(_) => true,
            Struct {
                generic_replacements,
                ..
            } => generic_replacements.iter().any(Self::contains_placeholder),
            Ptr(inner) => inner.contains_placeholder(),
            _ => false,
        }
    }
}

impl Scopes<'i> {
    /// find a generic struct fuzzily
    /// and gen a specialized version if one doesn't exist
    pub fn find_generic_struct(
        &mut self,
        symbol: &Symbol<'i>,
        span: Span<'i>,
    ) -> Res<'i, Symbol<'i>> {
        match symbol {
            Symbol::Struct {
                name,
                generic_replacements,
                ..
            } if !generic_replacements.is_empty() => {
                // find associated generic symbol
                let generic_symbol = 'find_symbol: {
                    for scope in self.0.iter().rev() {
                        let symbol = scope.symbols.iter().find(|&s| {
                            if let Symbol::GenericStruct {
                                name: symbol_name,
                                generic_placeholders,
                                ..
                            } = s
                            {
                                if name != symbol_name {
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
                            break 'find_symbol Ok(symbol);
                        }
                    }
                    err(
                        &format!("could not find generic struct matching {}", symbol),
                        span,
                    )
                }?
                .clone();

                match generic_symbol {
                    Symbol::GenericStruct {
                        span,
                        name,
                        generic_placeholders,
                        body,

                        scopes_index,
                    } => {
                        let generic_map = generic_placeholders
                            .iter()
                            .copied()
                            .zip(
                                generic_replacements
                                    .iter()
                                    .cloned()
                                    .map(|it| it.into_ast1(span)),
                            )
                            .collect::<GenericMap<'i>>();

                        // do replacements
                        let body = body
                            .iter()
                            .cloned()
                            .map(|mut define| {
                                define.replace_generics(&generic_map);
                                define
                            })
                            .vec()
                            .into();

                        // fixme this SHOULD error if you do it twice, since it'll try to create the specialized symbol twice
                        let define = Define {
                            span,
                            kind: DefineKind::Struct {
                                name,
                                generic_placeholders: generic_placeholders.clone(),
                                body,
                            },
                        };

                        let scopes_after = self.0.split_off(scopes_index);
                        let ctx: &mut Ctx<'i> = unsafe { &mut *std::ptr::null_mut() }; // lol
                        let nesting_prefix = ctx.scopes.nesting_prefix().intern(ctx);
                        let define = define.type_check(ctx)?;
                        self.0.extend(scopes_after);

                        define.clone().gen(ctx);

                        match define {
                            ast2::Define::Struct { body, .. } => {
                                let symbol = Symbol::Struct {
                                    nesting_prefix,
                                    name,
                                    generic_replacements: generic_replacements.clone(),
                                    field_types: body
                                        .iter()
                                        .cloned()
                                        .filter_map(|define| match define {
                                            ast2::Define::Var(ast2::VarDefine {
                                                name, ty, ..
                                            }) => Some((name, ty)),
                                            _ => None,
                                        })
                                        .collect::<HashMap<_, _>>()
                                        .into(),
                                };
                                Ok(symbol)
                            }
                            _ => unreachable!(),
                        }
                    }
                    _ => unreachable!(),
                }
            }
            _ => unreachable!(),
        }
    }

    /// find a generic func fuzzily
    /// and gen a specialized version if one doesn't exist
    pub fn find_generic_func(
        &mut self,
        symbol: &Symbol<'i>,
        span: Span<'i>,
    ) -> Res<'i, Symbol<'i>> {
        match symbol {
            Symbol::Func {
                receiver_ty,
                name,
                generic_replacements,
                arg_types,
                ..
            } if !generic_replacements.is_empty() => {
                // find associated generic symbol
                let generic_symbol = 'find_symbol: {
                    for scope in self.0.iter().rev() {
                        let symbol = scope.symbols.iter().find(|&s| {
                            if let Symbol::GenericFunc {
                                receiver_ty: symbol_receiver_ty,
                                name: symbol_name,
                                generic_placeholders,
                                arg_types: symbol_arg_types,
                                ..
                            } = s
                            {
                                if receiver_ty.is_some() != symbol_receiver_ty.is_some() {
                                    return false;
                                }
                                if receiver_ty.is_some()
                                    && !receiver_ty
                                        .as_ref()
                                        .unwrap()
                                        .generic_eq(symbol_receiver_ty.as_ref().unwrap())
                                {
                                    return false;
                                }

                                if name != symbol_name {
                                    return false;
                                }

                                if generic_placeholders.len() != generic_replacements.len() {
                                    return false;
                                }

                                if arg_types.len() != symbol_arg_types.len() {
                                    return false;
                                }
                                arg_types
                                    .iter()
                                    .zip(symbol_arg_types.iter())
                                    .all(|(ty, symbol_ty)| ty.generic_eq(symbol_ty))
                            } else {
                                false
                            }
                        });
                        if let Some(symbol) = symbol {
                            break 'find_symbol Ok(symbol);
                        }
                    }
                    err(
                        &format!("could not find generic func matching {}", symbol),
                        span,
                    )
                }?
                .clone();

                match generic_symbol {
                    Symbol::GenericFunc {
                        span,
                        ty_ast1,
                        receiver_ty_ast1,
                        name,
                        generic_placeholders,
                        args,
                        body,

                        scopes_index,
                        ..
                    } => {
                        let generic_map = generic_placeholders
                            .iter()
                            .copied()
                            .zip(
                                generic_replacements
                                    .iter()
                                    .cloned()
                                    .map(|it| it.into_ast1(span)),
                            )
                            .collect::<GenericMap<'i>>();

                        // do replacements
                        let ty = {
                            let mut it = ty_ast1.clone();
                            it.replace_generics(&generic_map);
                            it
                        };
                        let receiver_ty = receiver_ty_ast1.clone().map(|mut it| {
                            it.replace_generics(&generic_map);
                            it
                        });
                        let args = args
                            .iter()
                            .cloned()
                            .map(|mut it| {
                                it.replace_generics(&generic_map);
                                it
                            })
                            .vec()
                            .into();
                        let body = Block(
                            body.0
                                .iter()
                                .cloned()
                                .map(|mut statement| {
                                    statement.replace_generics(&generic_map);
                                    statement
                                })
                                .vec()
                                .into(),
                        );

                        // fixme this SHOULD error if you do it twice, since it'll try to create the specialized symbol twice
                        let define = Define {
                            span,
                            kind: DefineKind::Func {
                                ty,
                                receiver_ty: receiver_ty.clone(),
                                name,
                                generic_placeholders: generic_placeholders.clone(),
                                args,
                                body,
                            },
                        };

                        let scopes_after = self.0.split_off(scopes_index);
                        let ctx: &mut Ctx<'i> = unsafe { &mut *std::ptr::null_mut() }; // lol
                        let nesting_prefix = ctx.scopes.nesting_prefix().intern(ctx);
                        // bruh
                        let receiver_ty = if let Some(it) = receiver_ty {
                            Some(it.type_check(ctx)?)
                        } else {
                            None
                        };
                        let define = define.type_check(ctx)?;
                        self.0.extend(scopes_after);

                        define.clone().gen(ctx);

                        match define {
                            ast2::Define::Func {
                                ty,
                                generic_replacements,
                                args,
                                ..
                            } => {
                                let symbol = Symbol::Func {
                                    ty: ty.clone(),
                                    nesting_prefix,
                                    receiver_ty,
                                    name,
                                    generic_replacements: generic_replacements.clone(),
                                    arg_types: args.iter().cloned().map(|it| it.ty).vec().into(),
                                };
                                Ok(symbol)
                            }
                            _ => unreachable!(),
                        }
                    }
                    _ => unreachable!(),
                }
            }
            _ => unreachable!(),
        }
    }
}

// impl Scopes<'i> {
//     /// find a generic func with inference
//     ///
//     /// the goal is the figure out what the generic replacements are without actually being given them
//     pub fn find_generic_func_inference(
//         &self,
//         receiver_ty: Option<&ast2::Type<'i>>,
//         name: &'i str,
//         arg_types: &[&ast2::Type<'i>],
//         type_hint: Option<&ast2::Type<'i>>,
//         span: Span<'i>,
//     ) -> Res<'i, &Symbol<'i>> {
//         for scope in self.0.iter().rev() {
//             let symbol = scope
//                 .symbols
//                 .iter()
//                 .filter(|&s| {
//                     // filter non candidates
//                     // this is filter instead of find because there can be multiple (with different # of placeholders)
//                     if let Symbol::GenericFunc {
//                         receiver_ty: symbol_receiver_ty,
//                         name: symbol_name,
//                         arg_types: symbol_arg_types,
//                         ..
//                     } = s
//                     {
//                         if receiver_ty.is_some() != symbol_receiver_ty.is_some() {
//                             return false;
//                         }
//                         if receiver_ty.is_some()
//                             && !receiver_ty
//                                 .unwrap()
//                                 .generic_eq(symbol_receiver_ty.as_ref().unwrap())
//                         {
//                             return false;
//                         }
//
//                         if name != *symbol_name {
//                             return false;
//                         }
//
//                         if arg_types.len() != symbol_arg_types.len() {
//                             return false;
//                         }
//                         arg_types
//                             .iter()
//                             .zip(symbol_arg_types.iter())
//                             .all(|(ty, symbol_ty)| ty.generic_eq(symbol_ty))
//                     } else {
//                         false
//                     }
//                 })
//                 .find(|&s| {
//                     // actually do the replacements
//                     if let Symbol::GenericFunc {
//                         ty,
//                         receiver_ty: symbol_receiver_ty,
//                         arg_types: symbol_arg_types,
//                         generic_placeholders,
//                         ..
//                     } = s
//                     {
//                         let mut ty = ty.clone();
//                         let mut symbol_receiver_ty = symbol_receiver_ty.clone();
//                         let mut symbol_arg_types = symbol_arg_types.deref().clone();
//                         let mut generic_placeholders = generic_placeholders
//                             .iter()
//                             .copied()
//                             .collect::<IndexSet<_>>();
//
//                         let mut generic_replacements =
//                             vec![Default::default(); generic_placeholders.len()]; // uninitialized values lol
//
//                         println!(
//                             "candidate? {:?} {:?} {:?} {:?}",
//                             ty, symbol_receiver_ty, generic_placeholders, symbol_arg_types
//                         );
//
//                         impl ast2::Type<'i> {
//                             /// replace a generic placeholder with a real type lol
//                             ///
//                             /// the same as the whole pass, except does it for ast2 Type instead of ast1
//                             fn replace_generic(&mut self, map: (&'i str, &ast2::Type<'i>)) {
//                                 use ast2::Type::*;
//                                 match self {
//                                     Struct {
//                                         generic_replacements,
//                                         ..
//                                     } => generic_replacements.modify(|replacements| {
//                                         for replacement in replacements {
//                                             replacement.replace_generic(map)
//                                         }
//                                     }),
//                                     Ptr(inner) => inner.modify(|inner| inner.replace_generic(map)),
//                                     GenericPlaceholder(name) if *name == map.0 => {
//                                         *self = map.1.clone()
//                                     }
//                                     _ => {}
//                                 }
//                             }
//                         }
//
//                         // first, try replacing using return type hint
//                         if let Some(replacement) = type_hint {
//                             if let ast2::Type::GenericPlaceholder(placeholder) = ty {
//                                 let map = (placeholder, replacement);
//                                 ty.replace_generic(map);
//                                 if let Some(ty) = &mut symbol_receiver_ty {
//                                     ty.replace_generic(map)
//                                 }
//                                 for ty in symbol_arg_types.iter_mut() {
//                                     ty.replace_generic(map)
//                                 }
//
//                                 let i = generic_placeholders
//                                     .swap_remove_full(&placeholder)
//                                     .unwrap()
//                                     .0;
//                                 generic_replacements[i] = replacement.clone();
//                             }
//                         }
//
//                         // then, go thru the rest of the placeholders, replacing any with the proper arg types
//                         'outer: while !generic_placeholders.is_empty() {
//                             for (i, &replacement) in arg_types.iter().enumerate() {
//                                 if let ast2::Type::GenericPlaceholder(placeholder) =
//                                     symbol_arg_types[i]
//                                 {
//                                     let map = (placeholder, replacement);
//                                     ty.replace_generic(map);
//                                     if let Some(ty) = &mut symbol_receiver_ty {
//                                         ty.replace_generic(map)
//                                     }
//                                     for ty in symbol_arg_types.iter_mut() {
//                                         ty.replace_generic(map)
//                                     }
//
//                                     let i = generic_placeholders
//                                         .swap_remove_full(&placeholder)
//                                         .unwrap()
//                                         .0;
//                                     generic_replacements[i] = replacement.clone();
//                                     continue 'outer;
//                                 }
//                             }
//                             // we still have placeholders, but no more args to replace, so func doesn't match
//                             // fixme we actually allow unused placeholders elsewhere, so make that not possible please
//                             return false;
//                         }
//
//                         println!(
//                             "candidate! {:?} {:?} {:?} {:?}",
//                             ty, symbol_receiver_ty, generic_replacements, symbol_arg_types
//                         );
//
//                         // finally, check that everything still matches after replacement
//                         if receiver_ty.is_some()
//                             && receiver_ty.unwrap() != symbol_receiver_ty.as_ref().unwrap()
//                         {
//                             return false;
//                         }
//                         if arg_types != symbol_arg_types.iter().vec() {
//                             return false;
//                         }
//
//                         let func = Symbol::Func {
//                             ty,
//                             nesting_prefix: Default::default(),
//                             name,
//                             generic_replacements: generic_replacements.into(),
//                             arg_types: symbol_arg_types.into(),
//                         };
//                         println!("match! {:?}", func);
//
//                         true
//                     } else {
//                         unreachable!()
//                     }
//                 });
//             if let Some(symbol) = symbol {
//                 return Ok(symbol);
//             }
//         }
//         err(
//             &format!(
//                 "could not find generic func matching func {} using generic replacement inference",
//                 name.encode(&[], Some(arg_types))
//             ),
//             Some(span),
//         )
//     }
// }

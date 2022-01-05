//! generic helper stuff
//! this is awful, fixme please

use crate::context::Output;
use crate::error::{err, Res};
use crate::pass::ast1::*;
use crate::pass::ast2;
use crate::pass::replace_generics::GenericMap;
use crate::pass::scope::{Scope, Scopes, Symbol};
use crate::util::{IterExt, IterResExt, RcExt};
use std::collections::HashMap;
use std::ops::Deref;

#[ext]
impl<T> Option<T> {
    fn eq_by<U>(&self, other: &Option<U>, mut eq: impl FnMut(&T, &U) -> bool) -> bool {
        match (self, other) {
            (Some(_), None) | (None, Some(_)) => false,
            (None, None) => true,
            (Some(a), Some(b)) => eq(a, b),
        }
    }
}

impl Symbol {
    /// generic symbol == generic symbol
    pub fn generic_eq_generic(&self, other: &Self) -> bool {
        use Symbol::*;
        match (self, other) {
            (
                GenericStruct {
                    name,
                    generic_placeholders,
                    ..
                },
                GenericStruct {
                    name: other_name,
                    generic_placeholders: other_placeholders,
                    ..
                },
            ) => name == other_name && generic_placeholders.len() == other_placeholders.len(),
            (
                GenericFunc {
                    receiver_ty,
                    name,
                    generic_placeholders,
                    arg_types,
                    ..
                },
                GenericFunc {
                    receiver_ty: other_receiver_ty,
                    name: other_name,
                    generic_placeholders: other_placeholders,
                    arg_types: other_arg_types,
                    ..
                },
            ) => {
                receiver_ty.eq_by(other_receiver_ty, ast2::Type::placeholder_eq_placeholder)
                    && name == other_name
                    && generic_placeholders.len() == other_placeholders.len()
                    && arg_types.iter().eq_by(
                        other_arg_types.iter(),
                        ast2::Type::placeholder_eq_placeholder,
                    )
            }

            _ => false,
        }
    }
    /// generic symbol == normal counterpart
    fn generic_eq_normal(&self, other: &Self) -> bool {
        use Symbol::*;
        match (self, other) {
            (
                Struct {
                    name,
                    generic_replacements,
                    ..
                },
                GenericStruct {
                    name: other_name,
                    generic_placeholders,
                    ..
                },
            )
            | (
                GenericStruct {
                    name: other_name,
                    generic_placeholders,
                    ..
                },
                Struct {
                    name,
                    generic_replacements,
                    ..
                },
            ) => name == other_name && generic_replacements.len() == generic_placeholders.len(),
            (
                Func {
                    receiver_ty,
                    name,
                    generic_replacements,
                    arg_types,
                    ..
                },
                GenericFunc {
                    receiver_ty: other_receiver_ty,
                    name: other_name,
                    generic_placeholders,
                    arg_types: other_arg_types,
                    ..
                },
            )
            | (
                GenericFunc {
                    receiver_ty: other_receiver_ty,
                    name: other_name,
                    generic_placeholders,
                    arg_types: other_arg_types,
                    ..
                },
                Func {
                    receiver_ty,
                    name,
                    generic_replacements,
                    arg_types,
                    ..
                },
            ) => {
                receiver_ty.eq_by(other_receiver_ty, ast2::Type::placeholder_eq_any)
                    && name == other_name
                    && generic_replacements.len() == generic_placeholders.len()
                    && arg_types
                        .iter()
                        .eq_by(other_arg_types.iter(), ast2::Type::placeholder_eq_any)
            }

            _ => false,
        }
    }
}
impl ast2::Type {
    /// placeholder will match any other PLACEHOLDER
    fn placeholder_eq_placeholder(&self, other: &Self) -> bool {
        use ast2::Type::*;
        match (self, other) {
            (GenericPlaceholder(_), GenericPlaceholder(_)) => true,
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
                    && generic_replacements.iter().eq_by(
                        other_replacements.iter(),
                        ast2::Type::placeholder_eq_placeholder,
                    )
            }
            (Ptr(inner), Ptr(other_inner)) => inner.placeholder_eq_placeholder(other_inner),

            _ => self == other,
        }
    }
    /// placeholder will match any other TYPE
    fn placeholder_eq_any(&self, other: &Self) -> bool {
        use ast2::Type::*;
        match (self, other) {
            (_, GenericPlaceholder(_)) | (GenericPlaceholder(_), _) => true,
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
                    && generic_replacements
                        .iter()
                        .eq_by(other_replacements.iter(), ast2::Type::placeholder_eq_any)
            }
            (Ptr(inner), Ptr(other_inner)) => inner.placeholder_eq_any(other_inner),

            _ => self == other,
        }
    }
}

impl ast2::Type {
    /// revert an ast2 Type back into ast1
    pub fn into_ast1(self) -> Type {
        match self {
            ast2::Type::Primitive(kind) => Type::Primitive(kind),
            ast2::Type::Struct {
                name,
                generic_replacements,
                ..
            } => Type::Named {
                name,
                generic_replacements: generic_replacements
                    .iter()
                    .cloned()
                    .map(|x| x.into_ast1())
                    .vec()
                    .into(),
            },
            ast2::Type::Ptr(inner) => inner.deref().clone().into_ast1(),
            ast2::Type::GenericPlaceholder(name) => Type::Named {
                name,
                generic_replacements: Default::default(),
            },
            ast2::Type::Auto => Type::Auto,
            _ => panic!("can't turn ast2 ty {:?} back into ast1", self),
        }
    }

    /// check if self has a placeholder type in it
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

impl Scopes {
    /// find a generic symbol
    ///
    /// the created symbol will be at the same scope level as the generic symbol
    pub fn find_generic(&mut self, o: &mut Output, symbol: &Symbol) -> Res<Symbol> {
        match symbol {
            Symbol::Struct {
                name,
                generic_replacements,
                ..
            } => {
                debug_assert!(!generic_replacements.is_empty());

                // this should only happen when type checking generic func's args or receiver ty
                // and there's a struct with a replacement of one of the func's placeholders
                if generic_replacements
                    .iter()
                    .any(ast2::Type::contains_placeholder)
                {
                    return Ok(symbol.clone());
                }

                let scopes = self.0.iter().rev().map(|x| x.symbols.clone()).vec(); // fixme? yikes
                for symbols in scopes {
                    // try to find already specialized version first
                    if let Some(x) = symbols.get(symbol) {
                        return Ok(x.clone());
                    }

                    // then find generic one and do stuff
                    // we don't have to worry about ambiguous here
                    for s in symbols {
                        match s {
                            Symbol::GenericStruct {
                                span: other_span,
                                generic_placeholders,
                                body: mut other_body,

                                scopes_index,
                                ..
                            } if s.generic_eq_normal(symbol) => {
                                let generic_map = generic_placeholders
                                    .iter()
                                    .copied()
                                    .zip(
                                        generic_replacements.iter().cloned().map(|x| x.into_ast1()),
                                    )
                                    .collect::<GenericMap>();

                                // do replacements
                                other_body.modify(|x| {
                                    for define in x {
                                        define.replace_generics(&generic_map)
                                    }
                                });

                                let scopes_after = self.0.split_off(scopes_index);
                                Define::Struct {
                                    span: other_span,
                                    name: *name,
                                    generic_placeholders: Default::default(),
                                    body: other_body,
                                }
                                .type_check(self, o, generic_replacements.clone())?
                                .gen(o);

                                let symbol =
                                    self.0.last().unwrap().symbols.get(symbol).unwrap().clone();
                                self.0.extend(scopes_after);
                                return Ok(symbol);
                            }
                            _ => {}
                        }
                    }
                }

                err(
                    &format!("could not find generic struct matching {}", symbol),
                    symbol.name_span(),
                )
            }

            Symbol::Func {
                span,
                receiver_ty,
                name,
                generic_replacements,
                arg_types,
                ..
            } => {
                {
                    debug_assert!(!generic_replacements.is_empty());

                    let scopes = self.0.iter().rev().map(|x| x.symbols.clone()).vec(); // fixme? yikes
                    for symbols in scopes {
                        // try to find already specialized version first
                        if let Some(x) = symbols.get(symbol) {
                            return Ok(x.clone());
                        }

                        // then find generic one and do stuff
                        // fixme ambiguous can happen
                        for s in symbols {
                            match s {
                                Symbol::GenericFunc {
                                    span: other_span,
                                    ty: other_ty,
                                    receiver_ty: other_receiver_ty,
                                    generic_placeholders,
                                    args: mut other_args,
                                    body: mut other_body,

                                    scopes_index,
                                    ..
                                } if s.generic_eq_normal(symbol) => {
                                    let generic_map = generic_placeholders
                                        .iter()
                                        .copied()
                                        .zip(
                                            generic_replacements
                                                .iter()
                                                .cloned()
                                                .map(|x| x.into_ast1()),
                                        )
                                        .collect::<GenericMap>();

                                    // do replacements
                                    let mut other_ty_ast1 = other_ty.into_ast1();
                                    other_ty_ast1.replace_generics(&generic_map);
                                    let mut other_receiver_ty_ast1 =
                                        other_receiver_ty.map(|x| x.into_ast1());
                                    if let Some(x) = &mut other_receiver_ty_ast1 {
                                        x.replace_generics(&generic_map)
                                    }
                                    other_args.modify(|x| {
                                        for arg in x {
                                            arg.replace_generics(&generic_map)
                                        }
                                    });
                                    other_body.0.modify(|x| {
                                        for statement in x {
                                            statement.replace_generics(&generic_map)
                                        }
                                    });

                                    let scopes_after = self.0.split_off(scopes_index);
                                    // check that the types actually match
                                    // modified from Define::type_check
                                    let other_receiver_ty = other_receiver_ty_ast1
                                        .as_ref()
                                        .cloned()
                                        .map(|x| x.type_check(self, o))
                                        .transpose()?;
                                    self.push(Scope::new(false, false, None));
                                    let other_arg_types = other_args
                                        .iter()
                                        .cloned()
                                        .map(|x| x.type_check(self, o, true, false).map(|x| x.ty))
                                        .res_vec()?;
                                    self.pop();
                                    receiver_ty
                                        .as_ref()
                                        .map(|x| {
                                            x.check(
                                                other_receiver_ty.as_ref().unwrap(),
                                                // fixme span bodge
                                                *span,
                                            )
                                        })
                                        .transpose()?;
                                    arg_types
                                        .iter()
                                        .zip(other_arg_types.iter())
                                        .map(|(regular, generic)| {
                                            regular.check(
                                                generic, // fixme span bodge
                                                *span,
                                            )
                                        })
                                        .res_vec()?;

                                    Define::Func {
                                        span: other_span,
                                        ty: other_ty_ast1,
                                        receiver_ty: other_receiver_ty_ast1,
                                        name: *name,
                                        generic_placeholders: Default::default(),
                                        args: other_args,
                                        body: other_body,
                                    }
                                    .type_check(self, o, generic_replacements.clone())?
                                    .gen(o);

                                    let symbol =
                                        self.0.last().unwrap().symbols.get(symbol).unwrap().clone();
                                    self.0.extend(scopes_after);
                                    return Ok(symbol);
                                }
                                _ => {}
                            }
                        }
                    }

                    err(
                        &format!("could not find generic func matching {}", symbol),
                        symbol.name_span(),
                    )
                }
            }

            _ => unreachable!(),
        }
    }
}

impl Scopes {
    /// find a generic func with inference
    ///
    /// the goal is the figure out what the generic replacements are without actually being given them
    ///
    /// fixme very very bad
    pub fn find_generic_func_inference(
        &mut self,
        o: &mut Output,
        symbol: &Symbol,
        type_hint: Option<&ast2::Type>,
    ) -> Res<Symbol> {
        match symbol {
            Symbol::Func {
                span,
                receiver_ty,
                name,
                generic_replacements,
                arg_types,
                ..
            } => {
                debug_assert!(generic_replacements.is_empty());

                println!(
                    "match --- {:?} {:?} :: {} < ?? >( {:?} )",
                    type_hint, receiver_ty, name, arg_types
                );

                let scopes = self.0.iter().rev().map(|x| x.symbols.clone()).vec(); // fixme? yikes
                for symbols in scopes {
                    // try to find already specialized version first
                    let specialized_func = symbols.iter().find(|s| match s {
                        Symbol::Func {
                            receiver_ty: other_receiver_ty,
                            name: other_name,
                            generic_replacements,
                            arg_types: other_arg_types,
                            ..
                        } => {
                            receiver_ty == other_receiver_ty
                                && name == other_name
                                && !generic_replacements.is_empty()
                                && arg_types == other_arg_types
                        }
                        _ => false,
                    });
                    if let Some(x) = specialized_func {
                        return Ok(x.clone());
                    }

                    // that didn't work
                    // so find candidate generic funcs
                    // and infer replacements from them
                    let mut matching_symbols = vec![];
                    'find_symbols: for s in symbols {
                        match s {
                            Symbol::GenericFunc {
                                ty,
                                receiver_ty: other_receiver_ty,
                                name: other_name,
                                generic_placeholders,
                                arg_types: other_arg_types,
                                ..
                            } if receiver_ty
                                .eq_by(&other_receiver_ty, ast2::Type::placeholder_eq_any)
                                && name == &other_name
                                && arg_types.iter().eq_by(
                                    other_arg_types.iter(),
                                    ast2::Type::placeholder_eq_any,
                                ) =>
                            {
                                println!(
                                    "candidate? --- {:?} {:?} :: {} < {:?} >( {:?} )",
                                    ty,
                                    other_receiver_ty,
                                    other_name,
                                    generic_placeholders,
                                    other_arg_types
                                );

                                let mut generic_placeholders = generic_placeholders
                                    .iter()
                                    .map(|x| ast2::Type::GenericPlaceholder(*x))
                                    .zip(0..)
                                    .collect::<HashMap<_, _>>();
                                let mut generic_replacements =
                                    vec![Default::default(); generic_placeholders.len()]; // uninitialized values lol

                                // impl ast2::Type {
                                //     /// replace a generic placeholder with a real type lol
                                //     ///
                                //     /// the same as the whole pass, except does it for ast2 Type instead of ast1
                                //     fn replace_generic(&mut self, map: (Ident, &ast2::Type)) {
                                //         use ast2::Type::*;
                                //         match self {
                                //             Struct {
                                //                 generic_replacements,
                                //                 ..
                                //             } => generic_replacements.modify(|replacements| {
                                //                 for replacement in replacements {
                                //                     replacement.replace_generic(map)
                                //                 }
                                //             }),
                                //             Ptr(inner) => {
                                //                 inner.modify(|inner| inner.replace_generic(map))
                                //             }
                                //             GenericPlaceholder(name) if *name == map.0 => {
                                //                 *self = map.1.clone()
                                //             }
                                //             _ => {}
                                //         }
                                //     }
                                // }

                                'infer_replacements: while !generic_placeholders.is_empty() {
                                    if let (Some(placeholder), Some(replacement)) =
                                        (&other_receiver_ty, &receiver_ty)
                                    {
                                        if let Some(&i) = generic_placeholders.get(placeholder) {
                                            generic_replacements[i] = replacement.clone();
                                            generic_placeholders.remove(placeholder);
                                            continue 'infer_replacements;
                                        }
                                    }

                                    for (placeholder, replacement) in
                                        other_arg_types.iter().zip(arg_types.iter())
                                    {
                                        if let Some(&i) = generic_placeholders.get(placeholder) {
                                            generic_replacements[i] = replacement.clone();
                                            generic_placeholders.remove(placeholder);
                                            continue 'infer_replacements;
                                        }
                                    }

                                    if let (Some(placeholder), replacement) = (type_hint, &ty) {
                                        if let Some(&i) = generic_placeholders.get(placeholder) {
                                            generic_replacements[i] = replacement.clone();
                                            generic_placeholders.remove(placeholder);
                                            continue 'infer_replacements;
                                        }
                                    }

                                    // nothing was replaced, but there are still placeholders, so no match
                                    // fixme we actually allow unused placeholders elsewhere, so make that not possible please
                                    continue 'find_symbols;
                                }
                                // // first, try to infer placeholders from arg types
                                // 'replace_generics: while !generic_placeholders.is_empty() {
                                //     for (i, replacement) in arg_types.iter().enumerate() {
                                //         if let ast2::Type::GenericPlaceholder(placeholder) =
                                //             other_arg_types[i]
                                //         {
                                //             let map = (placeholder, replacement);
                                //             ty.replace_generic(map);
                                //             if let Some(x) = &mut other_receiver_ty {
                                //                 x.replace_generic(map)
                                //             }
                                //             for ty in other_arg_types.iter_mut() {
                                //                 ty.replace_generic(map)
                                //             }
                                //
                                //             let i =
                                //                 generic_placeholders.remove(placeholder).unwrap();
                                //             generic_replacements[i] = replacement.clone();
                                //             continue 'replace_generics;
                                //         }
                                //     }
                                //     // out of args to replace
                                //     break;
                                // }
                                //
                                // // infer from receiver type
                                // if let Some(replacement) = receiver_ty {
                                //     if let ast2::Type::GenericPlaceholder(placeholder) =
                                //         *other_receiver_ty.as_ref().unwrap()
                                //     {
                                //         let map = (placeholder, replacement);
                                //         ty.replace_generic(map);
                                //         if let Some(x) = &mut other_receiver_ty {
                                //             x.replace_generic(map)
                                //         }
                                //         for ty in other_arg_types.iter_mut() {
                                //             ty.replace_generic(map)
                                //         }
                                //
                                //         let i = generic_placeholders.remove(placeholder).unwrap();
                                //         generic_replacements[i] = replacement.clone();
                                //     }
                                // }
                                //
                                // // then, try to infer placeholder from type hint
                                // if let Some(replacement) = type_hint {
                                //     if let ast2::Type::GenericPlaceholder(placeholder) = ty {
                                //         let map = (placeholder, replacement);
                                //         ty.replace_generic(map);
                                //         if let Some(x) = &mut other_receiver_ty {
                                //             x.replace_generic(map)
                                //         }
                                //         for ty in other_arg_types.iter_mut() {
                                //             ty.replace_generic(map)
                                //         }
                                //
                                //         let i = generic_placeholders.remove(placeholder).unwrap();
                                //         generic_replacements[i] = replacement.clone();
                                //     }
                                // }
                                //
                                // if !generic_placeholders.is_empty() {
                                //     // we still have placeholders, but nothing left to replace, so func doesn't match
                                //     // fixme we actually allow unused placeholders elsewhere, so make that not possible please
                                //     continue;
                                // }

                                println!(
                                    "candidate! --- {:?} {:?} :: {} < {:?} >( {:?} )",
                                    ty,
                                    other_receiver_ty,
                                    other_name,
                                    generic_replacements,
                                    other_arg_types
                                );

                                // finally, check that the types still match after replacement
                                // if receiver_ty != &other_receiver_ty
                                //     || arg_types != &other_arg_types
                                // {
                                //     eprintln!("bruh moment");
                                //     continue;
                                // }

                                println!("match!");

                                let symbol = self.find_generic(
                                    o,
                                    &Symbol::new_func(
                                        *span,
                                        receiver_ty.clone(),
                                        *name,
                                        generic_replacements.into(),
                                        arg_types.clone(),
                                    ),
                                )?;
                                matching_symbols.push(symbol);
                            }
                            _ => {}
                        }
                    }
                    #[allow(clippy::comparison_chain)]
                    if matching_symbols.len() > 1 {
                        return err(
                            &format!(
                                "could not find generic func matching {} because it is ambiguous\n\
                                candidates:\n{}\n\
                                try specifying replacements explicitly",
                                symbol,
                                matching_symbols
                                    .iter()
                                    .map(|x| x.to_string())
                                    .vec()
                                    .join("\n")
                            ),
                            symbol.name_span(),
                        );
                    } else if matching_symbols.len() == 1 {
                        return Ok(matching_symbols[0].clone());
                    }
                }

                err(
                    &format!(
                        "could not find {} (including using generic replacement inference)",
                        symbol
                    ),
                    symbol.name_span(),
                )
            }

            _ => unreachable!(),
        }
    }
}

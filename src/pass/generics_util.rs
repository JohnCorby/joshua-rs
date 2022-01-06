//! generic helper stuff
//! this is awful, fixme please

use crate::context::Output;
use crate::error::{err, Res};
use crate::pass::ast1::*;
use crate::pass::ast2;
use crate::pass::replace_generics::GenericMap;
use crate::pass::scope::{Scope, Scopes, Symbol};
use crate::span::Span;
use crate::util::{IterExt, IterResExt, RcExt};
use std::collections::HashMap;
use std::ops::Deref;
use std::rc::Rc;

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
            (Ptr(.., inner), Ptr(.., other_inner)) => inner.placeholder_eq_placeholder(other_inner),

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
            (Ptr(.., inner), Ptr(.., other_inner)) => inner.placeholder_eq_any(other_inner),

            _ => self == other,
        }
    }
}

impl ast2::Type {
    /// revert an ast2 Type back into ast1
    pub fn into_ast1(self, span: Span) -> Type {
        match self {
            ast2::Type::Primitive(kind) => Type::Primitive(span, kind),
            ast2::Type::Struct {
                name,
                generic_replacements,
                ..
            } => Type::Named {
                span,
                name: Ident(span, name),
                generic_replacements: generic_replacements
                    .iter()
                    .cloned()
                    .map(|x| x.into_ast1(span))
                    .vec()
                    .into(),
            },
            ast2::Type::Ptr(inner) => Type::Ptr(span, inner.deref().clone().into_ast1(span).into()),
            ast2::Type::GenericPlaceholder(name) => Type::Named {
                span,
                name: Ident(span, name),
                generic_replacements: Default::default(),
            },
            ast2::Type::Auto => Type::Auto(span),
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
            Ptr(.., inner) => inner.contains_placeholder(),
            _ => false,
        }
    }
}

impl Scopes {
    /// find a generic symbol
    ///
    /// the created symbol will be at the same scope level as the generic symbol
    pub fn find_generic(&mut self, o: &mut Output, symbol: &Symbol, err_span: Span) -> Res<Symbol> {
        match symbol {
            Symbol::Struct {
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
                                span,
                                name_ast1,
                                generic_placeholders,
                                mut body,

                                scopes_index,
                                ..
                            } if s.generic_eq_normal(symbol) => {
                                let generic_map = generic_placeholders
                                    .iter()
                                    .enumerate()
                                    .map(|(i, x)| {
                                        (x.1, generic_replacements[i].clone().into_ast1(x.0))
                                    })
                                    .collect::<GenericMap>();

                                // do replacements
                                body.modify(|x| {
                                    for define in x {
                                        define.replace_generics(&generic_map)
                                    }
                                });

                                let scopes_after = self.0.split_off(scopes_index);
                                Define::Struct {
                                    span,
                                    name: name_ast1,
                                    generic_placeholders: Default::default(),
                                    body,
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
                    err_span,
                )
            }

            Symbol::Func {
                receiver_ty,
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
                                    span,
                                    mut ty_ast1,
                                    mut receiver_ty_ast1,
                                    name_ast1,
                                    generic_placeholders,
                                    mut args,
                                    mut body,

                                    scopes_index,
                                    ..
                                } if s.generic_eq_normal(symbol) => {
                                    let generic_map = generic_placeholders
                                        .iter()
                                        .enumerate()
                                        .map(|(i, x)| {
                                            (x.1, generic_replacements[i].clone().into_ast1(x.0))
                                        })
                                        .collect::<GenericMap>();

                                    // do replacements
                                    ty_ast1.replace_generics(&generic_map);
                                    if let Some(x) = &mut receiver_ty_ast1 {
                                        x.replace_generics(&generic_map)
                                    }
                                    args.modify(|x| {
                                        for arg in x {
                                            arg.replace_generics(&generic_map)
                                        }
                                    });
                                    body.1.modify(|x| {
                                        for statement in x {
                                            statement.replace_generics(&generic_map)
                                        }
                                    });

                                    let scopes_after = self.0.split_off(scopes_index);
                                    // check that the types actually match
                                    // modified from Define::type_check
                                    let other_receiver_ty = receiver_ty_ast1
                                        .as_ref()
                                        .cloned()
                                        .map(|x| x.type_check(self, o))
                                        .transpose()?;
                                    self.push(Scope::new(false, false, None));
                                    let other_arg_types = args
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
                                                receiver_ty_ast1.as_ref().unwrap().span(),
                                            )
                                        })
                                        .transpose()?;
                                    arg_types
                                        .iter()
                                        .enumerate()
                                        .map(|(i, x)| x.check(&other_arg_types[i], args[i].span))
                                        .res_vec()?;

                                    Define::Func {
                                        span,
                                        ty: ty_ast1,
                                        receiver_ty: receiver_ty_ast1,
                                        name: name_ast1,
                                        generic_placeholders: Default::default(),
                                        args,
                                        body,
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
                        err_span,
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
        err_span: Span,
    ) -> Res<Symbol> {
        match symbol {
            Symbol::Func {
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
                        println!("specialized! --- {}", x);
                        return Ok(x.clone());
                    }

                    // that didn't work
                    // so find candidate generic funcs
                    // and infer replacements from them
                    let mut matching_symbols = vec![];
                    'find_symbols: for s in symbols {
                        match s {
                            Symbol::GenericFunc {
                                receiver_ty: other_receiver_ty,
                                name: other_name,
                                arg_types: other_arg_types,

                                ty,

                                span,
                                mut ty_ast1,
                                mut receiver_ty_ast1,
                                name_ast1,
                                generic_placeholders,
                                mut args,
                                mut body,

                                scopes_index,
                            } if receiver_ty
                                .eq_by(&other_receiver_ty, ast2::Type::placeholder_eq_any)
                                && name == &other_name
                                && arg_types.iter().eq_by(
                                    other_arg_types.iter(),
                                    ast2::Type::placeholder_eq_any,
                                ) =>
                            {
                                println!(
                                    "candidate --- {:?} {:?} :: {} < {:?} >( {:?} )",
                                    ty,
                                    other_receiver_ty,
                                    other_name,
                                    generic_placeholders,
                                    other_arg_types
                                );

                                let generic_replacements: Rc<Vec<_>> = {
                                    let mut placeholders = generic_placeholders
                                        .iter()
                                        .map(|x| ast2::Type::GenericPlaceholder(x.1))
                                        .zip(0..)
                                        .collect::<HashMap<_, _>>();
                                    let mut replacements =
                                        vec![Default::default(); placeholders.len()]; // uninitialized values lol

                                    'infer_replacements: while !placeholders.is_empty() {
                                        if let (Some(placeholder), Some(replacement)) =
                                            (&other_receiver_ty, &receiver_ty)
                                        {
                                            if let Some(&i) = placeholders.get(placeholder) {
                                                replacements[i] = replacement.clone();
                                                placeholders.remove(placeholder);
                                                continue 'infer_replacements;
                                            }
                                        }

                                        for (placeholder, replacement) in
                                            other_arg_types.iter().zip(arg_types.iter())
                                        {
                                            if let Some(&i) = placeholders.get(placeholder) {
                                                replacements[i] = replacement.clone();
                                                placeholders.remove(placeholder);
                                                continue 'infer_replacements;
                                            }
                                        }

                                        if let (placeholder, Some(replacement)) = (&ty, type_hint) {
                                            if let Some(&i) = placeholders.get(placeholder) {
                                                replacements[i] = replacement.clone();
                                                placeholders.remove(placeholder);
                                                continue 'infer_replacements;
                                            }
                                        }

                                        // nothing was replaced, but there are still placeholders, so no match
                                        // fixme we actually allow unused placeholders elsewhere, so make that not possible please
                                        continue 'find_symbols;
                                    }

                                    replacements
                                }
                                .into();

                                println!(
                                    "match? --- {:?} {:?} :: {} < {:?} >( {:?} )",
                                    ty,
                                    other_receiver_ty,
                                    other_name,
                                    generic_replacements,
                                    other_arg_types
                                );

                                // modified from Scopes::find_generic
                                let symbol = {
                                    let generic_map = generic_placeholders
                                        .iter()
                                        .enumerate()
                                        .map(|(i, x)| {
                                            (x.1, generic_replacements[i].clone().into_ast1(x.0))
                                        })
                                        .collect::<GenericMap>();

                                    // do replacements
                                    // do replacements
                                    ty_ast1.replace_generics(&generic_map);
                                    if let Some(x) = &mut receiver_ty_ast1 {
                                        x.replace_generics(&generic_map)
                                    }
                                    args.modify(|x| {
                                        for arg in x {
                                            arg.replace_generics(&generic_map)
                                        }
                                    });
                                    body.1.modify(|x| {
                                        for statement in x {
                                            statement.replace_generics(&generic_map)
                                        }
                                    });

                                    let scopes_after = self.0.split_off(scopes_index);
                                    // check that the types actually match
                                    // modified from Define::type_check
                                    let other_receiver_ty = receiver_ty_ast1
                                        .as_ref()
                                        .cloned()
                                        .map(|x| x.type_check(self, o))
                                        .transpose()?;
                                    self.push(Scope::new(false, false, None));
                                    let other_arg_types = args
                                        .iter()
                                        .cloned()
                                        .map(|x| x.type_check(self, o, true, false).map(|x| x.ty))
                                        .res_vec()?;
                                    self.pop();
                                    let res = receiver_ty
                                        .as_ref()
                                        .map(|x| {
                                            x.check(
                                                other_receiver_ty.as_ref().unwrap(),
                                                receiver_ty_ast1.as_ref().unwrap().span(),
                                            )
                                        })
                                        .transpose();
                                    if let Err(err) = res {
                                        println!("type check failed for receiver_ty: {}", err);
                                        self.0.extend(scopes_after);
                                        continue 'find_symbols;
                                    }
                                    let res = arg_types
                                        .iter()
                                        .enumerate()
                                        .map(|(i, x)| x.check(&other_arg_types[i], args[i].span))
                                        .res_vec();
                                    if let Err(err) = res {
                                        println!("type check failed for receiver_ty: {}", err);
                                        self.0.extend(scopes_after);
                                        continue 'find_symbols;
                                    }

                                    Define::Func {
                                        span,
                                        ty: ty_ast1,
                                        receiver_ty: receiver_ty_ast1,
                                        name: name_ast1,
                                        generic_placeholders: Default::default(),
                                        args,
                                        body,
                                    }
                                    .type_check(self, o, generic_replacements.clone())?
                                    .gen(o);

                                    // inject replacements into symbol
                                    let symbol = &{
                                        let mut symbol = symbol.clone();
                                        let Symbol::Func {
                                            generic_replacements: gr,
                                            ..
                                        } = &mut symbol else { unreachable!() };
                                        *gr = generic_replacements;
                                        symbol
                                    };
                                    let symbol =
                                        self.0.last().unwrap().symbols.get(symbol).unwrap().clone();
                                    self.0.extend(scopes_after);
                                    symbol
                                };

                                println!("match!");
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
                            err_span,
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
                    err_span,
                )
            }

            _ => unreachable!(),
        }
    }
}

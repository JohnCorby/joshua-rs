//! generic helper stuff

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

fn option_eq<A, B>(a: Option<A>, b: Option<B>, mut eq: impl FnMut(A, B) -> bool) -> bool {
    match (a, b) {
        (Some(_), None) | (None, Some(_)) => false,
        (None, None) => true,
        (Some(a), Some(b)) => eq(a, b),
    }
}
fn iter_eq<A, B>(
    a: impl ExactSizeIterator<Item = A>,
    b: impl ExactSizeIterator<Item = B>,
    mut eq: impl FnMut(A, B) -> bool,
) -> bool {
    a.len() == b.len() && a.zip(b).all(|(a, b)| eq(a, b))
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
                option_eq(
                    receiver_ty.as_ref(),
                    other_receiver_ty.as_ref(),
                    ast2::Type::placeholder_eq_placeholder,
                ) && name == other_name
                    && generic_placeholders.len() == other_placeholders.len()
                    && iter_eq(
                        arg_types.iter(),
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
                option_eq(
                    receiver_ty.as_ref(),
                    other_receiver_ty.as_ref(),
                    ast2::Type::placeholder_eq_any,
                ) && name == other_name
                    && generic_replacements.len() == generic_placeholders.len()
                    && iter_eq(
                        arg_types.iter(),
                        other_arg_types.iter(),
                        ast2::Type::placeholder_eq_any,
                    )
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
                    && iter_eq(
                        generic_replacements.iter(),
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
                    && iter_eq(
                        generic_replacements.iter(),
                        other_replacements.iter(),
                        ast2::Type::placeholder_eq_any,
                    )
            }
            (Ptr(inner), Ptr(other_inner)) => inner.placeholder_eq_any(other_inner),

            _ => self == other,
        }
    }
}

impl ast2::Type {
    /// revert an ast2 Type back into ast1
    pub fn into_ast1(self, span: Span) -> Type {
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
    ///
    pub fn find_generic(&mut self, o: &mut Output, symbol: &Symbol, span: Span) -> Res<Symbol> {
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

                let scopes = self.0.iter().map(|scope| scope.symbols.clone()).vec(); // fixme? yikes
                for symbols in scopes {
                    // try to find already specialized version first
                    if let Some(it) = symbols.get(symbol) {
                        return Ok(it.clone());
                    }

                    // then find generic one and do stuff
                    // we don't have to worry about ambiguous here
                    for s in symbols {
                        match s {
                            Symbol::GenericStruct {
                                span,
                                generic_placeholders,
                                body,

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
                                            .map(|it| it.into_ast1(span)),
                                    )
                                    .collect::<GenericMap>();

                                // do replacements
                                let mut body = body.clone();
                                body.modify(|it| {
                                    for define in it {
                                        define.replace_generics(&generic_map)
                                    }
                                });

                                let scopes_after = self.0.split_off(scopes_index);
                                Define {
                                    span,
                                    kind: DefineKind::Struct {
                                        name,
                                        generic_placeholders: Default::default(),
                                        body,
                                    },
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
                    span,
                )
            }

            Symbol::Func {
                receiver_ty,
                name,
                generic_replacements,
                arg_types,
                ..
            } => {
                {
                    debug_assert!(!generic_replacements.is_empty());

                    let scopes = self.0.iter().map(|scope| scope.symbols.clone()).vec(); // fixme? yikes
                    for symbols in scopes {
                        // try to find already specialized version first
                        if let Some(it) = symbols.get(symbol) {
                            return Ok(it.clone());
                        }

                        // then find generic one and do stuff
                        // fixme ambiguous can happen
                        for s in symbols {
                            match s {
                                Symbol::GenericFunc {
                                    span,
                                    mut ty_ast1,
                                    mut receiver_ty_ast1,
                                    generic_placeholders,
                                    mut args,
                                    mut body,

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
                                                .map(|it| it.into_ast1(span)),
                                        )
                                        .collect::<GenericMap>();

                                    // do replacements
                                    ty_ast1.replace_generics(&generic_map);
                                    if let Some(it) = &mut receiver_ty_ast1 {
                                        it.replace_generics(&generic_map)
                                    }
                                    args.modify(|it| {
                                        for arg in it {
                                            arg.replace_generics(&generic_map)
                                        }
                                    });
                                    body.0.modify(|it| {
                                        for statement in it {
                                            statement.replace_generics(&generic_map)
                                        }
                                    });

                                    let scopes_after = self.0.split_off(scopes_index);
                                    // check that the types actually match
                                    // modified from Define::type_check
                                    let other_receiver_ty = receiver_ty_ast1
                                        .as_ref()
                                        .cloned()
                                        .map(|it| it.type_check(self, o))
                                        .transpose()?;
                                    self.push(Scope::new(false, false, None));
                                    let other_arg_types = args
                                        .iter()
                                        .cloned()
                                        .map(|it| {
                                            it.type_check(self, o, true, false).map(|it| it.ty)
                                        })
                                        .res_vec()?;
                                    self.pop();
                                    receiver_ty
                                        .as_ref()
                                        .map(|it| {
                                            it.check(
                                                other_receiver_ty.as_ref().unwrap(),
                                                receiver_ty_ast1.as_ref().unwrap().span,
                                            )
                                        })
                                        .transpose()?;
                                    arg_types
                                        .iter()
                                        .zip(other_arg_types.iter())
                                        .zip(args.iter())
                                        .map(|((arg_type, other_arg_type), arg)| {
                                            arg_type.check(other_arg_type, arg.span)
                                        })
                                        .res_vec()?;

                                    Define {
                                        span,
                                        kind: DefineKind::Func {
                                            ty: ty_ast1,
                                            receiver_ty: receiver_ty_ast1,
                                            name,
                                            generic_placeholders: Default::default(),
                                            args,
                                            body,
                                        },
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
                        span,
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
    pub fn find_generic_func_inference(
        &mut self,
        o: &mut Output,
        symbol: &Symbol,
        type_hint: Option<&ast2::Type>,
        span: Span,
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

                let scopes = self.0.iter().map(|scope| scope.symbols.clone()).vec(); // fixme? yikes
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
                    if let Some(it) = specialized_func {
                        return Ok(it.clone());
                    }

                    // that didn't work
                    // so find candidate generic funcs
                    // and infer replacements from them
                    let mut matching_symbols = vec![];
                    for s in symbols {
                        match s {
                            Symbol::GenericFunc {
                                mut ty,
                                receiver_ty: mut other_receiver_ty,
                                name: other_name,
                                generic_placeholders,
                                arg_types: other_arg_types,
                                ..
                            } if option_eq(
                                receiver_ty.as_ref(),
                                other_receiver_ty.as_ref(),
                                ast2::Type::placeholder_eq_any,
                            ) && name == &other_name
                                && iter_eq(
                                    arg_types.iter(),
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

                                let mut other_arg_types = other_arg_types.deref().clone();
                                let mut generic_placeholders = generic_placeholders
                                    .iter()
                                    .copied()
                                    .zip(0..)
                                    .collect::<HashMap<_, _>>();
                                let mut generic_replacements =
                                    vec![Default::default(); generic_placeholders.len()]; // uninitialized values lol

                                impl ast2::Type {
                                    /// replace a generic placeholder with a real type lol
                                    ///
                                    /// the same as the whole pass, except does it for ast2 Type instead of ast1
                                    fn replace_generic(
                                        &mut self,
                                        map: (&'static str, &ast2::Type),
                                    ) {
                                        use ast2::Type::*;
                                        match self {
                                            Struct {
                                                generic_replacements,
                                                ..
                                            } => generic_replacements.modify(|replacements| {
                                                for replacement in replacements {
                                                    replacement.replace_generic(map)
                                                }
                                            }),
                                            Ptr(inner) => {
                                                inner.modify(|inner| inner.replace_generic(map))
                                            }
                                            GenericPlaceholder(name) if *name == map.0 => {
                                                *self = map.1.clone()
                                            }
                                            _ => {}
                                        }
                                    }
                                }

                                // first, try to infer placeholders from arg types
                                'replace_generics: while !generic_placeholders.is_empty() {
                                    for (i, replacement) in arg_types.iter().enumerate() {
                                        if let ast2::Type::GenericPlaceholder(placeholder) =
                                            other_arg_types[i]
                                        {
                                            let map = (placeholder, replacement);
                                            ty.replace_generic(map);
                                            if let Some(it) = &mut other_receiver_ty {
                                                it.replace_generic(map)
                                            }
                                            for ty in other_arg_types.iter_mut() {
                                                ty.replace_generic(map)
                                            }

                                            let i =
                                                generic_placeholders.remove(placeholder).unwrap();
                                            generic_replacements[i] = replacement.clone();
                                            continue 'replace_generics;
                                        }
                                    }
                                    // out of args to replace
                                    break;
                                }

                                // then, try to infer placeholder from type hint
                                if let Some(replacement) = type_hint {
                                    if let ast2::Type::GenericPlaceholder(placeholder) = ty {
                                        let map = (placeholder, replacement);
                                        ty.replace_generic(map);
                                        if let Some(it) = &mut other_receiver_ty {
                                            it.replace_generic(map)
                                        }
                                        for ty in other_arg_types.iter_mut() {
                                            ty.replace_generic(map)
                                        }

                                        let i = generic_placeholders.remove(placeholder).unwrap();
                                        generic_replacements[i] = replacement.clone();
                                    }
                                }

                                if !generic_placeholders.is_empty() {
                                    // we still have placeholders, but nothing left to replace, so func doesn't match
                                    // fixme we actually allow unused placeholders elsewhere, so make that not possible please
                                    continue;
                                }

                                println!(
                                    "candidate! --- {:?} {:?} :: {} < {:?} >( {:?} )",
                                    ty,
                                    other_receiver_ty,
                                    other_name,
                                    generic_replacements,
                                    other_arg_types
                                );

                                // finally, check that the types still match after replacement
                                if receiver_ty != &other_receiver_ty
                                    || arg_types.deref() != &other_arg_types
                                {
                                    continue;
                                }

                                println!("match!");

                                let symbol = self.find_generic(
                                    o,
                                    &Symbol::new_func(
                                        receiver_ty.clone(),
                                        name,
                                        generic_replacements.into(),
                                        arg_types.clone(),
                                    ),
                                    span,
                                )?;
                                matching_symbols.push(symbol);
                            }
                            _ => {}
                        }
                    }
                    if matching_symbols.len() > 1 {
                        return err(
                            &format!(
                                "could not find generic func matching {} because it is ambiguous\n\
                            candidates:\n{}\n\
                            try specifying replacements explicitly",
                                symbol,
                                matching_symbols
                                    .iter()
                                    .map(|it| it.to_string())
                                    .vec()
                                    .join("\n")
                            ),
                            span,
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
                    span,
                )
            }
            _ => unreachable!(),
        }
    }
}

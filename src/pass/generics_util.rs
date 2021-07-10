//! generic helper stuff

use crate::context::Output;
use crate::error::{IntoErr, Res};
use crate::pass::ast1::*;
use crate::pass::ast2;
use crate::pass::replace_generics::GenericMap;
use crate::pass::scope::{Scope, Scopes, Symbol};
use crate::span::Span;
use crate::util::{IterExt, IterResExt, RcExt};
use std::ops::Deref;
use std::rc::Rc;

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
    /// special eq check used in various places
    pub fn generic_eq(&self, other: &Self) -> bool {
        use Symbol::*;
        match (self, other) {
            // generic vs generic
            // used in Scopes::add
            // placeholder == placeholder
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

            // normal vs generic
            // used in Scopes::find_generic
            // placeholder == any
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

            _ => unreachable!(),
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

impl Scopes {
    /// find a generic symbol using a specialized one.
    /// add and gen that specialized one if it hasn't already been
    pub fn find_generic(&mut self, o: &mut Output, symbol: &Symbol, span: Span) -> Res<Symbol> {
        match symbol {
            Symbol::Struct {
                generic_replacements,
                ..
            } if !generic_replacements.is_empty() => {
                // this should only happen when type checking generic func's args or receiver ty
                // and there's a struct with a replacement of one of the func's placeholders
                if generic_replacements
                    .iter()
                    .any(ast2::Type::contains_placeholder)
                {
                    return Ok(symbol.clone());
                }

                // find associated generic symbol
                let generic_symbol = self
                    .0
                    .iter()
                    .rev()
                    .map(|scope| &scope.symbols)
                    .flatten()
                    .find(|&s| matches!(s, Symbol::GenericStruct { .. }) && symbol.generic_eq(s))
                    .ok_or_else(|| {
                        format!("could not find generic struct matching {}", symbol)
                            .into_err(Some(span))
                    })?
                    .clone();

                match generic_symbol {
                    Symbol::GenericStruct {
                        span,
                        name,
                        generic_placeholders,
                        mut body,

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
                            .collect::<GenericMap>();

                        // do replacements
                        body.modify(|it| {
                            for define in it {
                                define.replace_generics(&generic_map)
                            }
                        });

                        let scopes_after = self.0.split_off(scopes_index);
                        let symbol = if let Some(symbol) = self
                            .0
                            .last()
                            .unwrap()
                            .symbols
                            .get(&Symbol::new_struct(name, generic_replacements.clone()))
                        {
                            symbol
                        } else {
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

                            self.0
                                .last()
                                .unwrap()
                                .symbols
                                .get(&Symbol::new_struct(name, generic_replacements.clone()))
                                .unwrap()
                        }
                        .clone();
                        self.0.extend(scopes_after);
                        Ok(symbol)
                    }
                    _ => unreachable!(),
                }
            }

            Symbol::Func {
                generic_replacements,
                ..
            } if !generic_replacements.is_empty() => {
                // find associated generic symbol
                let generic_symbol = self
                    .0
                    .iter()
                    .rev()
                    .map(|scope| &scope.symbols)
                    .flatten()
                    .find(|&s| matches!(s, Symbol::GenericFunc { .. }) && symbol.generic_eq(s))
                    .ok_or_else(|| {
                        format!("could not find generic func matching {}", symbol)
                            .into_err(Some(span))
                    })?
                    .clone();

                match generic_symbol {
                    Symbol::GenericFunc {
                        span,
                        mut ty_ast1,
                        mut receiver_ty_ast1,
                        name,
                        generic_placeholders,
                        mut args,
                        mut body,

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
                        // modified from Define::type_check
                        let receiver_ty = receiver_ty_ast1
                            .as_ref()
                            .cloned()
                            .map(|it| it.type_check(self, o))
                            .transpose()?;
                        self.push(Scope::new(None, false, None));
                        let arg_types: Rc<Vec<_>> = args
                            .iter()
                            .cloned()
                            .map(|it| it.type_check(self, o, true, false).map(|it| it.ty))
                            .res_vec()?
                            .into();
                        self.pop();

                        let symbol = if let Some(symbol) =
                            self.0.last().unwrap().symbols.get(&Symbol::new_func(
                                receiver_ty.clone(),
                                name,
                                generic_replacements.clone(),
                                arg_types.clone(),
                            )) {
                            symbol
                        } else {
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

                            self.0
                                .last()
                                .unwrap()
                                .symbols
                                .get(&Symbol::new_func(
                                    receiver_ty,
                                    name,
                                    generic_replacements.clone(),
                                    arg_types,
                                ))
                                .unwrap()
                        }
                        .clone();
                        self.0.extend(scopes_after);
                        Ok(symbol)
                    }
                    _ => unreachable!(),
                }
            }
            _ => unreachable!(),
        }
    }
}

// // test
// fn bruh() {
//     struct Hello;
//
//     fn yes<T>(t: T) {}
//
//     fn bruh2() {
//         struct Hello2;
//         yes(Hello2);
//     }
//     yes(Hello2);
// }

// impl Scopes {
//     /// find a generic func with inference
//     ///
//     /// the goal is the figure out what the generic replacements are without actually being given them
//     pub fn find_generic_func_inference(
//         &self,
//         receiver_ty: Option<&ast2::Type>,
//         name: &'static str,
//         arg_types: &[&ast2::Type],
//         type_hint: Option<&ast2::Type>,
//         span: Span,
//     ) -> Res<&Symbol> {
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
//                         impl ast2::Type {
//                             /// replace a generic placeholder with a real type lol
//                             ///
//                             /// the same as the whole pass, except does it for ast2 Type instead of ast1
//                             fn replace_generic(&mut self, map: (&'static str, &ast2::Type)) {
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

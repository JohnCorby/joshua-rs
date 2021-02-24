//! scope stack contains scopes
//! scopes contain symbols
//! symbols allow us to check for existence and type of stuff we define

use crate::define::VarDefine;
use crate::error::{err, Res};
use crate::interned_string::InternedStr;
use crate::span::Span;
use crate::statement::Block;
use crate::ty::{PrimitiveType, Type, TypeNode};
use std::collections::{HashMap, HashSet};
use std::fmt::{Debug, Display, Formatter};
use std::hash::{Hash, Hasher};

#[derive(Debug, Clone)]
pub enum Symbol<'i> {
    Func {
        ty: Type<'i>,
        name: InternedStr<'i>,
        arg_types: Vec<Type<'i>>,
    },
    Var {
        ty: Type<'i>,
        name: InternedStr<'i>,
    },
    StructType {
        name: InternedStr<'i>,
        field_types: HashMap<InternedStr<'i>, Type<'i>>,
    },
    GenericPlaceholderType(InternedStr<'i>),
    GenericFunc {
        // cached for faster access on eq/hash
        _ty: Type<'i>,
        _arg_types: Vec<Type<'i>>,

        // copied from func define
        span: Span<'i>,
        ty_node: TypeNode<'i>,
        name: InternedStr<'i>,
        generic_placeholders: Vec<InternedStr<'i>>,
        args: Vec<VarDefine<'i>>,
        body: Block<'i>,
    },
}
impl<'i> Symbol<'i> {
    pub fn ty(&self) -> Type<'i> {
        use Symbol::*;
        match self {
            Func { ty, .. } | Var { ty, .. } | GenericFunc { _ty: ty, .. } => *ty,
            StructType { name, .. } => Type::Struct(*name),
            GenericPlaceholderType(name) => Type::GenericPlaceholder(*name),
        }
    }
}

/// note: eq contains cases that hash doesnt cover, check both when comparing
impl Hash for Symbol<'_> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        use Symbol::*;
        match self {
            Func {
                name, arg_types, ..
            }
            | GenericFunc {
                name,
                _arg_types: arg_types,
                ..
            } => {
                name.hash(state);
                arg_types.hash(state);
            }
            Var { name, .. } | StructType { name, .. } | GenericPlaceholderType(name) => {
                name.hash(state)
            }
        }
    }
}
impl PartialEq for Symbol<'_> {
    fn eq(&self, other: &Self) -> bool {
        use Symbol::*;
        match (self, other) {
            (
                Func {
                    name: name1,
                    arg_types: arg_types1,
                    ..
                },
                Func {
                    name: name2,
                    arg_types: arg_types2,
                    ..
                },
            )
            | (
                Func {
                    name: name1,
                    arg_types: arg_types1,
                    ..
                },
                GenericFunc {
                    name: name2,
                    _arg_types: arg_types2,
                    ..
                },
            )
            | (
                GenericFunc {
                    name: name1,
                    _arg_types: arg_types1,
                    ..
                },
                Func {
                    name: name2,
                    arg_types: arg_types2,
                    ..
                },
            ) => name1 == name2 && arg_types1 == arg_types2,
            (Var { name: name1, .. }, Var { name: name2, .. })
            | (StructType { name: name1, .. }, StructType { name: name2, .. })
            | (GenericPlaceholderType(name1), GenericPlaceholderType(name2)) => name1 == name2,

            _ => false,
        }
    }
}
impl Eq for Symbol<'_> {}

impl Display for Symbol<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        use Symbol::*;
        match self {
            Func {
                name, arg_types, ..
            } => write!(
                f,
                "func symbol `{}` with arg types ({})",
                name,
                arg_types
                    .iter()
                    .map(Type::to_string)
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            Var { name, .. } => write!(f, "var symbol `{}`", name),
            StructType { name, .. } => write!(f, "struct type symbol `{}`", name),
            GenericPlaceholderType(name) => write!(f, "generic placeholder type symbol `{}`", name),
            GenericFunc {
                name, _arg_types, ..
            } => write!(
                f,
                "generic func symbol `{}` and arg types ({})",
                name,
                _arg_types
                    .iter()
                    .map(Type::to_string)
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
        }
    }
}

#[derive(Debug)]
pub struct Scopes<'i>(Vec<Scope<'i>>);
impl Default for Scopes<'_> {
    fn default() -> Self {
        Scopes(vec![Scope {
            in_loop: false,
            func_return_type: None,
            return_called: false,
            symbols: Default::default(),
        }])
    }
}

impl<'i> Scopes<'i> {
    pub fn push(&mut self, in_loop: bool, func_return_type: impl Into<Option<Type<'i>>>) {
        self.0.push(Scope {
            in_loop,
            func_return_type: func_return_type.into(),
            return_called: false,

            symbols: Default::default(),
        });
    }

    pub fn pop(&mut self) {
        self.0.pop().expect("tried to pop an empty scope stack");
    }
}

#[derive(Debug)]
pub struct Scope<'i> {
    in_loop: bool,
    func_return_type: Option<Type<'i>>,
    return_called: bool,

    symbols: HashSet<Symbol<'i>>,
}

impl<'i> Scopes<'i> {
    pub fn in_loop(&self) -> bool {
        for scope in self.0.iter().rev() {
            // accounts for inner functions
            if scope.func_return_type.is_some() {
                return false;
            }
            if scope.in_loop {
                return true;
            }
        }
        false
    }
    pub fn func_return_type(&self) -> Type<'i> {
        for scope in self.0.iter().rev() {
            if let Some(ty) = scope.func_return_type {
                return ty;
            }
        }
        unreachable!("tried to get func return type when we arent in any func")
    }

    /// fixme: this doesnt account for branches and stuff (e.g. `if(false) return` works)
    pub fn return_called(&mut self) {
        for scope in self.0.iter_mut().rev() {
            // find the scope that is a function
            if scope.func_return_type.is_some() {
                scope.return_called = true;
                return;
            }
        }
    }
    /// note: only checks one current scope and outer ones
    pub fn check_return_called(&self, span: impl Into<Option<Span<'i>>>) -> Res<'i, ()> {
        let return_called = self.0.last().unwrap().return_called;
        let is_void = self.func_return_type() == Type::Primitive(PrimitiveType::Void);

        if !return_called && !is_void {
            err("return was never called for non-void func", span)
        } else {
            Ok(())
        }
    }
}

impl<'i> Scopes<'i> {
    pub fn add(&mut self, symbol: Symbol<'i>, span: impl Into<Option<Span<'i>>>) -> Res<'i, ()> {
        let scope = self.0.last_mut().unwrap();
        // find with hash first
        if let Some(symbol) = scope.symbols.get(&symbol) {
            return err(format!("{} already defined", symbol), span);
        }
        // then use eq if hash didnt find anything
        if let Some(symbol) = scope.symbols.iter().find(|&s| s == &symbol) {
            return err(format!("{} already defined", symbol), span);
        }
        scope.symbols.insert(symbol);
        Ok(())
    }

    fn find(&self, symbol: Symbol<'i>, span: impl Into<Option<Span<'i>>>) -> Res<'i, Symbol<'i>> {
        for scope in self.0.iter().rev() {
            // find with hash first
            if let Some(symbol) = scope.symbols.get(&symbol) {
                return Ok(symbol.clone());
            }
            // then use eq if hash didnt find anything
            if let Some(symbol) = scope.symbols.iter().find(|&s| s == &symbol) {
                return Ok(symbol.clone());
            }

            // then try to find a fuzzy generic match (for funcs only)
            if let Symbol::Func {
                name: name1,
                arg_types: arg_types1,
                ..
            } = &symbol
            {
                if let Some(symbol) = scope.symbols.iter().find(|&s| {
                    if let Symbol::GenericFunc {
                        name: name2,
                        _arg_types: arg_types2,
                        ..
                    } = s
                    {
                        println!("FUZZY GENERIC SEARCH! for {} == {}", symbol, s);
                        if name1 != name2 {
                            println!("failed on name :( for {} == {}", name1, name2);
                            return false;
                        }
                        println!("name match! for {} == {}", name1, name2);
                        for (&ty1, &ty2) in arg_types1.iter().zip(arg_types2.iter()) {
                            if let Type::GenericPlaceholder(_) = ty2 {
                                println!("generic fuzzy match! for {} == {}", ty1, ty2)
                                // generic ty2 will always match ty1, so don't return false
                            } else if ty1 != ty2 {
                                println!("failed on regular types :( for {} == {}", ty1, ty2);
                                return false;
                            }
                        }
                        println!("MATCHED!");
                        true
                    } else {
                        false
                    }
                }) {
                    return Ok(symbol.clone());
                }
            }
        }
        err(format!("could not find {}", symbol), span)
    }

    pub fn get_var(
        &self,
        name: InternedStr<'i>,
        span: impl Into<Option<Span<'i>>>,
    ) -> Res<'i, Symbol<'i>> {
        self.find(
            Symbol::Var {
                ty: Default::default(),
                name,
            },
            span,
        )
    }
    /// note: can return normal or generic func
    pub fn get_func(
        &self,
        name: InternedStr<'i>,
        arg_types: impl AsRef<[Type<'i>]>,
        span: impl Into<Option<Span<'i>>>,
    ) -> Res<'i, Symbol<'i>> {
        self.find(
            Symbol::Func {
                ty: Default::default(),
                name,
                arg_types: arg_types.as_ref().into(),
            },
            span,
        )
    }
    pub fn get_struct(
        &self,
        name: InternedStr<'i>,
        span: impl Into<Option<Span<'i>>>,
    ) -> Res<'i, Symbol<'i>> {
        self.find(
            Symbol::StructType {
                name,
                field_types: Default::default(),
            },
            span,
        )
    }
    pub fn get_generic(
        &self,
        name: InternedStr<'i>,
        span: impl Into<Option<Span<'i>>>,
    ) -> Res<'i, Symbol<'i>> {
        self.find(Symbol::GenericPlaceholderType(name), span)
    }
}

//! scope stack contains scopes
//! scopes contain symbols
//! symbols allow us to check for existence and type of stuff we define

use crate::context::Ctx;
use crate::define::{Define, DefineKind};
use crate::error::{err, Res};
use crate::expr::FuncCall;
use crate::interned_string::InternedStr;
use crate::span::Span;
use crate::ty::{PrimitiveType, Type};
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
        ty: Type<'i>,
        name: InternedStr<'i>,
        generic_placeholders: Vec<InternedStr<'i>>,
        arg_types: Vec<Type<'i>>,
    },
}
impl<'i> Symbol<'i> {
    pub fn ty(&self) -> Type<'i> {
        use Symbol::*;
        match self {
            Func { ty, .. } | Var { ty, .. } | GenericFunc { ty, .. } => *ty,
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
                name, arg_types, ..
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
                GenericFunc {
                    name: name1,
                    arg_types: arg_types1,
                    ..
                },
                GenericFunc {
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
                    arg_types: arg_types2,
                    ..
                },
            )
            | (
                GenericFunc {
                    name: name1,
                    arg_types: arg_types1,
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
            | (GenericPlaceholderType(name1), GenericPlaceholderType(name2))
            | (StructType { name: name1, .. }, GenericPlaceholderType(name2))
            | (GenericPlaceholderType(name1), StructType { name: name2, .. }) => name1 == name2,

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
                name, arg_types, ..
            } => write!(
                f,
                "generic func symbol `{}` and arg types ({})",
                name,
                arg_types
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
    pub fn get_type(
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
}

impl<'i> Ctx<'i> {
    /// create a template for a generic func using a func_define
    pub fn make_generic_func(&mut self, func_define: Define<'i>) -> Res<'i, ()> {
        if let Define {
            span,
            kind:
                DefineKind::Func {
                    ty_node,
                    name,
                    generic_placeholders,
                    args,
                    ..
                },
        } = func_define
        {
            assert!(
                !generic_placeholders.is_empty(),
                "tried to make a generic func from a normal func"
            );

            // hack so generic types resolve in args and ret type
            self.scopes.push(false, None);
            for &placeholder in &generic_placeholders {
                self.scopes
                    .add(Symbol::GenericPlaceholderType(placeholder), span)?;
            }
            let ty = ty_node.init_ty(self)?;
            let arg_types = args
                .iter()
                .map(|var_define| var_define.ty_node.init_ty(self))
                .collect::<Res<'i, Vec<_>>>()?;
            self.scopes.pop();

            self.scopes.add(
                Symbol::GenericFunc {
                    ty,
                    name,
                    generic_placeholders,
                    arg_types,
                },
                span,
            )
        } else {
            unreachable!()
        }
    }

    /// take a generic function and specialize it
    /// returns specialized ret type and arg types
    #[allow(warnings)]
    pub fn specialize_generic_func(
        &mut self,
        func_call: &FuncCall<'i>,
    ) -> Res<'i, (Type<'i>, Vec<Type<'i>>)> {
        assert!(
            !func_call.generic_replacements.is_empty(),
            "tried to specialize a non-generic func"
        );

        let mut arg_types = func_call
            .args
            .iter()
            .map(|it| it.init_ty(self))
            .collect::<Res<'i, Vec<_>>>()?;

        // get or create specialized func
        // if self.scopes.get_func(func_call.name, func_call)
        // let i = format!("");
        // self.make_func(i)?;

        dbg!(&self.scopes);
        let placeholders =
            match self
                .scopes
                .get_func(func_call.name.clone(), &arg_types, func_call.span)?
            {
                Symbol::GenericFunc {
                    generic_placeholders,
                    ..
                } => generic_placeholders,
                symbol => {
                    return err(
                        format!("expected generic func symbol, but got {}", symbol),
                        func_call.span,
                    )
                }
            };
        let replacements = func_call
            .generic_replacements
            .iter()
            .map(|it| it.init_ty(self))
            .collect::<Res<'i, Vec<_>>>()?;
        assert_eq!(placeholders.len(), func_call.generic_replacements.len());
        let generic_map = placeholders
            .iter()
            .map(|it| Type::GenericPlaceholder(*it))
            .zip(replacements)
            .collect::<HashMap<_, _>>();
        // todo remove because it should already include replacements (EXPECT THE RET_TYPE!!!)
        // let mut ret_type = func_call
        //     .ty
        //     .get()
        //     .copied()
        //     .expect("ret type should be initialized at this point");

        todo!("replace generics in specialized func (in ret type and body. arg types should already be specialized)")
        // Ok((ret_type, arg_types))
    }
}

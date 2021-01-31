//! scope stack contains scopes
//! scopes contain symbols
//! symbols allow us to check for existence and type of stuff we define

use crate::error::MyResult;
use crate::ty::Type;
use parking_lot::{Mutex, MutexGuard};
use std::collections::HashSet;
use std::hash::{Hash, Hasher};

static SCOPES: Mutex<Vec<Scope>> = Mutex::new(Vec::new());
type OwningRefMut<T> = owning_ref::OwningRefMut<MutexGuard<'static, Vec<Scope>>, T>;

#[derive(Debug, Clone)]
pub struct Scope {
    is_loop: bool,
    symbols: HashSet<Symbol>,
}

#[derive(Debug, Clone)]
pub enum Symbol {
    Func {
        ty: Type,
        name: String,
        arg_types: Vec<Type>,
    },
    Var {
        ty: Type,
        name: String,
    },
    Type(Type),
}
impl Hash for Symbol {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Symbol::Func {
                name, arg_types, ..
            } => {
                name.hash(state);
                arg_types.hash(state);
            }
            Symbol::Var { name, .. } => name.hash(state),
            Symbol::Type(ty) => ty.hash(state),
        }
    }
}
impl PartialEq for Symbol {
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
            ) => name1 == name2 && arg_types1 == arg_types2,
            (Var { name: name1, .. }, Var { name: name2, .. }) => name1 == name2,
            (Type(ty1), Type(ty2)) => ty1 == ty2,
            (_, _) => false,
        }
    }
}
impl Eq for Symbol {}
impl ToString for Symbol {
    fn to_string(&self) -> String {
        match self {
            Symbol::Func {
                name, arg_types, ..
            } => format!(
                "func {} with arg types ({})",
                name,
                arg_types
                    .iter()
                    .map(Type::to_string)
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            Symbol::Var { name, .. } => format!("var {}", name),
            Symbol::Type(ty) => ty.to_string(),
        }
    }
}

impl Scope {
    pub fn push(is_loop: bool) {
        SCOPES.lock().push(Scope {
            is_loop,
            symbols: Default::default(),
        });
    }

    pub fn pop() {
        SCOPES
            .lock()
            .pop()
            .expect("tried to pop an empty scope stack");
    }

    pub fn current() -> OwningRefMut<Scope> {
        OwningRefMut::new(SCOPES.lock()).map_mut(|scopes| {
            scopes
                .last_mut()
                .expect("tried to get current scope from empty scope stack")
        })
    }

    pub fn is_loop(&self) -> bool {
        self.is_loop
    }

    pub fn add(&mut self, symbol: Symbol) -> MyResult<()> {
        let err = format!("{} already defined", symbol.to_string());
        let success = self.symbols.insert(symbol);
        if !success {
            Err(err.into())
        } else {
            Ok(())
        }
    }

    fn find(symbol: Symbol) -> MyResult<Symbol> {
        for scope in SCOPES.lock().iter() {
            if let Some(symbol) = scope.symbols.get(&symbol) {
                return Ok(symbol.clone());
            }
        }
        Err(format!("could not find {}", symbol.to_string()).into())
    }

    pub fn get_var(name: impl AsRef<str>) -> MyResult<Symbol> {
        Self::find(Symbol::Var {
            ty: Type::default(),
            name: name.as_ref().into(),
        })
    }
    #[allow(dead_code)]
    pub fn get_func(name: impl AsRef<str>, arg_types: impl AsRef<[Type]>) -> MyResult<Symbol> {
        Self::find(Symbol::Func {
            ty: Type::default(),
            name: name.as_ref().into(),
            arg_types: arg_types.as_ref().into(),
        })
    }
}

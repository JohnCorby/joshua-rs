//! scope stack contains scopes
//! scopes contain symbols
//! symbols allow us to check for existence and type of stuff we define

use crate::error::MyResult;
use crate::ty::Type;
use owning_ref::OwningRefMut;
use parking_lot::{Mutex, MutexGuard};
use std::collections::HashSet;
use std::hash::{Hash, Hasher};

static SCOPES: Mutex<Vec<Scope>> = Mutex::new(Vec::new());

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
    Type {
        name: String,
    },
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
            Symbol::Type { name, .. } => name.hash(state),
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
            (Type { name: name1, .. }, Type { name: name2, .. }) => name1 == name2,
            (_, _) => false,
        }
    }
}
impl Eq for Symbol {}

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

    pub fn current() -> OwningRefMut<MutexGuard<'static, Vec<Scope>>, Scope> {
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
        let success = self.symbols.insert(symbol);
        if !success {
            Err("symbol already defined".into())
        } else {
            Ok(())
        }
    }

    fn find(mut predicate: impl FnMut(&Symbol) -> bool) -> MyResult<Symbol> {
        for scope in SCOPES.lock().iter().rev() {
            if let Some(item) = scope.symbols.iter().find(|item| predicate(item)) {
                return Ok(item.clone());
            }
        }
        Err("could not find symbol".into())
    }

    pub fn get_var(name: impl AsRef<str>) -> MyResult<Symbol> {
        Self::find(|item| {
            if let Symbol::Var { name: n, .. } = item {
                return n == name.as_ref();
            }
            false
        })
    }
    #[allow(dead_code)]
    pub fn get_func(name: impl AsRef<str>, arg_types: impl AsRef<[Type]>) -> MyResult<Symbol> {
        Self::find(|item| {
            if let Symbol::Func {
                name: n,
                arg_types: at,
                ..
            } = item
            {
                return n == name.as_ref() && at == arg_types.as_ref();
            }
            false
        })
    }
}

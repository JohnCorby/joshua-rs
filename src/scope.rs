//! scope stack contains scopes
//! scopes contain symbols
//! symbols allow us to check for existence and type of stuff we define

use crate::error::MyResult;
use crate::ty::Type;
use parking_lot::Mutex;
use std::ops::Try;

static SCOPES: Mutex<Vec<Scope>> = Mutex::new(Vec::new());

#[derive(Debug, Clone, Default)]
pub struct Scope {
    items: Vec<Symbol>,
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

impl Scope {
    pub fn push() {
        SCOPES.lock().push(Scope::default());
    }

    pub fn pop() {
        SCOPES
            .lock()
            .pop()
            .expect("tried to pop an empty scope stack");
    }

    pub fn add(symbol: Symbol) {
        SCOPES
            .lock()
            .last_mut()
            .expect("tried to get current scope from empty scope stack")
            .items
            .push(symbol);
    }

    fn find(mut predicate: impl FnMut(&Symbol) -> bool) -> Option<Symbol> {
        for scope in SCOPES.lock().iter().rev() {
            if let Some(item) = scope.items.iter().find(|item| predicate(item)) {
                return Some(item.clone());
            }
        }
        None
    }

    pub fn get_var(name: impl AsRef<str>) -> MyResult<Symbol> {
        Self::find(|item| {
            if let Symbol::Var { name: n, .. } = item {
                return n == name.as_ref();
            }
            false
        })
        .into_result()
        .map_err(|_| format!("unable to find var `{}`", name.as_ref()).into())
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
        .into_result()
        .map_err(|_| {
            format!(
                "unable to find func `{}` with matching arg types",
                name.as_ref()
            )
            .into()
        })
    }
}

//! scope stack contains scopes
//! scopes contain symbols
//! symbols allow us to check for existence and type of stuff we define

use crate::error::MyResult;
use crate::ty::{HasType, Type};
use parking_lot::{Mutex, MutexGuard};

static SCOPES: Mutex<Vec<Scope>> = Mutex::new(Vec::new());
type OwningRefMut<T> = owning_ref::OwningRefMut<MutexGuard<'static, Vec<Scope>>, T>;

#[derive(Debug, Clone)]
pub struct Scope {
    in_loop: bool,
    func_return_type: Option<Type>,
    symbols: Vec<Symbol>,
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
    pub fn init() {
        Self::push(false, None);
    }

    pub fn push(in_loop: bool, func_return_type: Option<Type>) {
        SCOPES.lock().push(Self {
            in_loop,
            func_return_type,
            symbols: Default::default(),
        });
    }

    pub fn pop() {
        SCOPES
            .lock()
            .pop()
            .expect("tried to pop an empty scope stack");
    }

    fn current() -> OwningRefMut<Scope> {
        OwningRefMut::new(SCOPES.lock()).map_mut(|scopes| {
            scopes
                .last_mut()
                .expect("tried to get current scope from empty scope stack")
        })
    }

    pub fn in_loop() -> bool {
        for scope in SCOPES.lock().iter().rev() {
            if scope.func_return_type.is_some() {
                return false;
            }
            if scope.in_loop {
                return true;
            }
        }
        false
    }
    pub fn func_return_type() -> Type {
        for scope in SCOPES.lock().iter().rev() {
            if let Some(ty) = &scope.func_return_type {
                return ty.clone();
            }
        }
        unreachable!("tried to get func return type when we arent in any func")
    }

    pub fn add(symbol: Symbol) -> MyResult<()> {
        let symbols = &mut Self::current().symbols;
        if symbols.contains(&symbol) {
            return Err(format!("{} already defined", symbol.to_string()).into());
        }
        symbols.push(symbol);
        Ok(())
    }

    fn find(symbol: Symbol) -> MyResult<Symbol> {
        for scope in SCOPES.lock().iter().rev() {
            if let Some(symbol) = scope.symbols.iter().find(|s| s == &&symbol) {
                return Ok(symbol.clone());
            }
        }
        Err(format!("could not find {}", symbol.to_string()).into())
    }

    pub fn get_var(name: impl AsRef<str>) -> MyResult<Symbol> {
        Self::find(Symbol::Var {
            ty: Default::default(),
            name: name.as_ref().into(),
        })
    }
    pub fn get_func(name: impl AsRef<str>, arg_types: impl AsRef<[Type]>) -> MyResult<Symbol> {
        Self::find(Symbol::Func {
            ty: Default::default(),
            name: name.as_ref().into(),
            arg_types: arg_types.as_ref().into(),
        })
    }
    pub fn get_type(name: impl AsRef<str>) -> MyResult<Symbol> {
        Self::find(Symbol::Type(Type::Named {
            pos: Default::default(),
            name: name.as_ref().into(),
        }))
    }
}

impl HasType for Symbol {
    fn ty(&self) -> Type {
        match self {
            Symbol::Func { ty, .. } => ty,
            Symbol::Var { ty, .. } => ty,
            Symbol::Type(ty) => ty,
        }
        .clone()
    }
}

//! scope stack contains scopes
//! scopes contain symbols
//! symbols allow us to check for existence and type of stuff we define

use crate::cached::CachedString;
use crate::error::{err, warn, MyResult};
use crate::ty::{HasType, Type};
use parking_lot::{Mutex, MutexGuard};
use std::collections::HashMap;
use std::fmt::{Debug, Display, Formatter};

static SCOPES: Mutex<Vec<Scope>> = Mutex::new(Vec::new());
fn scopes() -> MutexGuard<'static, Vec<Scope>> {
    SCOPES.try_lock().expect("SCOPES locked")
}

#[derive(Debug)]
pub struct Scope {
    in_loop: bool,
    func_return_type: Option<Type>,
    symbols: Vec<Symbol>,
}

#[derive(Debug, Clone)]
pub enum Symbol {
    Func {
        ty: Type,
        name: CachedString,
        arg_types: Vec<Type>,
    },
    Var {
        ty: Type,
        name: CachedString,
    },
    Struct {
        name: CachedString,
        field_types: HashMap<CachedString, Type>,
    },
    Type(Type),
}
impl HasType for Symbol {
    fn ty(&self) -> Type {
        match self {
            Symbol::Func { ty, .. } => *ty,
            Symbol::Var { ty, .. } => *ty,
            Symbol::Struct { name, .. } => Type::Named(*name),
            Symbol::Type(ty) => *ty,
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
            (Struct { name: name1, .. }, Struct { name: name2, .. }) => name1 == name2,
            (Type(ty1), Type(ty2)) => ty1 == ty2,

            // struct/named type equality
            (Struct { name: name1, .. }, Type(self::Type::Named(name2)))
            | (Type(self::Type::Named(name1)), Struct { name: name2, .. }) => name1 == name2,

            (_, _) => false,
        }
    }
}
impl Eq for Symbol {}
impl Display for Symbol {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            Self::Func {
                name, arg_types, ..
            } => write!(
                f,
                "func `{}` with arg types ({})",
                name,
                arg_types
                    .iter()
                    .map(Type::to_string)
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            Self::Var { name, .. } => write!(f, "var `{}`", name),
            Self::Struct { name, .. } => write!(f, "struct `{}`", name),
            Self::Type(ty) => Display::fmt(&ty, f),
        }
    }
}

#[derive(Debug)]
pub struct ScopeHandle {
    index: usize,
    pop: bool,
}
impl ScopeHandle {
    pub fn in_loop(&self) -> bool {
        for scope in scopes()[..=self.index].iter().rev() {
            if scope.func_return_type.is_some() {
                return false;
            }
            if scope.in_loop {
                return true;
            }
        }
        false
    }
    pub fn func_return_type(&self) -> Type {
        for scope in scopes()[..=self.index].iter().rev() {
            if let Some(ty) = scope.func_return_type {
                return ty;
            }
        }
        unreachable!("tried to get func return type when we arent in any func")
    }

    pub fn add(&mut self, symbol: Symbol) -> MyResult<()> {
        let symbols = &mut scopes()[self.index].symbols;
        if symbols.contains(&symbol) {
            return err(format!("{} already defined", symbol));
        }
        symbols.push(symbol);
        Ok(())
    }

    fn find(&self, symbol: Symbol) -> MyResult<Symbol> {
        for scope in scopes()[..=self.index].iter().rev() {
            let matching = scope
                .symbols
                .iter()
                .filter(|&s| s == &symbol)
                .collect::<Vec<_>>();
            if matching.len() > 1 {
                warn(format!(
                    "finding {} got multiple matches. choosing first",
                    symbol
                ));
            }
            if let Some(&symbol) = matching.first() {
                return Ok(symbol.clone());
            }
        }
        err(format!("could not find {}", symbol))
    }

    pub fn get_var(&self, name: CachedString) -> MyResult<Symbol> {
        self.find(Symbol::Var {
            ty: Default::default(),
            name,
        })
    }
    pub fn get_func(&self, name: CachedString, arg_types: impl AsRef<[Type]>) -> MyResult<Symbol> {
        self.find(Symbol::Func {
            ty: Default::default(),
            name,
            arg_types: arg_types.as_ref().into(),
        })
    }
    pub fn get_type(&self, name: CachedString) -> MyResult<Symbol> {
        self.find(Symbol::Type(Type::Named(name)))
    }
}
impl Drop for ScopeHandle {
    fn drop(&mut self) {
        if self.pop {
            scopes().pop().expect("tried to pop an empty scope stack");
        }
    }
}
impl Scope {
    pub fn init() -> ScopeHandle {
        let mut scope = Self::new(false, None);

        use crate::ty::LiteralType::*;
        use crate::ty::PrimitiveType::*;
        fn funcs<Str: AsRef<str>>(
            scope: &mut ScopeHandle,
            names: impl AsRef<[Str]>,
            num_args: usize,
            types: impl AsRef<[Type]>,
        ) {
            for name in names.as_ref() {
                for ty in types.as_ref() {
                    scope
                        .add(Symbol::Func {
                            ty: *ty,
                            name: name.as_ref().into(),
                            arg_types: std::iter::repeat(*ty).take(num_args).collect(),
                        })
                        .unwrap();
                }
            }
        }
        fn funcs_ret<Str: AsRef<str>>(
            scope: &mut ScopeHandle,
            names: impl AsRef<[Str]>,
            num_args: usize,
            arg_types: impl AsRef<[Type]>,
            ret_type: Type,
        ) {
            for name in names.as_ref() {
                for arg_type in arg_types.as_ref() {
                    scope
                        .add(Symbol::Func {
                            ty: ret_type,
                            name: name.as_ref().into(),
                            arg_types: std::iter::repeat(*arg_type).take(num_args).collect(),
                        })
                        .unwrap();
                }
            }
        }
        let num_types = [
            I8.ty(),
            U8.ty(),
            I16.ty(),
            U16.ty(),
            I32.ty(),
            U32.ty(),
            I64.ty(),
            U64.ty(),
            F32.ty(),
            F64.ty(),
        ];

        funcs(&mut scope, ["+", "-", "*", "/", "%"], 2, num_types);
        funcs(&mut scope, ["-"], 1, num_types);
        funcs_ret(&mut scope, ["<", "<=", ">", ">="], 2, num_types, Bool.ty());
        funcs_ret(&mut scope, ["==", "!="], 2, [Bool.ty()], Bool.ty());
        funcs_ret(&mut scope, ["!"], 1, [Bool.ty()], Bool.ty());

        for ty in &num_types {
            funcs_ret(&mut scope, [format!("as {}", ty)], 1, num_types, *ty);
            funcs_ret(
                &mut scope,
                [format!("as {}", ty)],
                1,
                [Int.ty(), Float.ty()],
                *ty,
            );
        }

        scope
    }

    #[allow(clippy::new_ret_no_self)]
    pub fn new(in_loop: bool, func_return_type: Option<Type>) -> ScopeHandle {
        let mut scopes = scopes();
        let index = scopes.len();
        scopes.push(Self {
            in_loop,
            func_return_type,
            symbols: Default::default(),
        });
        ScopeHandle { index, pop: true }
    }

    pub fn current() -> ScopeHandle {
        ScopeHandle {
            index: scopes().len() - 1,
            pop: false,
        }
    }
}

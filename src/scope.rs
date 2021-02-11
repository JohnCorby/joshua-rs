//! scope stack contains scopes
//! scopes contain symbols
//! symbols allow us to check for existence and type of stuff we define

use crate::cached::CachedString;
use crate::error::{err, MyResult};
use crate::span::Span;
use crate::ty::TypeKind;
use parking_lot::{Mutex, MutexGuard};
use std::collections::{HashMap, HashSet};
use std::fmt::{Debug, Display, Formatter};
use std::hash::{Hash, Hasher};

static SCOPES: Mutex<Vec<Scope>> = Mutex::new(Vec::new());
fn scopes() -> MutexGuard<'static, Vec<Scope>> {
    SCOPES.try_lock().expect("SCOPES locked")
}

#[derive(Debug)]
pub struct Scope {
    in_loop: bool,
    func_return_type: Option<TypeKind>,
    symbols: HashSet<Symbol>,
}

#[derive(Debug, Clone)]
pub enum Symbol {
    Func {
        ty: TypeKind,
        name: CachedString,
        arg_types: Vec<TypeKind>,
    },
    Var {
        ty: TypeKind,
        name: CachedString,
    },
    Struct {
        name: CachedString,
        field_types: HashMap<CachedString, TypeKind>,
    },
}
impl Symbol {
    pub fn ty(&self) -> TypeKind {
        use Symbol::*;
        match self {
            Func { ty, .. } => *ty,
            Var { ty, .. } => *ty,
            Struct { name, .. } => TypeKind::Struct(*name),
        }
    }
}

/// note: eq contains cases that hash doesnt cover, check both when comparing
impl Hash for Symbol {
    fn hash<H: Hasher>(&self, state: &mut H) {
        use Symbol::*;
        match self {
            Func {
                name, arg_types, ..
            } => {
                name.hash(state);
                arg_types.hash(state);
            }
            Var { name, .. } => name.hash(state),
            Struct { name, .. } => name.hash(state),
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

            _ => false,
        }
    }
}
impl Eq for Symbol {}

impl Display for Symbol {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        use Symbol::*;
        match self {
            Func {
                name, arg_types, ..
            } => write!(
                f,
                "func `{}` with arg types ({})",
                name,
                arg_types
                    .iter()
                    .map(ToString::to_string)
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            Var { name, .. } => write!(f, "var `{}`", name),
            Struct { name, .. } => write!(f, "struct `{}`", name),
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
    pub fn func_return_type(&self) -> TypeKind {
        for scope in scopes()[..=self.index].iter().rev() {
            if let Some(ty) = scope.func_return_type {
                return ty;
            }
        }
        unreachable!("tried to get func return type when we arent in any func")
    }

    pub fn add(&mut self, symbol: Symbol, span: impl Into<Option<Span>>) -> MyResult<()> {
        let symbols = &mut scopes()[self.index].symbols;
        let err = err(format!("{} already defined", symbol), span);
        // find with hash first
        if symbols.contains(&symbol) {
            return err;
        }
        // then use eq if hash didnt find anything
        if symbols.iter().any(|s| s == &symbol) {
            return err;
        }
        symbols.insert(symbol);
        Ok(())
    }

    fn find(&self, symbol: Symbol, span: impl Into<Option<Span>>) -> MyResult<Symbol> {
        for scope in scopes()[..=self.index].iter().rev() {
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

    pub fn get_var(&self, name: CachedString, span: impl Into<Option<Span>>) -> MyResult<Symbol> {
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
        name: CachedString,
        arg_types: impl AsRef<[TypeKind]>,
        span: impl Into<Option<Span>>,
    ) -> MyResult<Symbol> {
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
        name: CachedString,
        span: impl Into<Option<Span>>,
    ) -> MyResult<Symbol> {
        self.find(
            Symbol::Struct {
                name,
                field_types: Default::default(),
            },
            span,
        )
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
            types: impl AsRef<[TypeKind]>,
        ) {
            for name in names.as_ref() {
                for ty in types.as_ref() {
                    scope
                        .add(
                            Symbol::Func {
                                ty: *ty,
                                name: name.as_ref().into(),
                                arg_types: std::iter::repeat(*ty).take(num_args).collect(),
                            },
                            None,
                        )
                        .unwrap();
                }
            }
        }
        fn funcs_ret<Str: AsRef<str>>(
            scope: &mut ScopeHandle,
            names: impl AsRef<[Str]>,
            num_args: usize,
            arg_types: impl AsRef<[TypeKind]>,
            ret_type: TypeKind,
        ) {
            for name in names.as_ref() {
                for arg_type in arg_types.as_ref() {
                    scope
                        .add(
                            Symbol::Func {
                                ty: ret_type,
                                name: name.as_ref().into(),
                                arg_types: std::iter::repeat(*arg_type).take(num_args).collect(),
                            },
                            None,
                        )
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
    pub fn new(in_loop: bool, func_return_type: impl Into<Option<TypeKind>>) -> ScopeHandle {
        let mut scopes = scopes();
        let index = scopes.len();
        scopes.push(Self {
            in_loop,
            func_return_type: func_return_type.into(),
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

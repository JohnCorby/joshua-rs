//! scope stack contains scopes
//! scopes contain symbols
//! symbols allow us to check for existence and type of stuff we define

use crate::cached::CachedString;
use crate::define::Define;
use crate::error::{err, Res};
use crate::parse::{Kind, Node};
use crate::span::Span;
use crate::ty::{PrimitiveType, TypeKind};
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
    return_called: bool,

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

    /// fixme: this doesnt account for branches and stuff (e.g. `if(false) return` works)
    pub fn return_called(&self) {
        for scope in scopes()[..=self.index].iter_mut().rev() {
            // find the scope that is a function
            if scope.func_return_type.is_some() {
                scope.return_called = true;
                return;
            }
        }
    }
    /// note: only checks one current scope and outer ones
    pub fn check_return_called(&self, span: impl Into<Option<Span>>) -> Res<()> {
        let return_called = scopes()[self.index].return_called;
        let is_void = self.func_return_type() == TypeKind::Primitive(PrimitiveType::Void);

        if !return_called && !is_void {
            err("return was never called for non-void func", span)
        } else {
            Ok(())
        }
    }

    pub fn add(&mut self, symbol: Symbol, span: impl Into<Option<Span>>) -> Res<()> {
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

    fn find(&self, symbol: Symbol, span: impl Into<Option<Span>>) -> Res<Symbol> {
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

    pub fn get_var(&self, name: CachedString, span: impl Into<Option<Span>>) -> Res<Symbol> {
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
    ) -> Res<Symbol> {
        self.find(
            Symbol::Func {
                ty: Default::default(),
                name,
                arg_types: arg_types.as_ref().into(),
            },
            span,
        )
    }
    pub fn get_struct(&self, name: CachedString, span: impl Into<Option<Span>>) -> Res<Symbol> {
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
    pub fn init(c_code: &mut String) -> ScopeHandle {
        let scope = Self::new(false, None);

        fn make_func(c_code: &mut String, code: impl AsRef<str>) {
            Node::parse(code.as_ref(), Kind::func_define)
                .unwrap()
                .visit::<Define>()
                .gen(c_code)
                .unwrap();
            c_code.push('\n');
        }

        fn op_funcs<Str: AsRef<str>>(
            c_code: &mut String,
            ops: impl AsRef<[Str]>,
            num_args: usize,
            arg_tys: impl AsRef<[PrimitiveType]>,
            ret_tys: impl Into<Option<PrimitiveType>> + Copy,
        ) {
            for op in ops.as_ref() {
                let op = op.as_ref();
                for &arg_ty in arg_tys.as_ref() {
                    let ret_ty = ret_tys.into().unwrap_or(arg_ty);
                    make_func(
                        c_code,
                        match num_args {
                            1 => format!(
                                "{} `{}`({} a) return <{{ {} ${{ a }} }}>",
                                ret_ty, op, arg_ty, op
                            ),
                            2 => format!(
                                "{} `{}`({} a, {} b) return <{{ ${{ a }} {} ${{ b }} }}>",
                                ret_ty, op, arg_ty, arg_ty, op
                            ),
                            _ => unreachable!(),
                        },
                    );
                }
            }
        }
        use crate::ty::PrimitiveType::*;
        let num_prims = [I8, U8, I16, U16, I32, U32, I64, U64, F32, F64];

        // binary
        op_funcs(c_code, ["+", "-", "*", "/"], 2, num_prims, None);
        op_funcs(
            c_code,
            ["%"],
            2,
            [I8, U8, I16, U16, I32, U32, I64, U64],
            None,
        );
        op_funcs(c_code, ["<", "<=", ">", ">="], 2, num_prims, Bool);
        op_funcs(c_code, ["==", "!="], 2, [Bool], Bool);

        // unary
        op_funcs(c_code, ["-"], 1, num_prims, None);
        op_funcs(c_code, ["!"], 1, [Bool], Bool);

        // cast
        for &ret_ty in &num_prims {
            for &arg_ty in &num_prims {
                make_func(
                    c_code,
                    format!(
                        "{} `as {}`({} a) return <{{ ({}) ${{ a }} }}>",
                        ret_ty,
                        ret_ty,
                        arg_ty,
                        ret_ty.c_type()
                    ),
                );
            }
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
            return_called: false,

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

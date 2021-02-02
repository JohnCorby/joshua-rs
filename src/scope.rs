//! scope stack contains scopes
//! scopes contain symbols
//! symbols allow us to check for existence and type of stuff we define

use crate::error::MyResult;
use crate::ty::{HasType, PrimitiveType, Type};
use parking_lot::{Mutex, MutexGuard};

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
        name: String,
        arg_types: Vec<Type>,
    },
    Var {
        ty: Type,
        name: String,
    },
    Type(Type),
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
            if let Some(ty) = &scope.func_return_type {
                return ty.clone();
            }
        }
        unreachable!("tried to get func return type when we arent in any func")
    }

    pub fn add(&mut self, symbol: Symbol) -> MyResult<()> {
        let symbols = &mut scopes()[self.index].symbols;
        if symbols.contains(&symbol) {
            return Err(format!("{} already defined", symbol.to_string()).into());
        }
        symbols.push(symbol);
        Ok(())
    }

    fn find(&self, symbol: Symbol) -> MyResult<Symbol> {
        for scope in scopes()[..=self.index].iter().rev() {
            if let Some(symbol) = scope.symbols.iter().find(|s| s == &&symbol) {
                return Ok(symbol.clone());
            }
        }
        Err(format!("could not find {}", symbol.to_string()).into())
    }

    pub fn get_var(&self, name: impl AsRef<str>) -> MyResult<Symbol> {
        self.find(Symbol::Var {
            ty: Default::default(),
            name: name.as_ref().into(),
        })
    }
    pub fn get_func(
        &self,
        name: impl AsRef<str>,
        arg_types: impl AsRef<[Type]>,
    ) -> MyResult<Symbol> {
        self.find(Symbol::Func {
            ty: Default::default(),
            name: name.as_ref().into(),
            arg_types: arg_types.as_ref().into(),
        })
    }
    pub fn get_type(&self, name: impl AsRef<str>) -> MyResult<Symbol> {
        self.find(Symbol::Type(Type::Named {
            pos: Default::default(),
            name: name.as_ref().into(),
        }))
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

        // for op type checking
        use PrimitiveType::*;
        fn op_funcs<Str: AsRef<str>, Ht: HasType>(
            scope: &mut ScopeHandle,
            ops: impl AsRef<[Str]>,
            num_args: usize,
            hts: impl AsRef<[Ht]>,
        ) {
            for op in ops.as_ref() {
                for ht in hts.as_ref() {
                    let ty = ht.ty();
                    scope
                        .add(Symbol::Func {
                            ty: ty.clone(),
                            name: op.as_ref().into(),
                            arg_types: std::iter::repeat(ty).take(num_args).collect(),
                        })
                        .unwrap();
                }
            }
        }
        fn bool_op_funcs<Str: AsRef<str>, Ht: HasType>(
            scope: &mut ScopeHandle,
            ops: impl AsRef<[Str]>,
            num_args: usize,
            hts: impl AsRef<[Ht]>,
        ) {
            for op in ops.as_ref() {
                for ht in hts.as_ref() {
                    let ty = ht.ty();
                    scope
                        .add(Symbol::Func {
                            ty: Bool.ty(),
                            name: op.as_ref().into(),
                            arg_types: std::iter::repeat(ty).take(num_args).collect(),
                        })
                        .unwrap();
                }
            }
        }
        op_funcs(
            &mut scope,
            ["+", "-", "*", "/", "%"],
            2,
            [I8, U8, I16, U16, I32, U32, I64, U64, F32, F64],
        );
        op_funcs(
            &mut scope,
            ["-"],
            1,
            [I8, U8, I16, U16, I32, U32, I64, U64, F32, F64],
        );
        bool_op_funcs(
            &mut scope,
            ["<", "<=", ">", ">="],
            2,
            [I8, U8, I16, U16, I32, U32, I64, U64, F32, F64],
        );
        bool_op_funcs(&mut scope, ["==", "!="], 2, [Bool]);
        bool_op_funcs(&mut scope, ["!"], 1, [Bool]);

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

//! scope stack contains scopes
//! scopes contain symbols
//! symbols allow us to check for existence and type of stuff we define

use crate::context::Ctx;
use crate::define::Define;
use crate::error::{err, Res};
use crate::interned_string::InternedStr;
use crate::parse::{Kind, Node};
use crate::span::Span;
use crate::ty::{PrimitiveType, Type};
use std::collections::{HashMap, HashSet};
use std::fmt::{Debug, Display, Formatter};
use std::hash::{Hash, Hasher};

#[derive(Debug, Clone)]
pub enum Symbol<'i> {
    Func {
        ty: Type<'i>,
        name: InternedStr<String>,
        arg_types: Vec<Type<'i>>,
    },
    Var {
        ty: Type<'i>,
        name: InternedStr<&'i str>,
    },
    Struct {
        name: InternedStr<&'i str>,
        field_types: HashMap<InternedStr<&'i str>, Type<'i>>,
    },
}
impl<'i> Symbol<'i> {
    pub fn ty(&self) -> Type<'i> {
        use Symbol::*;
        match self {
            Func { ty, .. } => *ty,
            Var { ty, .. } => *ty,
            Struct { name, .. } => Type::Struct(*name),
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
            } => {
                name.hash(state);
                arg_types.hash(state);
            }
            Var { name, .. } => name.hash(state),
            Struct { name, .. } => name.hash(state),
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
            ) => name1 == name2 && arg_types1 == arg_types2,
            (Var { name: name1, .. }, Var { name: name2, .. }) => name1 == name2,
            (Struct { name: name1, .. }, Struct { name: name2, .. }) => name1 == name2,

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
                "func `{}` with arg types ({})",
                name,
                arg_types
                    .iter()
                    .map(Type::to_string)
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            Var { name, .. } => write!(f, "var `{}`", name),
            Struct { name, .. } => write!(f, "struct `{}`", name),
        }
    }
}

#[derive(Debug, Default)]
pub struct Scopes<'i>(Vec<Scope<'i>>);
impl<'i> Ctx<'i> {
    pub fn init_scopes(&mut self) {
        self.scopes.push(false, None);

        trait Ext<'i> {
            fn make_func(&mut self, i: impl AsRef<str>);

            fn op_funcs<Str: AsRef<str>>(
                &mut self,
                ops: impl AsRef<[Str]>,
                num_args: usize,
                arg_tys: impl AsRef<[PrimitiveType]>,
                ret_tys: impl Into<Option<PrimitiveType>> + Copy,
            );
        }
        impl<'i> Ext<'i> for Ctx<'i> {
            #[allow(warnings)]
            fn make_func(&mut self, i: impl AsRef<str>) {
                Node::parse(todo!("make_func"), Kind::func_define)
                    .unwrap()
                    .visit::<Define<'i>>(self)
                    .gen(self)
                    .unwrap();
                self.o.push('\n');
            }

            fn op_funcs<Str: AsRef<str>>(
                &mut self,
                ops: impl AsRef<[Str]>,
                num_args: usize,
                arg_tys: impl AsRef<[PrimitiveType]>,
                ret_tys: impl Into<Option<PrimitiveType>> + Copy,
            ) {
                for op in ops.as_ref() {
                    let op = op.as_ref();
                    for &arg_ty in arg_tys.as_ref() {
                        let ret_ty = ret_tys.into().unwrap_or(arg_ty);
                        self.make_func(match num_args {
                            1 => format!(
                                "{} `{}`({} a) return <{{ {} ${{ a }} }}>",
                                ret_ty, op, arg_ty, op
                            ),
                            2 => format!(
                                "{} `{}`({} a, {} b) return <{{ ${{ a }} {} ${{ b }} }}>",
                                ret_ty, op, arg_ty, arg_ty, op
                            ),
                            _ => unreachable!(),
                        });
                    }
                }
            }
        }

        use crate::ty::PrimitiveType::*;
        let num_prims = [I8, U8, I16, U16, I32, U32, I64, U64, F32, F64];

        // binary
        self.op_funcs(["+", "-", "*", "/"], 2, num_prims, None);
        self.op_funcs(["%"], 2, [I8, U8, I16, U16, I32, U32, I64, U64], None);
        self.op_funcs(["<", "<=", ">", ">="], 2, num_prims, Bool);
        self.op_funcs(["==", "!="], 2, [Bool], Bool);

        // unary
        self.op_funcs(["-"], 1, num_prims, None);
        self.op_funcs(["!"], 1, [Bool], Bool);

        // cast
        for &ret_ty in &num_prims {
            for &arg_ty in &num_prims {
                self.make_func(format!(
                    "{} `as {}`({} a) return <{{ ({}) ${{ a }} }}>",
                    ret_ty,
                    ret_ty,
                    arg_ty,
                    ret_ty.c_type()
                ));
            }
        }
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
        let symbols = &mut self.0.last_mut().unwrap().symbols;
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
        name: InternedStr<&'i str>,
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
        name: InternedStr<String>,
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
        name: InternedStr<&'i str>,
        span: impl Into<Option<Span<'i>>>,
    ) -> Res<'i, Symbol<'i>> {
        self.find(
            Symbol::Struct {
                name,
                field_types: Default::default(),
            },
            span,
        )
    }
}

//! scope stack contains scopes
//! scopes contain symbols
//! symbols allow us to check for existence and type of stuff we define

use crate::error::{err, Res};
use crate::pass::ast::{Block, TypeNode};
use crate::pass::ast::{Define, VarDefine};
use crate::pass::ty::{PrimitiveType, Type};
use crate::span::Span;
use crate::util::ctx_str::CtxStr;
use crate::util::{IterExt, StrExt};
use std::collections::{HashMap, HashSet};
use std::fmt::{Debug, Display, Formatter};
use std::ops::Deref;
use std::rc::Rc;

#[allow(clippy::too_many_arguments)]
#[derive(Debug, Clone, derivative::Derivative, derive_new::new)]
#[derivative(Hash, PartialEq)]
pub enum Symbol<'i> {
    Func {
        #[derivative(Hash = "ignore", PartialEq = "ignore")]
        #[new(default)]
        ty: Type<'i>,
        #[derivative(Hash = "ignore", PartialEq = "ignore")]
        #[new(default)]
        nesting_prefix: Rc<Vec<CtxStr<'i>>>,
        name: CtxStr<'i>,
        generic_replacements: Rc<Vec<Type<'i>>>,
        arg_types: Rc<Vec<Type<'i>>>,
    },
    Var {
        #[derivative(Hash = "ignore", PartialEq = "ignore")]
        #[new(default)]
        ty: Type<'i>,
        name: CtxStr<'i>,
    },
    StructType {
        #[derivative(Hash = "ignore", PartialEq = "ignore")]
        #[new(default)]
        nesting_prefix: Rc<Vec<CtxStr<'i>>>,
        name: CtxStr<'i>,
        generic_replacements: Rc<Vec<Type<'i>>>,
        #[derivative(Hash = "ignore", PartialEq = "ignore")]
        #[new(default)]
        field_types: Rc<HashMap<CtxStr<'i>, Type<'i>>>,
    },
    GenericPlaceholderType(CtxStr<'i>),
    GenericFunc {
        // cached for faster access
        #[derivative(Hash = "ignore", PartialEq = "ignore")]
        ty: Type<'i>,
        arg_types: Rc<Vec<Type<'i>>>,

        // copied from func define
        #[derivative(Hash = "ignore", PartialEq = "ignore")]
        span: Span<'i>,
        #[derivative(Hash = "ignore", PartialEq = "ignore")]
        ty_node: TypeNode<'i>,
        #[derivative(Hash = "ignore", PartialEq = "ignore")]
        nesting_prefix: Rc<Vec<CtxStr<'i>>>,
        name: CtxStr<'i>,
        #[derivative(Hash = "ignore", PartialEq = "ignore")]
        generic_placeholders: Rc<Vec<CtxStr<'i>>>,
        #[derivative(Hash = "ignore", PartialEq = "ignore")]
        args: Rc<Vec<VarDefine<'i>>>,
        #[derivative(Hash = "ignore", PartialEq = "ignore")]
        body: Block<'i>,

        // codegen info
        #[derivative(Hash = "ignore", PartialEq = "ignore")]
        scopes_index: usize,
    },
    GenericStruct {
        // cached for faster access
        #[derivative(Hash = "ignore", PartialEq = "ignore")]
        ty: Type<'i>,

        // copied from struct define
        #[derivative(Hash = "ignore", PartialEq = "ignore")]
        span: Span<'i>,
        #[derivative(Hash = "ignore", PartialEq = "ignore")]
        nesting_prefix: Rc<Vec<CtxStr<'i>>>,
        name: CtxStr<'i>,
        #[derivative(Hash = "ignore", PartialEq = "ignore")]
        generic_placeholders: Rc<Vec<CtxStr<'i>>>,
        #[derivative(Hash = "ignore", PartialEq = "ignore")]
        body: Rc<Vec<Define<'i>>>,

        // codegen info
        #[derivative(Hash = "ignore", PartialEq = "ignore")]
        scopes_index: usize,
    },
}
impl Symbol<'i> {
    pub fn ty(&self) -> Type<'i> {
        use Symbol::*;
        match self {
            Func { ty, .. }
            | Var { ty, .. }
            | GenericFunc { ty, .. }
            | GenericStruct { ty, .. } => ty.deref().clone(),
            StructType {
                name,
                generic_replacements,
                ..
            } => Type::Struct {
                name: *name,
                generic_replacements: generic_replacements.clone(),
            },
            GenericPlaceholderType(name) => Type::GenericPlaceholder(*name),
        }
    }
}
impl Eq for Symbol<'_> {}

impl Display for Symbol<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        use Symbol::*;
        match self {
            Func {
                name,
                generic_replacements,
                arg_types,
                ..
            } => f.write_str(&name.to_display(
                "func symbol",
                &generic_replacements.iter().vec(),
                Some(&arg_types.iter().vec()),
            )),
            Var { name, .. } => f.write_str(&name.to_display("var symbol", &[], None)),
            StructType {
                name,
                generic_replacements,
                ..
            } => f.write_str(&name.to_display(
                "struct type symbol",
                &generic_replacements.iter().vec(),
                None,
            )),
            // _ => write!(f, "internal symbol {:?}", self),
            _ => panic!("internal symbol {:?} should not be displayed", self),
        }
    }
}

#[derive(Debug, Default)]
pub struct Scopes<'i>(pub Vec<Scope<'i>>);

impl Scopes<'i> {
    pub fn push(
        &mut self,
        name: Option<CtxStr<'i>>,
        is_loop: bool,
        func_return_type: Option<Type<'i>>,
    ) {
        self.0.push(Scope::new(name, is_loop, func_return_type))
    }

    pub fn pop(&mut self) {
        self.0.pop().expect("tried to pop an empty scope stack");
    }
}

#[derive(Debug, derive_new::new)]
pub struct Scope<'i> {
    name: Option<CtxStr<'i>>, // todo give name to ALL blocks, not just funcs and structs
    is_loop: bool,
    func_return_type: Option<Type<'i>>,
    #[new(default)]
    return_called: bool,

    #[new(default)]
    pub symbols: HashSet<Symbol<'i>>,
}

impl Scopes<'i> {
    /// use for initializing ast nodes
    pub fn nesting_prefix(&self) -> Vec<CtxStr<'i>> {
        self.0.iter().rev().filter_map(|scope| scope.name).collect()
    }

    pub fn in_loop(&self) -> bool {
        for scope in self.0.iter().rev() {
            // accounts for inner functions
            if scope.func_return_type.is_some() {
                return false;
            }
            if scope.is_loop {
                return true;
            }
        }
        false
    }
    pub fn func_return_type(&self) -> &Type<'i> {
        for scope in self.0.iter().rev() {
            if let Some(ty) = &scope.func_return_type {
                return ty;
            }
        }
        unreachable!("getting func return type when we aren't in any func")
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
    pub fn check_return_called(&self, span: Option<Span<'i>>) -> Res<'i> {
        let return_called = self.0.last().unwrap().return_called;
        let is_void = self.func_return_type() == &Type::Primitive(PrimitiveType::Void);

        if !return_called && !is_void {
            err("return was never called for non-void func", span)
        } else {
            Ok(())
        }
    }
}

impl Scopes<'i> {
    pub fn add(&mut self, symbol: Symbol<'i>, span: Option<Span<'i>>) -> Res<'i> {
        let scope = self.0.last_mut().unwrap();
        if let Some(symbol) = scope.symbols.get(&symbol) {
            return err(&format!("{} already defined", symbol), span);
        }
        scope.symbols.insert(symbol);
        Ok(())
    }

    /// fixme include prefix name for funcs and structs
    pub fn find(&self, symbol: &Symbol<'i>, span: Option<Span<'i>>) -> Res<'i, &Symbol<'i>> {
        for scope in self.0.iter().rev() {
            if let Some(symbol) = scope.symbols.get(symbol) {
                return Ok(symbol);
            }
        }
        err(&format!("could not find {}", symbol), span)
    }
}

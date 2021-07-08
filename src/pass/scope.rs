//! scope stack contains scopes
//! scopes contain symbols
//! symbols allow us to check for existence and type of stuff we define

use crate::error::{err, Res};
use crate::pass::ast1;
use crate::pass::ast2::Type;
use crate::pass::ty::PrimitiveType;
use crate::span::Span;
use crate::util::{IterExt, StrExt};
use std::collections::{HashMap, HashSet};
use std::fmt::{Debug, Display, Formatter};
use std::ops::Deref;
use std::rc::Rc;

/// NOTE: hash is only simple way to prevent duplicates. extra checking is needed
#[allow(clippy::too_many_arguments)]
#[derive(Debug, Clone, Derivative, new)]
#[derivative(Hash, PartialEq)]
pub enum Symbol {
    Func {
        #[derivative(Hash = "ignore", PartialEq = "ignore")]
        #[new(default)]
        ty: Type,
        #[derivative(Hash = "ignore", PartialEq = "ignore")]
        #[new(default)]
        nesting_prefix: &'static str,
        receiver_ty: Option<Type>,
        name: &'static str,
        generic_replacements: Rc<Vec<Type>>,
        arg_types: Rc<Vec<Type>>,
    },
    Var {
        #[derivative(Hash = "ignore", PartialEq = "ignore")]
        #[new(default)]
        ty: Type,
        name: &'static str,
    },
    Struct {
        #[derivative(Hash = "ignore", PartialEq = "ignore")]
        #[new(default)]
        nesting_prefix: &'static str,
        name: &'static str,
        generic_replacements: Rc<Vec<Type>>,
        #[derivative(Hash = "ignore", PartialEq = "ignore")]
        #[new(default)]
        field_types: Rc<HashMap<&'static str, Type>>,
    },
    GenericPlaceholder(&'static str),
    GenericStruct {
        // copied from struct define
        #[derivative(Hash = "ignore", PartialEq = "ignore")]
        span: Span,
        name: &'static str,
        #[derivative(Hash = "ignore", PartialEq = "ignore")]
        generic_placeholders: Rc<Vec<&'static str>>,
        #[derivative(Hash = "ignore", PartialEq = "ignore")]
        body: Rc<Vec<ast1::Define>>,

        // codegen info
        #[derivative(Hash = "ignore", PartialEq = "ignore")]
        scopes_index: usize,
    },
    GenericFunc {
        // used only for eq/hash
        receiver_ty: Option<Type>,
        arg_types: Rc<Vec<Type>>,

        /// used only for generic inference
        #[derivative(Hash = "ignore", PartialEq = "ignore")]
        ty: Type,

        // copied from func define
        #[derivative(Hash = "ignore", PartialEq = "ignore")]
        span: Span,
        #[derivative(Hash = "ignore", PartialEq = "ignore")]
        ty_ast1: ast1::Type,
        #[derivative(Hash = "ignore", PartialEq = "ignore")]
        receiver_ty_ast1: Option<ast1::Type>,
        name: &'static str,
        #[derivative(Hash = "ignore", PartialEq = "ignore")]
        generic_placeholders: Rc<Vec<&'static str>>,
        #[derivative(Hash = "ignore", PartialEq = "ignore")]
        args: Rc<Vec<ast1::VarDefine>>,
        #[derivative(Hash = "ignore", PartialEq = "ignore")]
        body: ast1::Block,

        // codegen info
        #[derivative(Hash = "ignore", PartialEq = "ignore")]
        scopes_index: usize,
    },
}
impl Symbol {
    pub fn ty(&self) -> Type {
        use Symbol::*;
        match self {
            Func { ty, .. } | Var { ty, .. } => ty.deref().clone(),
            Struct {
                nesting_prefix,
                name,
                generic_replacements,
                ..
            } => Type::Struct {
                nesting_prefix: *nesting_prefix,
                name: *name,
                generic_replacements: generic_replacements.clone(),
            },
            GenericPlaceholder(name) => Type::GenericPlaceholder(*name),
            _ => panic!("symbol {:?} doesn't have a type", self),
        }
    }
}
impl Eq for Symbol {}

impl Display for Symbol {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        use Symbol::*;
        match self {
            Func {
                receiver_ty,
                name,
                generic_replacements,
                arg_types,
                ..
            } => write!(
                f,
                "func {}{}",
                receiver_ty
                    .as_ref()
                    .map_or(String::new(), |it| format!("{}::", it)),
                name.encode(
                    &generic_replacements.iter().vec(),
                    Some(&arg_types.iter().vec()),
                )
            ),
            Var { name, .. } => write!(f, "var {}", name),
            Struct {
                name,
                generic_replacements,
                ..
            } => write!(
                f,
                "struct {}",
                name.encode(&generic_replacements.iter().vec(), None)
            ),
            GenericPlaceholder(name) => write!(f, "generic placeholder {}", name),
            _ => panic!("symbol {:?} shouldn't be displayed or encoded", self),
        }
    }
}

#[derive(Debug, Default)]
pub struct Scopes(pub Vec<Scope>);

impl Scopes {
    pub fn push(&mut self, scope: Scope) {
        self.0.push(scope)
    }

    pub fn pop(&mut self) -> Scope {
        self.0.pop().expect("tried to pop an empty scope stack")
    }
}

#[derive(Debug, new)]
pub struct Scope {
    nesting_name: Option<&'static str>, // todo give name to ALL blocks, not just funcs
    is_loop: bool,
    func_return_type: Option<Type>,
    #[new(default)]
    return_called: bool,

    #[new(default)]
    pub symbols: HashSet<Symbol>,
}

impl Scopes {
    /// use for initializing ast nodes
    pub fn nesting_prefix(&self) -> String {
        self.0
            .iter()
            .rev()
            .filter_map(|scope| scope.nesting_name.map(|it| format!("{}$", it)))
            .collect()
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
    pub fn func_return_type(&self) -> &Type {
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
    pub fn check_return_called(&self, span: Span) -> Res {
        let return_called = self.0.last().unwrap().return_called;
        let is_void = self.func_return_type() == &Type::Primitive(PrimitiveType::Void);

        if !return_called && !is_void {
            err("return was never called for non-void func", span)
        } else {
            Ok(())
        }
    }
}

/// these are simple and just use hash
impl Scopes {
    pub fn add(&mut self, symbol: Symbol, span: Span) -> Res {
        match symbol {
            // Symbol::Struct {
            //     generic_replacements,
            //     ..
            // } if !generic_replacements.is_empty() => todo!("add generic struct"),
            // Symbol::Func {
            //     generic_replacements,
            //     ..
            // } if !generic_replacements.is_empty() => todo!("add generic func"),
            _ => {
                let symbols = &mut self.0.last_mut().unwrap().symbols;
                if let Some(symbol) = symbols.get(&symbol) {
                    return err(&format!("{} already defined", symbol), span);
                }
                symbols.insert(symbol);
                Ok(())
            }
        }
    }

    pub fn find(&mut self, symbol: &Symbol, span: Span) -> Res<Symbol> {
        match symbol {
            Symbol::Struct {
                generic_replacements,
                ..
            } if !generic_replacements.is_empty() => self.find_generic_struct(symbol, span),
            Symbol::Func {
                generic_replacements,
                ..
            } if !generic_replacements.is_empty() => self.find_generic_func(symbol, span),

            _ => {
                for scope in self.0.iter().rev() {
                    if let Some(symbol) = scope.symbols.get(symbol) {
                        return Ok(symbol.clone());
                    }
                }
                err(&format!("could not find {}", symbol), span)
            }
        }
    }
}

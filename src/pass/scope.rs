//! scope stack contains scopes
//! scopes contain symbols
//! symbols allow us to check for existence and type of stuff we define

use crate::context::{Intern, Output};
use crate::error::{err, Res};
use crate::pass::ast2::Type;
use crate::pass::{ast1, Ident, PrimitiveKind};
use crate::span::Span;
use crate::util::{IterExt, StrExt};
use std::collections::{HashMap, HashSet};
use std::fmt::{Debug, Display, Formatter};
use std::rc::Rc;
use std::sync::atomic::{AtomicUsize, Ordering};

/// NOTE: hash is only simple way to prevent duplicates. extra checking is needed
///
/// the spans here are the whole definition on add symbol and the call/reference/whatever on find symbol
#[allow(clippy::too_many_arguments)]
#[derive(Debug, Clone, Derivative, new)]
#[derivative(Hash, PartialEq)]
pub enum Symbol {
    Func {
        /// the func definition and the func call
        #[derivative(Hash = "ignore", PartialEq = "ignore")]
        span: Span,
        #[derivative(Hash = "ignore", PartialEq = "ignore")]
        #[new(default)]
        ty: Type,
        #[derivative(Hash = "ignore", PartialEq = "ignore")]
        #[new(default)]
        nesting_prefix: &'static str,
        receiver_ty: Option<Type>,
        name: Ident,
        generic_replacements: Rc<Vec<Type>>,
        arg_types: Rc<Vec<Type>>,
    },
    Var(
        #[derivative(Hash = "ignore", PartialEq = "ignore")]
        #[new(default)]
        Type,
        Ident,
    ),
    Struct {
        #[derivative(Hash = "ignore", PartialEq = "ignore")]
        #[new(default)]
        nesting_prefix: &'static str,
        name: Ident,
        generic_replacements: Rc<Vec<Type>>,
        #[derivative(Hash = "ignore", PartialEq = "ignore")]
        #[new(default)]
        field_types: Rc<HashMap<Ident, Type>>,
    },
    /// replaced with concrete type on specialization
    ///
    /// this should only ever show up in
    /// generic func receiver type, ret type, or arg types
    GenericPlaceholder(Ident),
    GenericStruct {
        #[derivative(Hash = "ignore", PartialEq = "ignore")]
        span: Span,
        name: Ident,
        #[derivative(Hash = "ignore", PartialEq = "ignore")]
        generic_placeholders: Rc<Vec<Ident>>,
        #[derivative(Hash = "ignore", PartialEq = "ignore")]
        body: Rc<Vec<ast1::Define>>,

        // codegen info
        #[derivative(Hash = "ignore", PartialEq = "ignore")]
        scopes_index: usize,
    },
    GenericFunc {
        #[derivative(Hash = "ignore", PartialEq = "ignore")]
        span: Span,
        #[derivative(Hash = "ignore", PartialEq = "ignore")]
        ty: Type,
        receiver_ty: Option<Type>,
        name: Ident,
        #[derivative(Hash = "ignore", PartialEq = "ignore")]
        generic_placeholders: Rc<Vec<Ident>>,
        #[derivative(Hash = "ignore", PartialEq = "ignore")]
        args: Rc<Vec<ast1::VarDefine>>,
        /// calculated ONCE from args
        arg_types: Rc<Vec<Type>>,
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
            Func { ty, .. } | Var(ty, ..) => ty.clone(),
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

    pub fn name_span(&self) -> Span {
        use Symbol::*;
        match self {
            Func { name, .. } => name,
            Var(.., name) => name,
            Struct { name, .. } => name,
            GenericPlaceholder(name) => name,
            GenericStruct { name, .. } => name,
            GenericFunc { name, .. } => name,
        }
        .0
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
                "func {}",
                name.encode(
                    "",
                    receiver_ty.as_ref(),
                    generic_replacements,
                    Some(arg_types),
                    false
                )
            ),
            Var(.., name) => write!(f, "var {}", name),
            Struct {
                name,
                generic_replacements,
                ..
            } => write!(
                f,
                "struct {}",
                name.encode("", None, generic_replacements, None, false)
            ),
            GenericPlaceholder(name) => {
                write!(f, "generic placeholder {}", name)
            }
            GenericFunc {
                receiver_ty,
                name,
                generic_placeholders,
                arg_types,
                ..
            } => write!(
                f,
                "generic func {}",
                name.encode(
                    "",
                    receiver_ty.as_ref(),
                    &generic_placeholders
                        .iter()
                        .map(|it| Type::GenericPlaceholder(*it))
                        .vec(),
                    Some(arg_types),
                    false
                )
            ),
            GenericStruct {
                name,
                generic_placeholders,
                ..
            } => write!(
                f,
                "generic struct {}",
                name.encode(
                    "",
                    None,
                    &generic_placeholders
                        .iter()
                        .map(|it| Type::GenericPlaceholder(*it))
                        .vec(),
                    None,
                    false
                )
            ),
        }
    }
}

#[derive(Debug)]
pub struct Scopes(pub Vec<Scope>);

impl Scopes {
    pub fn push(&mut self, scope: Scope) {
        self.0.push(scope)
    }

    pub fn pop(&mut self) -> Scope {
        self.0.pop().expect("tried to pop an empty scope stack")
    }
}

#[derive(Debug)]
pub struct Scope {
    /// all blocks that can hold nested funcs and structs have this
    nesting_id: Option<usize>,

    is_loop: bool,
    func_return_type: Option<Type>,
    return_called: bool,

    pub symbols: HashSet<Symbol>,
}
impl Scope {
    pub fn new(has_nesting_id: bool, is_loop: bool, func_return_type: Option<Type>) -> Self {
        Self {
            nesting_id: if has_nesting_id {
                static ID: AtomicUsize = AtomicUsize::new(0);
                Some(ID.fetch_add(1, Ordering::Relaxed))
            } else {
                None
            },

            is_loop,
            func_return_type,
            return_called: false,

            symbols: Default::default(),
        }
    }
}

impl Scopes {
    /// used in gen with nested funcs and structs
    pub fn nesting_prefix(&self) -> &'static str {
        self.0
            .iter()
            .filter_map(|scope| scope.nesting_id.map(|it| format!("{:x}`", it)))
            .collect::<String>()
            .intern()
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
        let is_void = self.func_return_type() == &Type::Primitive(PrimitiveKind::Void);

        if !return_called && !is_void {
            err("return was never called for non-void func", span)
        } else {
            Ok(())
        }
    }
}

impl Scopes {
    pub fn add(&mut self, symbol: Symbol) -> Res {
        let symbols = &mut self.0.last_mut().unwrap().symbols;
        let existing = match &symbol {
            // placeholders always eq any other placeholder
            Symbol::GenericStruct { .. } | Symbol::GenericFunc { .. } => {
                symbols.iter().find(|s| symbol.generic_eq_generic(s))
            }

            _ => symbols.get(&symbol),
        };
        if let Some(existing) = existing {
            err(&format!("{} already defined", existing), symbol.name_span())
        } else {
            symbols.insert(symbol);
            Ok(())
        }
    }

    /// `type_hint` is used for generic func inference
    pub fn find(
        &mut self,
        o: &mut Output,
        symbol: &Symbol,
        type_hint: Option<&Type>,
    ) -> Res<Symbol> {
        match symbol {
            Symbol::Struct {
                generic_replacements,
                ..
            }
            | Symbol::Func {
                generic_replacements,
                ..
            } if !generic_replacements.is_empty() => return self.find_generic(o, symbol),
            _ => {}
        };

        for scope in self.0.iter().rev() {
            if let Some(existing) = scope.symbols.get(symbol) {
                return Ok(existing.clone());
            }
        }
        match symbol {
            Symbol::Func {
                generic_replacements,
                ..
            } if generic_replacements.is_empty() => {
                return self.find_generic_func_inference(o, symbol, type_hint)
            }
            _ => {}
        }

        err(&format!("could not find {}", symbol), symbol.name_span())
    }
}

//! post-type-checked

use crate::error::{err, Res};
use crate::pass::{Ident, Literal, PrimitiveKind};
use crate::span::Span;
use crate::util::StrExt;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct Program(pub Rc<Vec<Define>>);

#[derive(Debug, Clone)]
pub enum Define {
    Struct {
        span: Span,
        nesting_prefix: &'static str,
        name: Ident,
        generic_replacements: Rc<Vec<Type>>,
        body: Rc<Vec<Define>>,
    },
    Func {
        span: Span,
        ty: Type,
        nesting_prefix: &'static str,
        receiver_ty: Option<Type>,
        name: Ident,
        generic_replacements: Rc<Vec<Type>>,
        args: Rc<Vec<VarDefine>>,
        body: Block,
    },
    Var(VarDefine),

    CCode(CCode),

    /// used for generic template that should not generate anything
    NoGen,
}

#[derive(Debug, Clone)]
pub struct VarDefine {
    pub span: Span,
    pub ty: Type,
    pub name: Ident,
    pub value: Option<Expr>,

    /// used for gen. kinda hacky, oh well
    pub is_global: bool,
}

#[derive(Debug, Clone)]
pub enum Statement {
    Return(Option<Expr>),
    Break,
    Continue,
    If {
        cond: Expr,
        then: Block,
        otherwise: Option<Block>,
    },
    Until {
        cond: Expr,
        block: Block,
    },
    For {
        init: VarDefine,
        cond: Expr,
        update: Rc<Statement>,
        block: Block,
    },
    ExprAssign {
        lvalue: Expr,
        rvalue: Expr,
    },
    Define(Define),
    Expr(Expr),
}

#[derive(Debug, Clone)]
pub struct Block(pub Rc<Vec<Statement>>);

#[derive(Debug, Clone)]
pub struct CCode(pub Rc<Vec<CCodePart>>);

#[derive(Debug, Clone)]
pub enum CCodePart {
    String(&'static str),
    Expr(Expr),
}

#[derive(Debug, Clone)]
pub enum Expr {
    Cast {
        span: Span,
        nesting_prefix: &'static str,
        thing: Rc<Expr>,
        ty: Type,
    },

    Field {
        span: Span,
        receiver: Rc<Expr>,
        var: &'static str,
        ty: Type,
    },

    // primary
    Literal {
        span: Span,
        value: Literal,
        ty: Type,
    },
    FuncCall {
        span: Span,
        nesting_prefix: &'static str,
        receiver_ty: Option<Type>,
        name: &'static str,
        generic_replacements: Rc<Vec<Type>>,
        args: Rc<Vec<Expr>>,
        ty: Type,
    },
    Var {
        name: Ident,
        ty: Type,
    },

    CCode(CCode),
}

impl Expr {
    pub fn ty(&self) -> Type {
        use Expr::*;
        match &self {
            Cast { ty, .. } => ty,
            Field { ty, .. } => ty,
            Literal { ty, .. } => ty,
            FuncCall { ty, .. } => ty,
            Var { ty, .. } => ty,
            CCode(_) => unimplemented!(),
        }
    }

    pub fn check_assignable(&self) -> Res {
        use Expr::*;
        let is_ptr = matches!(self.ty(), TypeKind::Ptr(_));
        let is_assignable = match self.kind {
            Var { name: _ } | Field { .. } => true,
            FuncCall { .. } if is_ptr => true,
            _ => false,
        };

        if !is_assignable {
            err("expr is not assignable", span)
        } else {
            Ok(())
        }
    }
}

impl Literal {
    /// fixme remove this
    pub fn ty(&self, span: Span) -> Type {
        use Literal::*;
        match self {
            Float(_) => LiteralKind::Float.ty(span),
            Int(_) => LiteralKind::Int.ty(span),
            Bool(_) => PrimitiveKind::Bool.ty(span),
            Char(_) => PrimitiveKind::U8.ty(span),
            StrZ(_) => Type::Ptr(PrimitiveKind::U8.ty(span).into()),
        }
    }
}

/// NOTE: hash is only simple way to prevent duplicates. extra checking is needed
#[derive(Debug, Clone, Derivative)]
#[derivative(Hash, PartialEq)]
pub enum Type {
    Primitive(PrimitiveKind),
    /// type version of the symbol
    Struct {
        #[derivative(Hash = "ignore", PartialEq = "ignore")]
        span: Span,
        #[derivative(Hash = "ignore", PartialEq = "ignore")]
        nesting_prefix: &'static str,
        name: Ident,
        generic_replacements: Rc<Vec<Type>>,
    },
    Ptr(Rc<Type>),

    /// fixme merge these into generics when we get type inference
    Literal(LiteralKind),
    /// type version of the symbol
    GenericPlaceholder(Ident),
    /// for inferring with var define and probably other stuff later
    Auto,
    CCode,
}
impl Eq for Type {}

impl Type {
    pub fn check(&self, expected: &Self, span: Span) -> Res {
        let actual = self;
        if expected == actual {
            Ok(())
        } else {
            err(
                &format!(
                    "expected {}, but got {}",
                    expected.encode(false),
                    actual.encode(false)
                ),
                span,
            )
        }
    }
}
impl Default for Type {
    fn default() -> Self {
        Self {
            span: Default::default(),
            kind: TypeKind::Primitive(PrimitiveKind::Void),
        }
    }
}

impl Type {
    /// used for codegen and display
    pub fn encode(&self, include_nesting_prefixes: bool) -> String {
        use Type::*;
        match &self {
            Primitive(kind) => kind.to_string(),
            Struct {
                nesting_prefix,
                name,
                generic_replacements,
                ..
            } => name.encode(
                nesting_prefix,
                None,
                generic_replacements,
                None,
                include_nesting_prefixes,
            ),
            Ptr(ty) => format!("ptr<{}>", ty.encode(include_nesting_prefixes)),
            GenericPlaceholder(name) => name.to_string(),
            _ => panic!("type {:?} shouldn't be displayed or encoded", self),
        }
    }
}

#[derive(Debug, Copy, Clone, Hash, PartialEq)]
pub enum LiteralKind {
    Float,
    Int,
}
impl LiteralKind {
    pub const fn ty(&self, span: Span) -> Type {
        Type {
            span,
            kind: TypeKind::Literal(*self),
        }
    }
}

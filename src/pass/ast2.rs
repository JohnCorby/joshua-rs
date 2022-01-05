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
    Return(Span, Option<Expr>),
    Break(Span),
    Continue(Span),
    If {
        span: Span,
        cond: Expr,
        then: Block,
        otherwise: Option<Block>,
    },
    Until {
        span: Span,
        cond: Expr,
        block: Block,
    },
    For {
        span: Span,
        init: VarDefine,
        cond: Expr,
        update: Rc<Statement>,
        block: Block,
    },
    ExprAssign {
        span: Span,
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
        name: Ident,
        ty: Type,
    },

    // primary
    Literal(Literal),
    FuncCall {
        span: Span,
        nesting_prefix: &'static str,
        receiver_ty: Option<Type>,
        name: Ident,
        generic_replacements: Rc<Vec<Type>>,
        args: Rc<Vec<Expr>>,
        ty: Type,
    },
    Var(Ident, Type),

    CCode(Span, CCode),
}

impl Expr {
    pub fn ty(&self) -> &Type {
        use Expr::*;
        match self {
            Cast { ty, .. } => ty,
            Field { ty, .. } => ty,
            Literal(literal) => match literal {
                self::Literal::Float(..) => &Type::Literal(LiteralKind::Float),
                self::Literal::Int(..) => &Type::Literal(LiteralKind::Int),
                self::Literal::Bool(..) => &Type::Primitive(PrimitiveKind::Bool),
                self::Literal::Char(..) => &Type::Primitive(PrimitiveKind::U8),
                self::Literal::StrZ(..) => &Type::Ptr(Type::Primitive(PrimitiveKind::U8).into()),
            },
            FuncCall { ty, .. } => ty,
            Var(.., ty) => ty,
            CCode(..) => &Type::CCode,
        }
    }

    pub fn span(&self) -> Span {
        use Expr::*;
        match self {
            Cast { span, .. } => *span,
            Field { span, .. } => *span,
            Literal(literal) => match literal {
                self::Literal::Float(span, ..) => *span,
                self::Literal::Int(span, ..) => *span,
                self::Literal::Bool(span, ..) => *span,
                self::Literal::Char(span, ..) => *span,
                self::Literal::StrZ(span, ..) => *span,
            },
            FuncCall { span, .. } => *span,
            Var(name, _) => name.0,
            CCode(span, ..) => *span,
        }
    }

    pub fn check_assignable(&self) -> Res {
        use Expr::*;
        let is_ptr = matches!(self.ty(), Type::Ptr(_));
        let is_assignable = match self {
            Var(_, _) | Field { .. } => true,
            FuncCall { .. } if is_ptr => true,
            _ => false,
        };

        if !is_assignable {
            err("expr is not assignable", self.span())
        } else {
            Ok(())
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
        Self::Primitive(PrimitiveKind::Void)
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

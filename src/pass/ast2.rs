//! post-type-checked

use crate::error::{err, Res};
use crate::pass::ast1::GenericPlaceholder;
use crate::pass::ty::{LiteralType, PrimitiveType};
use crate::span::Span;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct Program(pub Rc<Vec<Define>>);

#[derive(Debug, Clone)]
pub struct Define {
    pub span: Span,
    pub kind: DefineKind,
}

#[derive(Debug, Clone)]
pub enum DefineKind {
    Struct {
        nesting_prefix: &'static str,
        name: &'static str,
        generic_replacements: Rc<Vec<Type>>,
        body: Rc<Vec<Define>>,
    },
    Func {
        ty: Type,
        nesting_prefix: &'static str,
        receiver_ty: Option<Type>,
        name: &'static str,
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
    pub name: &'static str,
    pub value: Option<Expr>,

    /// used for gen. kinda hacky, oh well
    pub is_global: bool,
}

#[derive(Debug, Clone)]
pub struct Statement {
    pub span: Span,
    pub kind: StatementKind,
}

#[derive(Debug, Clone)]
pub enum StatementKind {
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
pub struct Expr {
    pub span: Span,
    pub kind: ExprKind,
    pub ty: Type,
}

#[derive(Debug, Clone)]
pub enum ExprKind {
    Cast {
        nesting_prefix: &'static str,
        thing: Rc<Expr>,
    },

    Field {
        receiver: Rc<Expr>,
        var: &'static str,
    },

    // primary
    Literal(Literal),
    FuncCall {
        nesting_prefix: &'static str,
        receiver_ty: Option<Type>,
        name: &'static str,
        generic_replacements: Rc<Vec<Type>>,
        args: Rc<Vec<Expr>>,
    },
    Var(&'static str),

    CCode(CCode),
}

impl Expr {
    pub fn check_assignable(&self, span: Span) -> Res {
        use ExprKind::*;
        let is_ptr = matches!(self.ty.kind, TypeKind::Ptr(_));
        let is_assignable = match self.kind {
            Var(_) | Field { .. } => true,
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

#[derive(Debug, Copy, Clone)]
pub enum Literal {
    Float(f64),
    Int(i64),
    Bool(bool),
    Char(char),
    StrZ(&'static str),
}

impl Literal {
    pub fn ty(&self, span: Span) -> Type {
        use Literal::*;
        match self {
            Float(_) => LiteralType::Float.ty(span),
            Int(_) => LiteralType::Int.ty(span),
            Bool(_) => PrimitiveType::Bool.ty(span),
            Char(_) => PrimitiveType::U8.ty(span),
            StrZ(_) => Type {
                span,
                kind: TypeKind::Ptr(PrimitiveType::U8.ty(span).into()),
            },
        }
    }
}

/// NOTE: hash is only simple way to prevent duplicates. extra checking is needed
#[derive(Debug, Clone, Derivative)]
#[derivative(Hash, PartialEq)]
pub struct Type {
    #[derivative(Hash = "ignore", PartialEq = "ignore")]
    pub span: Span,
    pub kind: TypeKind,
}

/// NOTE: hash is only simple way to prevent duplicates. extra checking is needed
#[derive(Debug, Clone, Derivative)]
#[derivative(Hash, PartialEq)]
pub enum TypeKind {
    Primitive(PrimitiveType),
    /// type version of the symbol
    Struct {
        #[derivative(Hash = "ignore", PartialEq = "ignore")]
        nesting_prefix: &'static str,
        name: &'static str,
        generic_replacements: Rc<Vec<Type>>,
    },
    Ptr(Rc<Type>),

    /// fixme merge these into generics when we get type inference
    Literal(LiteralType),
    /// type version of the symbol
    GenericPlaceholder(GenericPlaceholder),
    /// for inferring with var define and probably other stuff later
    Auto,
    CCode,
}
impl Eq for Type {}

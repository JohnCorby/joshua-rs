//! pre-type-checked

use crate::pass::{Ident, Literal, PrimitiveKind};
use crate::span::Span;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct Program(pub Span, pub Rc<Vec<Define>>);

#[derive(Debug, Clone)]
pub enum Define {
    Struct {
        span: Span,
        name: Ident,
        generic_placeholders: Rc<Vec<Ident>>,
        body: Rc<Vec<Define>>,
    },
    Func {
        span: Span,
        ty: TypeName,
        receiver_ty: Option<TypeName>,
        name: Ident,
        generic_placeholders: Rc<Vec<Ident>>,
        args: Rc<Vec<VarDefine>>,
        body: Block,
    },
    Var(VarDefine),

    CCode(CCode),
}

#[derive(Debug, Clone)]
pub struct VarDefine {
    pub span: Span,
    pub ty: TypeName,
    pub name: Ident,
    pub value: Option<Expr>,
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
pub struct Block(pub Span, pub Rc<Vec<Statement>>);

#[derive(Debug, Clone)]
pub struct CCode(pub Span, pub Rc<Vec<CCodePart>>);

#[derive(Debug, Clone)]
pub enum CCodePart {
    String(Span, &'static str),
    Expr(Expr),
}

#[derive(Debug, Clone)]
pub enum Expr {
    Cast {
        span: Span,
        thing: Rc<Expr>,
        ty: TypeName,
    },

    MethodCall {
        span: Span,
        receiver: Rc<Expr>,
        func_call: FuncCall,
    },
    Field {
        span: Span,
        receiver: Rc<Expr>,
        name: Ident,
    },

    // primary
    Literal(Span, Literal),
    FuncCall(FuncCall),
    Var(Ident),

    CCode(CCode),
}

#[derive(Debug, Clone)]
pub struct FuncCall {
    pub span: Span,
    pub receiver_ty: Option<TypeName>,
    pub name: Ident,
    pub generic_replacements: Rc<Vec<TypeName>>,
    pub args: Rc<Vec<Expr>>,
}

/// `span` is usually just the type name
/// except when converting from ast2, where it might be the whole expr/define/statement
#[derive(Debug, Clone)]
pub enum TypeName {
    Primitive(Span, PrimitiveKind),
    Named {
        span: Span,
        name: Ident,
        generic_replacements: Rc<Vec<TypeName>>,
    },
    Ptr(Span, Rc<TypeName>),

    Auto(Span),
}

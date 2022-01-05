//! pre-type-checked

use crate::pass::{Ident, Literal, PrimitiveKind};
use crate::span::Span;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct Program(pub Rc<Vec<Define>>);

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
        ty: Type,
        receiver_ty: Option<Type>,
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
    pub ty: Type,
    pub name: Ident,
    pub value: Option<Expr>,
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
pub struct CCode {
    pub span: Span,
    pub parts: Rc<Vec<CCodePart>>,
}

#[derive(Debug, Clone)]
pub enum CCodePart {
    String(&'static str),
    Expr(Expr),
}

#[derive(Debug, Clone)]
pub enum Expr {
    Cast {
        span: Span,
        thing: Rc<Expr>,
        ty: Type,
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
    Literal(Literal),
    FuncCall(FuncCall),
    Var(Ident),

    CCode(CCode),
}

#[derive(Debug, Clone)]
pub struct FuncCall {
    pub span: Span,
    pub receiver_ty: Option<Type>,
    pub name: Ident,
    pub generic_replacements: Rc<Vec<Type>>,
    pub args: Rc<Vec<Expr>>,
}

#[derive(Debug, Clone)]
pub enum Type {
    Primitive(PrimitiveKind),
    Named {
        span: Span,
        name: Ident,
        generic_replacements: Rc<Vec<Type>>,
    },
    Ptr(Rc<Type>),

    Auto,
}

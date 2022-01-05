//! pre-type-checked

use crate::pass::ast2::Literal;
use crate::pass::ty::PrimitiveType;
use crate::span::Span;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct Program(pub Rc<Vec<Define>>);

#[derive(Debug, Copy, Clone, Derivative, new)]
#[derivative(Hash, PartialEq)]
pub struct GenericPlaceholder {
    #[derivative(Hash = "ignore", PartialEq = "ignore")]
    #[new(default)]
    pub span: Span,
    pub name: &'static str,
}

#[derive(Debug, Clone, Derivative, new)]
pub struct Define {
    pub span: Span,
    pub kind: DefineKind,
}

#[derive(Debug, Clone)]
pub enum DefineKind {
    Struct {
        name: &'static str,
        generic_placeholders: Rc<Vec<GenericPlaceholder>>,
        body: Rc<Vec<Define>>,
    },
    Func {
        ty: Type,
        receiver_ty: Option<Type>,
        name: &'static str,
        generic_placeholders: Rc<Vec<GenericPlaceholder>>,
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
    pub name: &'static str,
    pub value: Option<Expr>,
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
}

#[derive(Debug, Clone)]
pub enum ExprKind {
    Cast {
        thing: Rc<Expr>,
        ty: Type,
    },

    MethodCall {
        receiver: Rc<Expr>,
        func_call: FuncCall,
    },
    Field {
        receiver: Rc<Expr>,
        var: &'static str,
    },

    // primary
    Literal(Literal),
    FuncCall(FuncCall),
    Var(&'static str),

    CCode(CCode),
}

#[derive(Debug, Clone)]
pub struct FuncCall {
    pub span: Span,
    pub receiver_ty: Option<Type>,
    pub name: &'static str,
    pub generic_replacements: Rc<Vec<Type>>,
    pub args: Rc<Vec<Expr>>,
}

#[derive(Debug, Clone)]
pub struct Type {
    pub span: Span,
    pub kind: TypeKind,
}

#[derive(Debug, Clone)]
pub enum TypeKind {
    Primitive(PrimitiveType),
    Named {
        name: &'static str,
        generic_replacements: Rc<Vec<Type>>,
    },
    Ptr(Rc<Type>),

    Auto,
}

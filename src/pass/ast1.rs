//! pre-type-checked

use crate::pass::ast2::Literal;
use crate::pass::ty::PrimitiveType;
use crate::span::Span;
use crate::util::ctx_str::CtxStr;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct Program<'i>(pub Rc<Vec<Define<'i>>>);

#[derive(Debug, Clone)]
pub struct Define<'i> {
    pub span: Span<'i>,
    pub kind: DefineKind<'i>,
}

#[derive(Debug, Clone)]
pub enum DefineKind<'i> {
    Struct {
        name: CtxStr<'i>,
        generic_placeholders: Rc<Vec<CtxStr<'i>>>,
        body: Rc<Vec<Define<'i>>>,
    },
    Func {
        ty: Type<'i>,
        name: CtxStr<'i>,
        generic_placeholders: Rc<Vec<CtxStr<'i>>>,
        args: Rc<Vec<VarDefine<'i>>>,
        body: Block<'i>,
    },
    Var(VarDefine<'i>),

    CCode(CCode<'i>),
}

#[derive(Debug, Clone)]
pub struct VarDefine<'i> {
    pub span: Span<'i>,
    pub ty: Type<'i>,
    pub name: CtxStr<'i>,
    pub value: Option<Expr<'i>>,
}

#[derive(Debug, Clone)]
pub struct Statement<'i> {
    pub span: Span<'i>,
    pub kind: StatementKind<'i>,
}

#[allow(clippy::large_enum_variant)]
#[derive(Debug, Clone)]
pub enum StatementKind<'i> {
    Return(Option<Expr<'i>>),
    Break,
    Continue,
    If {
        cond: Expr<'i>,
        then: Block<'i>,
        otherwise: Option<Block<'i>>,
    },
    Until {
        cond: Expr<'i>,
        block: Block<'i>,
    },
    For {
        init: VarDefine<'i>,
        cond: Expr<'i>,
        update: Rc<Statement<'i>>,
        block: Block<'i>,
    },
    ExprAssign {
        lvalue: Expr<'i>,
        rvalue: Expr<'i>,
    },
    Define(Define<'i>),
    Expr(Expr<'i>),
}

#[derive(Debug, Clone)]
pub struct Block<'i>(pub Rc<Vec<Statement<'i>>>);

#[derive(Debug, Clone)]
pub struct CCode<'i>(pub Rc<Vec<CCodePart<'i>>>);

#[derive(Debug, Clone)]
pub enum CCodePart<'i> {
    String(CtxStr<'i>),
    Expr(Expr<'i>),
}

#[derive(Debug, Clone)]
pub struct Expr<'i> {
    pub span: Span<'i>,
    pub kind: ExprKind<'i>,
}

#[derive(Debug, Clone)]
pub enum ExprKind<'i> {
    Cast {
        thing: Rc<Expr<'i>>,
        ty: Type<'i>,
    },

    MethodCall {
        receiver: Rc<Expr<'i>>,
        func_call: FuncCall<'i>,
    },
    Field {
        receiver: Rc<Expr<'i>>,
        var: CtxStr<'i>,
    },

    // primary
    Literal(Literal<'i>),
    FuncCall(FuncCall<'i>),
    Var(CtxStr<'i>),

    CCode(CCode<'i>),
}

#[derive(Debug, Clone)]
pub struct FuncCall<'i> {
    pub span: Span<'i>,
    pub name: CtxStr<'i>,
    pub generic_replacements: Rc<Vec<Type<'i>>>,
    pub args: Rc<Vec<Expr<'i>>>,
}

#[derive(Debug, Clone)]
pub struct Type<'i> {
    pub span: Span<'i>,
    pub kind: TypeKind<'i>,
}

#[derive(Debug, Clone)]
pub enum TypeKind<'i> {
    Primitive(PrimitiveType),
    Ptr(Rc<Type<'i>>),
    Named {
        name: CtxStr<'i>,
        generic_replacements: Rc<Vec<Type<'i>>>,
    },
    Auto,
}

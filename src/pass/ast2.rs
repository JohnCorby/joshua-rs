//! post-type-checked

use crate::pass::ty::{LiteralType, PrimitiveType};
use crate::util::ctx_str::CtxStr;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct Program<'i>(pub Rc<Vec<Define<'i>>>);

#[derive(Debug, Clone)]
pub enum Define<'i> {
    Struct {
        full_name: CtxStr<'i>,
        generic_replacements: Rc<Vec<Type<'i>>>,
        body: Rc<Vec<Define<'i>>>,
    },
    Func {
        ty: Type<'i>,
        full_name: CtxStr<'i>,
        generic_replacements: Rc<Vec<Type<'i>>>,
        args: Rc<Vec<VarDefine<'i>>>,
        body: Block<'i>,
    },
    Var(VarDefine<'i>),

    CCode(CCode<'i>),

    /// used for generic template that should not generate anything
    NoGen,
}

#[derive(Debug, Clone)]
pub struct VarDefine<'i> {
    pub ty: Type<'i>,
    pub name: CtxStr<'i>,
    pub value: Option<Expr<'i>>,
}

#[derive(Debug, Clone)]
pub enum Statement<'i> {
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
    pub kind: ExprKind<'i>,
    pub ty: Type<'i>,
}

#[derive(Debug, Clone)]
pub enum ExprKind<'i> {
    Cast {
        nesting_prefix: CtxStr<'i>,
        thing: Rc<Expr<'i>>,
    },

    Field {
        receiver: Rc<Expr<'i>>,
        var: CtxStr<'i>,
    },

    // primary
    Literal(Literal<'i>),
    FuncCall {
        full_name: CtxStr<'i>,
        generic_replacements: Rc<Vec<Type<'i>>>,
        args: Rc<Vec<Expr<'i>>>,
    },
    Var(CtxStr<'i>),

    CCode(CCode<'i>),
}

#[derive(Debug, Copy, Clone)]
pub enum Literal<'i> {
    Float(f64),
    Int(i64),
    Bool(bool),
    Char(char),
    StrZ(CtxStr<'i>),
}

impl Literal<'i> {
    pub fn ty(&self) -> Type<'i> {
        use Literal::*;
        match self {
            Float(_) => LiteralType::Float.ty(),
            Int(_) => LiteralType::Int.ty(),
            Bool(_) => PrimitiveType::Bool.ty(),
            Char(_) => PrimitiveType::U8.ty(),
            StrZ(_) => Type::Ptr(PrimitiveType::U8.ty().into()),
        }
    }
}

#[derive(Debug, Clone, derivative::Derivative)]
#[derivative(Hash, PartialEq)]
pub enum Type<'i> {
    Primitive(PrimitiveType),
    /// fixme merge these into generics when we get type inference
    Literal(LiteralType),
    CCode,
    Struct {
        #[derivative(Hash = "ignore", PartialEq = "ignore")]
        nesting_prefix: CtxStr<'i>,
        name: CtxStr<'i>,
        generic_replacements: Rc<Vec<Type<'i>>>,
    },
    /// replaced with concrete type on specialization
    GenericPlaceholder(CtxStr<'i>),
    Ptr(Rc<Type<'i>>),
    /// for inferring with var define and probably other stuff later
    Auto,
}

//! post-type-checked

use crate::error::{err, Res};
use crate::pass::ty::{LiteralType, PrimitiveType};
use crate::span::Span;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct Program<'i>(pub Rc<Vec<Define<'i>>>);

#[derive(Debug, Clone)]
pub enum Define<'i> {
    Struct {
        full_name: &'i str,
        generic_replacements: Rc<Vec<Type<'i>>>,
        body: Rc<Vec<Define<'i>>>,
    },
    Func {
        ty: Type<'i>,
        full_name: &'i str,
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
    pub name: &'i str,
    pub value: Option<Expr<'i>>,

    /// used for gen. kinda hacky, oh well
    pub is_global: bool,
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
    String(&'i str),
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
        nesting_prefix: &'i str,
        thing: Rc<Expr<'i>>,
    },

    Field {
        receiver: Rc<Expr<'i>>,
        var: &'i str,
    },

    // primary
    Literal(Literal<'i>),
    FuncCall {
        full_name: &'i str,
        generic_replacements: Rc<Vec<Type<'i>>>,
        args: Rc<Vec<Expr<'i>>>,
    },
    Var(&'i str),

    CCode(CCode<'i>),
}

impl Expr<'i> {
    pub fn check_assignable(&self, span: Option<Span<'i>>) -> Res<'i> {
        use ExprKind::*;
        let is_ptr = matches!(self.ty, Type::Ptr(_));
        let is_non_void = self.ty != PrimitiveType::Void.ty();
        let is_assignable = match self.kind {
            Var(_) if is_non_void => true,
            Field { .. } if is_non_void => true,
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
pub enum Literal<'i> {
    Float(f64),
    Int(i64),
    Bool(bool),
    Char(char),
    StrZ(&'i str),
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

/// NOTE: hash is only simple way to prevent duplicates. extra checking is needed
#[derive(Debug, Clone, Derivative)]
#[derivative(Hash, PartialEq)]
pub enum Type<'i> {
    Primitive(PrimitiveType),
    Struct {
        #[derivative(Hash = "ignore", PartialEq = "ignore")]
        nesting_prefix: &'i str,
        name: &'i str,
        generic_replacements: Rc<Vec<Type<'i>>>,
    },
    Ptr(Rc<Type<'i>>),

    /// fixme merge these into generics when we get type inference
    Literal(LiteralType),
    /// replaced with concrete type on specialization
    GenericPlaceholder(&'i str),
    /// for inferring with var define and probably other stuff later
    Auto,
    CCode,
}

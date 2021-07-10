//! post-type-checked

use crate::error::{err, Res};
use crate::pass::ty::{LiteralType, PrimitiveType};
use crate::span::Span;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct Program(pub Rc<Vec<Define>>);

#[derive(Debug, Clone)]
pub enum Define {
    Struct {
        full_name: &'static str,
        body: Rc<Vec<Define>>,
    },
    Func {
        ty: Type,
        full_name: &'static str,
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
    pub ty: Type,
    pub name: &'static str,
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
pub struct Expr {
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
        full_name: &'static str,
        args: Rc<Vec<Expr>>,
    },
    Var(&'static str),

    CCode(CCode),
}

impl Expr {
    pub fn check_assignable(&self, span: Span) -> Res {
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
pub enum Literal {
    Float(f64),
    Int(i64),
    Bool(bool),
    Char(char),
    StrZ(&'static str),
}

impl Literal {
    pub fn ty(&self) -> Type {
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
pub enum Type {
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
    GenericPlaceholder(&'static str),
    /// for inferring with var define and probably other stuff later
    Auto,
    CCode,
}

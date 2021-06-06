use crate::error::{err, Res};
use crate::pass::ty::{LiteralType, PrimitiveType, Type};
use crate::span::Span;
use crate::util::ctx_str::CtxStr;
use crate::util::late_init::LateInit;
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
        ty_node: TypeNode<'i>,
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
    pub ty_node: TypeNode<'i>,
    pub name: CtxStr<'i>,
    pub value: Option<Expr<'i>>,
}

#[derive(Debug, Clone)]
pub struct Statement<'i> {
    pub span: Span<'i>,
    pub kind: StatementKind<'i>,
}

#[derive(Debug, Clone)]
#[allow(clippy::large_enum_variant)]
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
    VarDefine(VarDefine<'i>),
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

impl CCode<'i> {
    pub fn ty(&self) -> Type<'i> {
        LiteralType::CCode.ty()
    }
}

#[derive(Debug, Clone)]
pub struct Expr<'i> {
    pub span: Span<'i>,
    pub kind: ExprKind<'i>,
    pub ty: LateInit<Type<'i>>,
}

#[derive(Debug, Clone)]
pub enum ExprKind<'i> {
    Cast {
        thing: Rc<Expr<'i>>,
        ty_node: TypeNode<'i>,
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

impl Expr<'i> {
    pub fn check_assignable(&self, span: Option<Span<'i>>) -> Res<'i> {
        use ExprKind::*;
        let is_assignable = matches!(self.kind, Field { .. } | Var(_));

        if !is_assignable {
            err("expr is not assignable", span)
        } else {
            Ok(())
        }
    }
}

#[derive(Debug, Clone)]
pub struct FuncCall<'i> {
    pub span: Span<'i>,
    pub name: CtxStr<'i>,
    pub generic_replacements: Rc<Vec<TypeNode<'i>>>,
    pub args: Rc<Vec<Expr<'i>>>,
    pub ty: LateInit<Type<'i>>,
}

#[derive(Debug, Copy, Clone)]
pub enum Literal<'i> {
    Float(f64),
    Int(i64),
    Bool(bool),
    Char(char),
    Str(CtxStr<'i>),
}

impl Literal<'i> {
    pub fn ty(&self) -> Type<'i> {
        use Literal::*;
        match self {
            Float(_) => LiteralType::Float.ty(),
            Int(_) => LiteralType::Int.ty(),
            Bool(_) => PrimitiveType::Bool.ty(),
            Char(_) => PrimitiveType::Char.ty(),
            Str(_) => todo!("usage of string literals is not yet supported"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct TypeNode<'i> {
    pub span: Span<'i>,
    pub kind: TypeKind<'i>,
    pub ty: LateInit<Type<'i>>,
}

#[derive(Debug, Clone)]
pub enum TypeKind<'i> {
    Primitive(PrimitiveType),
    Ptr(Rc<TypeNode<'i>>),
    Named {
        name: CtxStr<'i>,
        generic_replacements: Rc<Vec<TypeNode<'i>>>,
    },
    Auto,
}

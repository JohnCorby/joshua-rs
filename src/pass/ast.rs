use crate::context::Ctx;
use crate::error::{err, Res};
use crate::parse::Node;
use crate::pass::ty::{LiteralType, PrimitiveType, Type};
use crate::span::Span;
use crate::util::interned_str::{Intern, InternedStr};
use crate::util::late_init::LateInit;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct Program<'i>(pub Vec<Define<'i>>);

#[derive(Debug, Clone)]
pub struct Define<'i> {
    pub span: Span<'i>,
    pub kind: DefineKind<'i>,
}

#[derive(Debug, Clone)]
pub enum DefineKind<'i> {
    Struct {
        name: InternedStr<'i>,
        body: Vec<Define<'i>>,
    },
    Func {
        ty_node: TypeNode<'i>,
        name: InternedStr<'i>,
        generic_placeholders: Vec<InternedStr<'i>>,
        args: Vec<VarDefine<'i>>,
        body: Block<'i>,
    },
    Var(VarDefine<'i>),

    CCode(CCode<'i>),
}

#[derive(Debug, Clone)]
pub struct VarDefine<'i> {
    pub span: Span<'i>,
    pub ty_node: TypeNode<'i>,
    pub name: InternedStr<'i>,
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
        update: Box<Statement<'i>>,
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
pub struct Block<'i>(pub Vec<Statement<'i>>);

#[derive(Debug, Clone)]
pub struct CCode<'i>(pub Vec<CCodePart<'i>>);

#[derive(Debug, Clone)]
pub enum CCodePart<'i> {
    String(&'i str),
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
        thing: Box<Expr<'i>>,
        ty_node: TypeNode<'i>,
    },

    Field {
        receiver: Box<Expr<'i>>,
        var: InternedStr<'i>,
    },

    // primary
    Literal(Literal<'i>),
    FuncCall(FuncCall<'i>),
    Var(InternedStr<'i>),

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
    pub name: InternedStr<'i>,
    pub generic_replacements: Vec<TypeNode<'i>>,
    pub args: Vec<Expr<'i>>,
    pub ty: LateInit<Type<'i>>,
}

#[derive(Debug, Clone)]
pub enum Literal<'i> {
    Float(f64),
    Int(i64),
    Bool(bool),
    Char(char),
    Str(&'i str),
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

impl Node<'i> {
    pub fn visit_ident(&self, ctx: &mut Ctx<'i>) -> InternedStr<'i> {
        let str = self.str();
        str.strip_prefix('`')
            .unwrap_or(str)
            .strip_suffix('`')
            .unwrap_or(str)
            .intern(ctx)
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
    Named(InternedStr<'i>),
    Auto,
}

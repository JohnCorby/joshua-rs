//! post-type-checked

use crate::error::{err, Res};
use crate::pass::{Literal, PrimitiveKind};
use crate::span::Span;
use crate::util::StrExt;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct Program(pub Rc<Vec<Define>>);

#[derive(Debug, Clone)]
pub enum Define {
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
pub enum Expr {
    Cast {
        nesting_prefix: &'static str,
        thing: Rc<Expr>,
        ty: Type,
    },

    Field {
        receiver: Rc<Expr>,
        name: &'static str,
        ty: Type,
    },

    // primary
    Literal(Literal),
    FuncCall {
        nesting_prefix: &'static str,
        receiver_ty: Option<Type>,
        name: &'static str,
        generic_replacements: Rc<Vec<Type>>,
        args: Rc<Vec<Expr>>,
        ty: Type,
    },
    Var(&'static str, Type),

    CCode(CCode),
}

impl Expr {
    pub fn ty(&self) -> Type {
        use Expr::*;
        match self {
            Cast { ty, .. } => ty.clone(),
            Field { ty, .. } => ty.clone(),
            Literal(literal) => match literal {
                self::Literal::Float(..) => Type::Literal(LiteralKind::Float),
                self::Literal::Int(..) => Type::Literal(LiteralKind::Int),
                self::Literal::Bool(..) => Type::Primitive(PrimitiveKind::Bool),
                self::Literal::Char(..) => Type::Primitive(PrimitiveKind::U8),
                self::Literal::StrZ(..) => Type::Ptr(Type::Primitive(PrimitiveKind::U8).into()),
            },
            FuncCall { ty, .. } => ty.clone(),
            Var(.., ty) => ty.clone(),
            CCode(..) => Type::CCode,
        }
    }

    pub fn check_assignable(&self, err_span: Span) -> Res {
        use Expr::*;
        let is_ptr = matches!(self.ty(), Type::Ptr(..));
        let is_assignable = match self {
            Var(..) | Field { .. } => true,
            FuncCall { .. } if is_ptr => true,
            _ => false,
        };

        if !is_assignable {
            err("expr is not assignable", err_span)
        } else {
            Ok(())
        }
    }
}

/// NOTE: hash is only simple way to prevent duplicates. extra checking is needed
///
/// used as both a type name and for storing type info
#[derive(Debug, Clone, Derivative)]
#[derivative(Hash, PartialEq)]
pub enum Type {
    Primitive(PrimitiveKind),
    /// type version of the symbol
    Struct {
        #[derivative(Hash = "ignore", PartialEq = "ignore")]
        nesting_prefix: &'static str,
        name: &'static str,
        generic_replacements: Rc<Vec<Type>>,
    },
    Ptr(Rc<Type>),

    /// fixme merge these into generics when we get type inference
    Literal(LiteralKind),
    /// type version of the symbol
    GenericPlaceholder(&'static str),
    /// for inferring with var define and probably other stuff later
    Auto,
    CCode,
}
impl Eq for Type {}
impl Default for Type {
    fn default() -> Self {
        Self::Primitive(PrimitiveKind::Void)
    }
}
impl Type {
    pub fn check(&self, expected: &Self, err_span: Span) -> Res {
        let actual = self;
        if expected == actual {
            Ok(())
        } else {
            err(
                &format!(
                    "expected {}, but got {}",
                    expected.encode(false),
                    actual.encode(false)
                ),
                err_span,
            )
        }
    }

    /// used for codegen and display
    pub fn encode(&self, include_nesting_prefixes: bool) -> String {
        use Type::*;
        match &self {
            Primitive(kind) => kind.to_string(),
            Struct {
                nesting_prefix,
                name,
                generic_replacements,
            } => name.encode(
                nesting_prefix,
                None,
                generic_replacements,
                None,
                include_nesting_prefixes,
            ),
            Ptr(ty) => format!("ptr<{}>", ty.encode(include_nesting_prefixes)),
            GenericPlaceholder(name) => name.to_string(),
            _ => panic!("type {:?} shouldn't be displayed or encoded", self),
        }
    }
}

#[derive(Debug, Copy, Clone, Hash, PartialEq)]
pub enum LiteralKind {
    Float,
    Int,
}

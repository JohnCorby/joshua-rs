//! handle the painful process that is parsing expressions

use crate::cached::CachedString;
use crate::error::{unexpected_rule, MyResult};
use crate::parse::{Node, Rule};
use crate::pass::{Gen, Visit};
use crate::scope::Scope;
use crate::span::Span;
use crate::statement::FuncCall;
use crate::ty::{HasType, LiteralType, PrimitiveType, Type};
use crate::with::{ToWith, WithSpan};

#[derive(Debug, Clone)]
pub enum Expr {
    Binary {
        left: Box<WithSpan<Expr>>,
        op: WithSpan<CachedString>,
        right: Box<WithSpan<Expr>>,
    },
    Unary {
        op: WithSpan<CachedString>,
        thing: Box<WithSpan<Expr>>,
    },
    Cast {
        thing: Box<WithSpan<Expr>>,
        ty: WithSpan<Type>,
    },

    // primary
    Literal(Literal),
    FuncCall(FuncCall),
    Var(CachedString),
}

impl Visit for Expr {
    fn visit_impl(node: Node) -> Self {
        let span = node.span();
        match node.rule() {
            Rule::expr => node.children().next().unwrap().visit(),
            Rule::equality_expr | Rule::compare_expr | Rule::add_expr | Rule::mul_expr => {
                // left assoc
                let mut nodes = node.children();

                let mut left: WithSpan<Expr> = nodes.next().unwrap().visit();
                while let Some(op) = nodes.next() {
                    // let op = With { thing: op.as_str(), with: op };
                    left = Self::Binary {
                        left: left.into(),
                        op: op.as_cached_str_with_span(),
                        right: nodes.next().unwrap().visit::<Expr>().into(),
                    }
                    .with(span);
                }

                left
            }
            Rule::unary_expr => {
                // right assoc
                let mut rev_nodes = node.children().rev();

                let mut thing: WithSpan<Expr> = rev_nodes.next().unwrap().visit();
                for op in rev_nodes {
                    thing = Self::Unary {
                        op: op.as_cached_str_with_span(),
                        thing: thing.into(),
                    }
                    .with(span);
                }

                thing
            }
            Rule::cast_expr => {
                // left assoc
                let mut nodes = node.children();

                let mut thing: WithSpan<Expr> = nodes.next().unwrap().visit();
                for ty in nodes {
                    thing = Self::Cast {
                        thing: thing.into(),
                        ty: ty.visit(),
                    }
                    .with(span);
                }

                thing
            }

            // primary
            Rule::float_literal
            | Rule::int_literal
            | Rule::bool_literal
            | Rule::char_literal
            | Rule::str_literal => Self::Literal(node.visit().0).with(span),
            Rule::func_call => Self::FuncCall(node.visit().0).with(span),
            Rule::ident => Self::Var(node.as_str().into()).with(node.span()),

            rule => unexpected_rule(rule),
        }
        .0
    }
}

impl Gen for WithSpan<Expr> {
    fn span(&self) -> Span {
        self.1
    }

    fn gen_impl(self) -> MyResult<String> {
        Ok(match self.0 {
            Expr::Binary { left, op, right } => {
                let s = format!(
                    "({} {} {})",
                    left.clone().gen()?,
                    op.to_string(),
                    right.clone().gen()?
                );
                // type check
                Scope::current().get_func(*op, [left.ty(), right.ty()])?;
                s
            }
            Expr::Unary { op, thing } => {
                let s = format!("({}{})", op.to_string(), thing.clone().gen()?);
                // type check
                Scope::current().get_func(*op, [thing.ty()])?;
                s
            }
            Expr::Cast { thing, ty } => {
                let s = format!("(({}) {})", ty.gen()?, thing.clone().gen()?);
                // type check
                Scope::current().get_func(format!("as {}", ty.to_string()).into(), [thing.ty()])?;
                s
            }
            Expr::Literal(literal) => literal.with(self.1).gen()?,
            Expr::FuncCall(func_call) => func_call.with(self.1).gen()?,
            Expr::Var(name) => {
                Scope::current().get_var(name)?;
                name.to_string()
            }
        })
    }
}

impl HasType for Expr {
    fn ty(&self) -> Type {
        const GET_OP_FUNC_ERR: &str =
            "cant get op func symbol for HasType even though this should have already been checked";
        const GET_CAST_FUNC_ERR: &str =
            "cant get cast func symbol for HasType even though this should have already been checked";
        const GET_VAR_ERR: &str =
            "cant get var symbol for HasType even though this should have already been checked";
        match self {
            Expr::Binary { left, op, right } => Scope::current()
                .get_func(op.0, [left.ty(), right.ty()])
                .expect(GET_OP_FUNC_ERR)
                .ty(),
            Expr::Unary { op, thing } => Scope::current()
                .get_func(op.0, [thing.ty()])
                .expect(GET_OP_FUNC_ERR)
                .ty(),
            Expr::Cast { thing, ty } => Scope::current()
                .get_func(format!("as {}", ty.to_string()).into(), [thing.ty()])
                .expect(GET_CAST_FUNC_ERR)
                .ty(),
            Expr::Literal(literal) => literal.ty(),
            Expr::FuncCall(func_call) => func_call.ty(),
            Expr::Var(name) => Scope::current().get_var(*name).expect(GET_VAR_ERR).ty(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Literal {
    Float(f64),
    Int(i64),
    Bool(bool),
    Char(char),
    Str(String),
}

impl Visit for Literal {
    fn visit_impl(node: Node) -> Self {
        match node.rule() {
            Rule::float_literal => Self::Float(node.as_str().parse().unwrap()),
            Rule::int_literal => Self::Int(node.as_str().parse().unwrap()),
            Rule::bool_literal => Self::Bool(node.as_str().parse().unwrap()),
            Rule::char_literal => {
                Self::Char(node.children().next().unwrap().as_str().parse().unwrap())
            }
            Rule::str_literal => Self::Str(node.children().next().unwrap().as_str().into()),

            rule => unexpected_rule(rule),
        }
    }
}

impl Gen for WithSpan<Literal> {
    fn span(&self) -> Span {
        self.1
    }

    fn gen_impl(self) -> MyResult<String> {
        Ok(match &*self {
            Literal::Float(value) => value.to_string(),
            Literal::Int(value) => value.to_string(),
            Literal::Bool(value) => (*value as u8).to_string(),
            Literal::Char(value) => format!("'{}'", value),
            Literal::Str(value) => format!("\"{}\"", value),
        })
    }
}

impl HasType for Literal {
    fn ty(&self) -> Type {
        match self {
            Self::Float(_) => LiteralType::Float.ty(),
            Self::Int(_) => LiteralType::Int.ty(),
            Self::Bool(_) => PrimitiveType::Bool.ty(),
            Self::Char(_) => PrimitiveType::Char.ty(),
            Self::Str(_) => todo!("usage of string literals is not yet supported"),
        }
    }
}

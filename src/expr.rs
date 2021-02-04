//! handle the painful process that is parsing expressions

use crate::cached::CachedString;
use crate::error::{unexpected_rule, MyResult};
use crate::gen::Gen;
use crate::parse::{Pair, Rule};
use crate::pos::{AsPos, Pos};
use crate::scope::Scope;
use crate::statement::FuncCall;
use crate::ty::{HasType, LiteralType, PrimitiveType, Type};
use crate::util::PairExt;
use crate::visit::Visit;
use crate::with::{ToWith, WithPos};

#[derive(Debug, Clone)]
pub enum Expr {
    Binary {
        left: Box<WithPos<Expr>>,
        op: WithPos<CachedString>,
        right: Box<WithPos<Expr>>,
    },
    Unary {
        op: WithPos<CachedString>,
        thing: Box<WithPos<Expr>>,
    },
    Cast {
        thing: Box<WithPos<Expr>>,
        ty: WithPos<Type>,
    },

    // primary
    Literal(Literal),
    FuncCall(FuncCall),
    Var(CachedString),
}

impl Visit for Expr {
    fn visit_impl(pair: Pair) -> Self {
        let pos = pair.as_pos();
        match pair.as_rule() {
            Rule::expr => pair.into_inner().next().unwrap().visit(),
            Rule::equality_expr | Rule::compare_expr | Rule::add_expr | Rule::mul_expr => {
                // left assoc
                let mut pairs = pair.into_inner();

                let mut left: WithPos<Expr> = pairs.next().unwrap().visit();
                while let Some(op) = pairs.next() {
                    // let op = With { thing: op.as_str(), with: op };
                    left = Self::Binary {
                        left: left.into(),
                        op: op.as_cached_str_with_pos(),
                        right: pairs.next().unwrap().visit::<Expr>().into(),
                    }
                    .with(pos);
                }

                left
            }
            Rule::unary_expr => {
                // right assoc
                let mut rev_pairs = pair.into_inner().rev();

                let mut thing: WithPos<Expr> = rev_pairs.next().unwrap().visit();
                for op in rev_pairs {
                    thing = Self::Unary {
                        op: op.as_cached_str_with_pos(),
                        thing: thing.into(),
                    }
                    .with(pos);
                }

                thing
            }
            Rule::cast_expr => {
                // left assoc
                let mut pairs = pair.into_inner();

                let mut thing: WithPos<Expr> = pairs.next().unwrap().visit();
                for ty in pairs {
                    thing = Self::Cast {
                        thing: thing.into(),
                        ty: ty.visit(),
                    }
                    .with(pos);
                }

                thing
            }

            // primary
            Rule::float_literal
            | Rule::int_literal
            | Rule::bool_literal
            | Rule::char_literal
            | Rule::str_literal => Self::Literal(pair.visit().inner).with(pos),
            Rule::func_call => Self::FuncCall(pair.visit().inner).with(pos),
            Rule::ident => Self::Var(pair.as_str().into()).with(pair.as_pos()),

            rule => unexpected_rule(rule),
        }
        .inner
    }
}

impl Gen for WithPos<Expr> {
    fn pos(&self) -> Pos {
        self.extra
    }

    fn gen_impl(self) -> MyResult<String> {
        Ok(match self.inner {
            Expr::Binary {
                left, op, right, ..
            } => {
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
            Expr::Unary { op, thing, .. } => {
                let s = format!("({}{})", op.to_string(), thing.clone().gen()?);
                // type check
                Scope::current().get_func(*op, [thing.ty()])?;
                s
            }
            Expr::Cast { thing, ty, .. } => {
                // let thing_ty = thing.ty();
                let s = format!("(({}) {})", ty.gen()?, thing.gen()?);
                // todo type check
                s
            }
            Expr::Literal(literal) => literal.with(self.extra).gen()?,
            Expr::FuncCall(func_call) => func_call.with(self.extra).gen()?,
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
        // const GET_CAST_FUNC_ERR: &str =
        //     "cant get cast func symbol for HasType even though this should have already been checked";
        const GET_VAR_ERR: &str =
            "cant get var symbol for HasType even though this should have already been checked";
        match self {
            Expr::Binary {
                left, op, right, ..
            } => Scope::current()
                .get_func(op.inner, [left.ty(), right.ty()])
                .expect(GET_OP_FUNC_ERR)
                .ty(),
            Expr::Unary { op, thing, .. } => Scope::current()
                .get_func(op.inner, [thing.ty()])
                .expect(GET_OP_FUNC_ERR)
                .ty(),
            Expr::Cast { ty, .. } => {
                // todo type check
                ty.inner
            }
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
    fn visit_impl(pair: Pair) -> Self {
        match pair.as_rule() {
            Rule::float_literal => Self::Float(pair.as_str().parse().unwrap()),
            Rule::int_literal => Self::Int(pair.as_str().parse().unwrap()),
            Rule::bool_literal => Self::Bool(pair.as_str().parse().unwrap()),
            Rule::char_literal => {
                Self::Char(pair.into_inner().next().unwrap().as_str().parse().unwrap())
            }
            Rule::str_literal => Self::Str(pair.into_inner().next().unwrap().as_str().into()),

            rule => unexpected_rule(rule),
        }
    }
}

impl Gen for WithPos<Literal> {
    fn pos(&self) -> Pos {
        self.extra
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

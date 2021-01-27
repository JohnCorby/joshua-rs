//! handle the painful process that is parsing expressions

use crate::error::{unexpected_rule, MyResult};
use crate::parse::{Pair, Rule};
use crate::ty::Type;
use crate::util::{PairExt, PairsExt};
use crate::visit::Visit;
use crate::Ref;

#[derive(Debug, Clone)]
pub enum Expr {
    Binary {
        left: Ref<Expr>,
        op: String,
        right: Ref<Expr>,
    },
    Unary {
        op: String,
        thing: Ref<Expr>,
    },
    Cast {
        thing: Ref<Expr>,
        ty: Type,
    },

    // primary
    Literal(Literal),
    FuncCall {
        name: String,
        args: Vec<Expr>,
    },
    Var(String),
}

impl Visit for Expr {
    fn visit_impl(pair: Pair) -> MyResult<Self> {
        Ok(match pair.as_rule() {
            Rule::expr => pair.into_inner().next()?.visit()?,
            Rule::equality_expr | Rule::compare_expr | Rule::add_expr | Rule::mul_expr => {
                // left assoc
                let mut pairs = pair.into_inner();

                let mut left: Expr = pairs.next()?.visit()?;
                while let Some(op) = pairs.next() {
                    left = Self::Binary {
                        left: left.into(),
                        op: op.as_str().into(),
                        right: pairs.next()?.visit::<Expr>()?.into(),
                    };
                }

                left
            }
            Rule::unary_expr => {
                // right assoc
                let mut rev_pairs = pair.into_inner().rev();

                let mut thing: Expr = rev_pairs.next()?.visit()?;
                for op in rev_pairs {
                    thing = Self::Unary {
                        op: op.as_str().into(),
                        thing: thing.into(),
                    };
                }

                thing
            }
            Rule::cast_expr => {
                // left assoc
                let mut pairs = pair.into_inner();

                let mut thing: Expr = pairs.next()?.visit()?;
                for ty in pairs {
                    thing = Self::Cast {
                        thing: thing.into(),
                        ty: ty.visit()?,
                    };
                }

                thing
            }

            // primary
            Rule::float_literal
            | Rule::int_literal
            | Rule::bool_literal
            | Rule::char_literal
            | Rule::str_literal => Self::Literal(pair.visit()?),
            Rule::func_call => {
                let mut pairs = pair.into_inner();

                Self::FuncCall {
                    name: pairs.next()?.as_str().into(),
                    args: pairs.visit_rest()?,
                }
            }
            Rule::ident => Self::Var(pair.as_str().into()),

            rule => unexpected_rule(rule)?,
        })
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
    fn visit_impl(pair: Pair) -> MyResult<Self> {
        Ok(match pair.as_rule() {
            Rule::float_literal => Self::Float(pair.as_str().parse()?),
            Rule::int_literal => Self::Int(pair.as_str().parse()?),
            Rule::bool_literal => Self::Bool(pair.as_str().parse()?),
            Rule::char_literal => Self::Char(pair.into_inner().next()?.as_str().parse()?),
            Rule::str_literal => Self::Str(pair.into_inner().next()?.as_str().into()),

            rule => unexpected_rule(rule)?,
        })
    }
}

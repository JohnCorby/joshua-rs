//! handle the painful process that is parsing expressions

use crate::error::MyResult;
use crate::util::{PairExt, PairsExt};
use crate::visit::Visit;
use crate::{Pair, Rule};

#[derive(Debug, Clone)]
pub enum Expr {
    Binary {
        left: Box<Expr>,
        right: Box<Expr>,
        op: String,
    },
    Unary {
        thing: Box<Expr>,
        op: String,
    },
    Cast {
        thing: Box<Expr>,
        ty: String,
    },

    // primary
    Literal(Literal),
    FuncCall {
        name: String,
        args: Vec<Expr>,
    },
    Var(String),
}

impl Visit<'_> for Expr {
    fn visit(pair: Pair) -> MyResult<Self> {
        match pair.as_rule() {
            Rule::expr => Ok(pair.into_inner().next()?.visit()?),
            Rule::equality_expr | Rule::compare_expr | Rule::add_expr | Rule::mul_expr => {
                // left assoc
                let mut pairs = pair.into_inner();

                let mut left = pairs.next()?.visit()?;
                while let Some(pair) = pairs.next() {
                    let op = pair.as_str();
                    let right = pairs.next()?.visit()?;

                    left = Self::visit_binary(left, right, op)?;
                }

                Ok(left)
            }
            Rule::unary_expr => {
                // right assoc
                let mut rev_pairs = pair.into_inner().rev();

                let mut thing = rev_pairs.next()?.visit()?;
                for pair in rev_pairs {
                    let op = pair.as_str();

                    thing = Self::visit_unary(thing, op)?;
                }

                Ok(thing)
            }
            Rule::cast_expr => {
                // left assoc
                let mut pairs = pair.into_inner();

                let mut thing = pairs.next()?.visit()?;
                for pair in pairs {
                    let ty = pair.as_str();

                    thing = Self::visit_cast(thing, ty)?;
                }

                Ok(thing)
            }

            Rule::primary_expr => Self::visit_primary(pair.into_inner().next()?),

            rule => Err(rule.into()),
        }
    }
}

#[allow(clippy::unnecessary_wraps)]
impl Expr {
    fn visit_binary(left: Self, right: Self, op: &str) -> MyResult<Self> {
        // todo op check

        Ok(Self::Binary {
            left: left.into(),
            right: right.into(),
            op: op.into(),
        })
    }

    fn visit_unary(thing: Self, op: &str) -> MyResult<Self> {
        // todo op check

        Ok(Self::Unary {
            thing: thing.into(),
            op: op.into(),
        })
    }

    fn visit_cast(thing: Self, ty: &str) -> MyResult<Self> {
        // todo type check

        Ok(Self::Cast {
            thing: thing.into(),
            ty: ty.into(),
        })
    }

    fn visit_primary(pair: Pair) -> MyResult<Self> {
        // fixme might refactor this back into parse_Self or something
        match pair.as_rule() {
            Rule::literal => Ok(Self::Literal(pair.into_inner().next()?.visit()?)),
            Rule::func_call => {
                let mut pairs = pair.into_inner();

                Ok(Self::FuncCall {
                    name: pairs.next()?.as_str().into(),
                    args: pairs.visit_rest()?,
                })
            }
            Rule::ident => Ok(Self::Var(pair.as_str().into())),
            Rule::paren_expr => Ok(pair.into_inner().next()?.visit()?),

            rule => Err(rule.into()),
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

impl Visit<'_> for Literal {
    fn visit(pair: Pair) -> MyResult<Self> {
        match pair.as_rule() {
            Rule::float_literal => Ok(Self::Float(pair.as_str().parse()?)),
            Rule::int_literal => Ok(Self::Int(pair.as_str().parse()?)),
            Rule::bool_literal => Ok(Self::Bool(pair.as_str().parse()?)),
            Rule::char_literal => Ok(Self::Char(pair.into_inner().next()?.as_str().parse()?)),
            Rule::str_literal => Ok(Self::Str(pair.into_inner().next()?.as_str().into())),

            rule => Err(rule.into()),
        }
    }
}

//! handle the painful process that is parsing expressions

use crate::error::{unexpected_rule, MyResult};
use crate::ty::Type;
use crate::util::{PairExt, PairsExt};
use crate::visit::Visit;
use crate::{Pair, Rule};
use std::convert::TryInto;

#[derive(Debug, Clone)]
pub enum Expr {
    Binary {
        left: Box<Expr>,
        op: String,
        right: Box<Expr>,
    },
    Unary {
        op: String,
        thing: Box<Expr>,
    },
    Cast {
        thing: Box<Expr>,
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
    fn visit(pair: Pair) -> MyResult<Self> {
        Ok(match pair.as_rule() {
            Rule::expr => pair.into_inner().next()?.visit()?,
            Rule::equality_expr | Rule::compare_expr | Rule::add_expr | Rule::mul_expr => {
                // left assoc
                let mut pairs = pair.into_inner();

                let mut left = pairs.next()?.visit()?;
                while let Some(pair) = pairs.next() {
                    let op = pair.as_str();
                    let right = pairs.next()?.visit()?;

                    left = Self::visit_binary(left, op, right)?;
                }

                left
            }
            Rule::unary_expr => {
                // right assoc
                let mut rev_pairs = pair.into_inner().rev();

                let mut thing = rev_pairs.next()?.visit()?;
                for pair in rev_pairs {
                    let op = pair.as_str();

                    thing = Self::visit_unary(op, thing)?;
                }

                thing
            }
            Rule::cast_expr => {
                // left assoc
                let mut pairs = pair.into_inner();

                let mut thing = pairs.next()?.visit()?;
                for pair in pairs {
                    let ty = pair.as_str();

                    thing = Self::visit_cast(thing, ty)?;
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

impl Expr {
    fn visit_binary(left: Self, op: impl AsRef<str>, right: Self) -> MyResult<Self> {
        // todo op check

        Ok(Self::Binary {
            left: left.into(),
            right: right.into(),
            op: op.as_ref().into(),
        })
    }

    fn visit_unary(op: impl AsRef<str>, thing: Self) -> MyResult<Self> {
        // todo op check

        Ok(Self::Unary {
            thing: thing.into(),
            op: op.as_ref().into(),
        })
    }

    fn visit_cast(thing: Self, ty: impl AsRef<str>) -> MyResult<Self> {
        Ok(Self::Cast {
            thing: thing.into(),
            ty: ty.as_ref().try_into()?,
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
    fn visit(pair: Pair) -> MyResult<Self> {
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

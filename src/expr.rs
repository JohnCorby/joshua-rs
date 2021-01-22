//! handle the painful process that is parsing expressions

use crate::error::MyError::UnreachableRule;
use crate::error::MyResult;
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
    Ident(String),
    Paren(Box<Expr>),
}

impl Visit for Expr {
    fn visit(pair: Pair) -> MyResult<Self> {
        match pair.as_rule() {
            Rule::expr => Self::visit(pair.into_inner().next()?),
            Rule::equality_expr | Rule::compare_expr | Rule::add_expr | Rule::mul_expr => {
                // left assoc
                let mut pairs = pair.into_inner();

                let mut left = Self::visit(pairs.next()?)?;
                while let Some(pair) = pairs.next() {
                    let op = pair.as_str();
                    let right = Self::visit(pairs.next()?)?;

                    left = Self::visit_binary(left, right, op)?;
                }

                Ok(left)
            }
            Rule::unary_expr => {
                // right assoc
                let mut rev_pairs = pair.into_inner().rev();

                let mut thing = Self::visit(rev_pairs.next()?)?;
                for pair in rev_pairs {
                    let op = pair.as_str();

                    thing = Self::visit_unary(thing, op)?;
                }

                Ok(thing)
            }
            Rule::cast_expr => {
                // left assoc
                let mut pairs = pair.into_inner();

                let mut thing = Self::visit(pairs.next()?)?;
                for pair in pairs {
                    let ty = pair.as_str();

                    thing = Self::visit_cast(thing, ty)?;
                }

                Ok(thing)
            }

            Rule::primary_expr => Self::visit_primary(pair.into_inner().next()?),

            rule => Err(UnreachableRule(rule)),
        }
    }
}

#[allow(clippy::unnecessary_wraps)]
impl Expr {
    fn visit_binary(left: Expr, right: Expr, op: &str) -> MyResult<Expr> {
        // todo op check

        Ok(Expr::Binary {
            left: left.into(),
            right: right.into(),
            op: op.into(),
        })
    }

    fn visit_unary(thing: Expr, op: &str) -> MyResult<Expr> {
        // todo op check

        Ok(Expr::Unary {
            thing: thing.into(),
            op: op.into(),
        })
    }

    fn visit_cast(thing: Expr, ty: &str) -> MyResult<Expr> {
        // todo type check

        Ok(Expr::Cast {
            thing: thing.into(),
            ty: ty.into(),
        })
    }

    fn visit_primary(pair: Pair) -> MyResult<Expr> {
        // fixme might refactor this back into parse_expr or something
        match pair.as_rule() {
            Rule::literal => Ok(Expr::Literal(Literal::visit(pair.into_inner().next()?)?)),
            Rule::func_call => {
                let mut pairs = pair.into_inner();

                let name = pairs.next()?.as_str();
                let mut args = vec![];
                for pair in pairs {
                    args.push(Self::visit(pair)?);
                }

                Ok(Expr::FuncCall {
                    name: name.into(),
                    args,
                })
            }
            Rule::ident => Ok(Expr::Ident(pair.as_str().into())),
            Rule::paren_expr => Expr::visit(pair.into_inner().next()?),

            rule => Err(UnreachableRule(rule)),
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
    fn visit(pair: Pair) -> MyResult<Self> {
        match pair.as_rule() {
            Rule::float_literal => Ok(Self::Float(pair.as_str().parse()?)),
            Rule::int_literal => Ok(Self::Int(pair.as_str().parse()?)),
            Rule::bool_literal => Ok(Self::Bool(pair.as_str().parse()?)),
            Rule::char_literal => Ok(Self::Char(pair.into_inner().next()?.as_str().parse()?)),
            Rule::str_literal => Ok(Self::Str(pair.into_inner().next()?.as_str().into())),

            rule => Err(UnreachableRule(rule)),
        }
    }
}

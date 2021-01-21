//! handle the painful process that is parsing expressions

#![allow(dead_code)]

use crate::error::MyError::UnreachableRule;
use crate::error::MyResult;
use crate::{Pair, Rule};

pub fn parse_expr(pair: Pair) -> MyResult<Expr> {
    match pair.as_rule() {
        Rule::expr => parse_expr(pair.into_inner().next()?),
        Rule::equality_expr | Rule::compare_expr | Rule::add_expr | Rule::mul_expr => {
            // left assoc
            let mut pairs = pair.into_inner();

            let mut left = parse_expr(pairs.next()?)?;
            while let Some(pair) = pairs.next() {
                let op = pair.as_str();
                let right = parse_expr(pairs.next()?)?;

                left = parse_binary(left, right, op)?;
            }

            Ok(left)
        }
        Rule::unary_expr => {
            // right assoc
            let mut rev_pairs = pair.into_inner().rev();

            let mut thing = parse_expr(rev_pairs.next()?)?;
            for pair in rev_pairs {
                let op = pair.as_str();

                thing = parse_unary(thing, op)?;
            }

            Ok(thing)
        }
        Rule::cast_expr => {
            // left assoc
            let mut pairs = pair.into_inner();

            let mut thing = parse_expr(pairs.next()?)?;
            for pair in pairs {
                let ty = pair.as_str();

                thing = parse_cast(thing, ty)?;
            }

            Ok(thing)
        }

        Rule::primary_expr => parse_primary(pair.into_inner().next()?),

        rule => Err(UnreachableRule(rule)),
    }
}

fn parse_binary(left: Expr, right: Expr, op: &str) -> MyResult<Expr> {
    // todo op check

    Ok(Expr::Binary {
        left: left.into(),
        right: right.into(),
        op: op.into(),
    })
}

fn parse_unary(thing: Expr, op: &str) -> MyResult<Expr> {
    // todo op check

    Ok(Expr::Unary {
        thing: thing.into(),
        op: op.into(),
    })
}

fn parse_cast(thing: Expr, ty: &str) -> MyResult<Expr> {
    // todo type check

    Ok(Expr::Cast {
        thing: thing.into(),
        ty: ty.into(),
    })
}

fn parse_primary(pair: Pair) -> MyResult<Expr> {
    // fixme might refactor this back into parse_expr or something
    match pair.as_rule() {
        Rule::literal => Ok(Expr::Literal(parse_literal(pair.into_inner().next()?)?)),
        Rule::func_call => Err(anyhow::anyhow!("func_call").into()),
        Rule::ident => Ok(Expr::Ident(pair.as_str().into())),
        Rule::paren_expr => parse_expr(pair.into_inner().next()?),

        rule => Err(UnreachableRule(rule)),
    }
}

fn parse_literal(pair: Pair) -> MyResult<Literal> {
    match pair.as_rule() {
        Rule::float_literal => Ok(Literal::Float(pair.as_str().parse()?)),
        Rule::int_literal => Ok(Literal::Int(pair.as_str().parse()?)),
        Rule::bool_literal => Ok(Literal::Bool(pair.as_str().parse()?)),
        Rule::char_literal => Ok(Literal::Char(pair.into_inner().next()?.as_str().parse()?)),
        Rule::str_literal => Ok(Literal::Str(pair.into_inner().next()?.as_str().into())),

        rule => Err(UnreachableRule(rule)),
    }
}

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

#[derive(Debug, Clone)]
pub enum Literal {
    Float(f64),
    Int(i64),
    Bool(bool),
    Char(char),
    Str(String),
}

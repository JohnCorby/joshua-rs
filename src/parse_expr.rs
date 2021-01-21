//! handle the painful process that is parsing expressions

#![allow(dead_code)]

use crate::{Pair, Rule};
use anyhow::*;

pub fn parse_expr(pair: Pair) -> Result<Expr> {
    Ok(match pair.as_rule() {
        Rule::expr => parse_expr(pair.into_inner().next().context("next is none")?)?,
        Rule::equality_expr | Rule::compare_expr | Rule::add_expr | Rule::mul_expr => {
            // left assoc
            let mut pairs = pair.into_inner();

            let mut left = parse_expr(pairs.next().context("next is none")?)?;
            while let Some(pair) = pairs.next() {
                let op = pair.as_str();
                let right = parse_expr(pairs.next().context("next is none")?)?;

                left = parse_binary(left, right, op)?;
            }

            left
        }
        Rule::unary_expr => {
            // right assoc
            let mut rev_pairs = pair.into_inner().rev();

            let mut thing = parse_expr(rev_pairs.next().context("next is none")?)?;
            for pair in rev_pairs {
                let op = pair.as_str();

                thing = parse_unary(thing, op)?;
            }

            thing
        }
        Rule::cast_expr => {
            // left assoc
            let mut pairs = pair.into_inner();

            let mut thing = parse_expr(pairs.next().context("next is none")?)?;
            for pair in pairs {
                let ty = pair.as_str();

                thing = parse_cast(thing, ty)?;
            }

            thing
        }

        Rule::primary_expr => parse_primary(pair.into_inner().next().context("next is none")?)?,

        rule => bail!("rule {:?} ureachable", rule),
    })
}

fn parse_binary(left: Expr, right: Expr, op: &str) -> Result<Expr> {
    // todo op check

    Ok(Expr::Binary {
        left: left.into(),
        right: right.into(),
        op: op.into(),
    })
}

fn parse_unary(thing: Expr, op: &str) -> Result<Expr> {
    // todo op check

    Ok(Expr::Unary {
        thing: thing.into(),
        op: op.into(),
    })
}

fn parse_cast(thing: Expr, ty: &str) -> Result<Expr> {
    // todo type check

    Ok(Expr::Cast {
        thing: thing.into(),
        ty: ty.into(),
    })
}

fn parse_primary(pair: Pair) -> Result<Expr> {
    // fixme might refactor this back into parse_expr or something
    Ok(match pair.as_rule() {
        Rule::literal => Expr::Literal(parse_literal(
            pair.into_inner().next().context("next is none")?,
        )?),
        Rule::func_call => bail!("func_call"),
        Rule::ident => bail!("ident"),
        Rule::paren_expr => bail!("paren_expr"),

        rule => bail!("rule {:?} unreachable", rule),
    })
}

fn parse_literal(pair: Pair) -> Result<Literal> {
    Ok(match pair.as_rule() {
        Rule::float_literal => Literal::Float(pair.as_str().parse()?),
        Rule::int_literal => Literal::Int(pair.as_str().parse()?),
        Rule::bool_literal => Literal::Bool(pair.as_str().parse()?),
        Rule::char_literal => Literal::Char(
            pair.into_inner()
                .next()
                .context("next is none")?
                .as_str()
                .parse()?,
        ),
        Rule::str_literal => Literal::Str(
            pair.into_inner()
                .next()
                .context("next is none")?
                .as_str()
                .into(),
        ),

        rule => bail!("rule {:?} unreachable", rule),
    })
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
    Call {
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

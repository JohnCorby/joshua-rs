//! handle the painful process that is parsing expressions

use crate::error::{unexpected_rule, MyResult};
use crate::gen::Gen;
use crate::parse::{Pair, Rule};
use crate::pos::{AsPos, HasPos, Pos};
use crate::scope::Scope;
use crate::statement::FuncCall;
use crate::ty::Type;
use crate::util::PairExt;
use crate::visit::Visit;

#[derive(Debug, Clone)]
pub enum Expr {
    Binary {
        pos: Pos,
        left: Box<Expr>,
        op: String,
        right: Box<Expr>,
    },
    Unary {
        pos: Pos,
        op: String,
        thing: Box<Expr>,
    },
    Cast {
        pos: Pos,
        thing: Box<Expr>,
        ty: Type,
    },

    // primary
    Literal(Literal),
    FuncCall(FuncCall),
    Var {
        pos: Pos,
        name: String,
    },
}

impl Visit for Expr {
    fn visit_impl(pair: Pair) -> Self {
        match pair.as_rule() {
            Rule::expr => pair.into_inner().next().unwrap().visit(),
            Rule::equality_expr | Rule::compare_expr | Rule::add_expr | Rule::mul_expr => {
                let pos = pair.as_pos();
                // left assoc
                let mut pairs = pair.into_inner();

                let mut left: Expr = pairs.next().unwrap().visit();
                while let Some(op) = pairs.next() {
                    left = Self::Binary {
                        pos,
                        left: left.into(),
                        op: op.as_str().into(),
                        right: pairs.next().unwrap().visit::<Expr>().into(),
                    };
                }

                left
            }
            Rule::unary_expr => {
                let pos = pair.as_pos();
                // right assoc
                let mut rev_pairs = pair.into_inner().rev();

                let mut thing: Expr = rev_pairs.next().unwrap().visit();
                for op in rev_pairs {
                    thing = Self::Unary {
                        pos,
                        op: op.as_str().into(),
                        thing: thing.into(),
                    };
                }

                thing
            }
            Rule::cast_expr => {
                let pos = pair.as_pos();
                // left assoc
                let mut pairs = pair.into_inner();

                let mut thing: Expr = pairs.next().unwrap().visit();
                for ty in pairs {
                    thing = Self::Cast {
                        pos,
                        thing: thing.into(),
                        ty: ty.visit(),
                    };
                }

                thing
            }

            // primary
            Rule::float_literal
            | Rule::int_literal
            | Rule::bool_literal
            | Rule::char_literal
            | Rule::str_literal => Self::Literal(pair.visit()),
            Rule::func_call => Self::FuncCall(pair.visit()),
            Rule::ident => Self::Var {
                pos: pair.as_pos(),
                name: pair.as_str().into(),
            },

            rule => unexpected_rule(rule),
        }
    }
}

impl HasPos for Expr {
    fn pos(&self) -> Pos {
        match self {
            Expr::Binary { pos, .. } => *pos,
            Expr::Unary { pos, .. } => *pos,
            Expr::Cast { pos, .. } => *pos,
            Expr::Literal(literal) => literal.pos(),
            Expr::FuncCall(func_call) => func_call.pos(),
            Expr::Var { pos, .. } => *pos,
        }
    }
}
impl Gen for Expr {
    fn gen_impl(self) -> MyResult<String> {
        // todo Type eq check with ops
        //  and eventually overloading ops via func check if op check fails or something
        Ok(match self {
            Self::Binary {
                left, op, right, ..
            } => format!("({} {} {})", left.gen()?, op, right.gen()?),
            Self::Unary { op, thing, .. } => format!("({}{})", op, thing.gen()?),
            Self::Cast { thing, ty, .. } => {
                format!("(({}) {})", ty.gen()?, thing.gen()?)
            }
            Self::Literal(literal) => literal.gen()?,
            Self::FuncCall(func_call) => func_call.gen()?,
            Self::Var { name, .. } => {
                Scope::get_var(&name)?;
                name
            }
        })
    }
}

#[derive(Debug, Clone)]
pub enum Literal {
    Float { pos: Pos, value: f64 },
    Int { pos: Pos, value: i64 },
    Bool { pos: Pos, value: bool },
    Char { pos: Pos, value: char },
    Str { pos: Pos, value: String },
}

impl Visit for Literal {
    fn visit_impl(pair: Pair) -> Self {
        let pos = pair.as_pos();
        match pair.as_rule() {
            Rule::float_literal => Self::Float {
                pos,
                value: pair.as_str().parse().unwrap(),
            },
            Rule::int_literal => Self::Int {
                pos,
                value: pair.as_str().parse().unwrap(),
            },
            Rule::bool_literal => Self::Bool {
                pos,
                value: pair.as_str().parse().unwrap(),
            },
            Rule::char_literal => Self::Char {
                pos,
                value: pair.into_inner().next().unwrap().as_str().parse().unwrap(),
            },
            Rule::str_literal => Self::Str {
                pos,
                value: pair.into_inner().next().unwrap().as_str().into(),
            },

            rule => unexpected_rule(rule),
        }
    }
}

impl HasPos for Literal {
    fn pos(&self) -> Pos {
        match self {
            Literal::Float { pos, .. } => *pos,
            Literal::Int { pos, .. } => *pos,
            Literal::Bool { pos, .. } => *pos,
            Literal::Char { pos, .. } => *pos,
            Literal::Str { pos, .. } => *pos,
        }
    }
}
impl Gen for Literal {
    fn gen_impl(self) -> MyResult<String> {
        Ok(match self {
            Literal::Float { value: float, .. } => float.to_string(),
            Literal::Int { value: int, .. } => int.to_string(),
            Literal::Bool { value: bool, .. } => (bool as u8).to_string(),
            Literal::Char { value: char, .. } => format!("'{}'", char),
            Literal::Str { value: str, .. } => format!("\"{}\"", str),
        })
    }
}

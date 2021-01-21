#![allow(dead_code)]

use crate::error::MyError::UnreachableRule;
use crate::error::MyResult;
use crate::expr::Expr;
use crate::{Pair, Pairs, Rule};

/// take a parser pair an turn it into ourselves
pub trait Parsable {
    fn parse(pair: Pair) -> MyResult<Self>
    where
        Self: Sized;
}

pub fn parse(defines: Pairs) -> MyResult<Program> {
    let mut program = Program::new();
    for define in defines {
        if !matches!(
            define.as_rule(),
            Rule::struct_define | Rule::func_define | Rule::var_define
        ) {
            return Err(anyhow::anyhow!("{:?} is not a define", define).into());
        }

        program.push(Define::parse(define)?);
    }
    Ok(program)
}

type Program = Vec<Define>;

#[derive(Debug, Clone)]
pub enum Define {
    Struct {
        name: String,
        body: Vec<Define>,
    },
    Func {
        ty: String,
        name: String,
        args: Vec<VarDefine>,
    },
    Var(VarDefine),
}
impl Parsable for Define {
    fn parse(pair: Pair) -> MyResult<Self> {
        match pair.as_rule() {
            Rule::struct_define => todo!(),
            Rule::func_define => todo!(),
            Rule::var_define => todo!(),

            rule => Err(UnreachableRule(rule)),
        }
    }
}

#[derive(Debug, Clone)]
struct VarDefine {
    ty: String,
    name: String,
    value: Option<Expr>,
}

type Block = Vec<Statement>;
#[derive(Debug, Clone)]
enum Statement {
    Return {
        value: Option<Expr>,
    },
    Break,
    Continue,
    If {
        cond: Expr,
        then: Block,
        otherwise: Block,
    },
    Until {
        cond: Expr,
        block: Block,
    },
    For {
        init: VarDefine,
        cond: Expr,
        update: Box<Statement>,
        block: Block,
    },
    FuncCall {
        name: String,
        args: Vec<Expr>,
    },
    VarAssign {
        name: String,
        value: Expr,
    },
    VarDefine(VarDefine),
}

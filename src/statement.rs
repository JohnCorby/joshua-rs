use crate::define::VarDefine;
use crate::error::MyResult;
use crate::expr::Expr;
use crate::util::pair_inner_checked;
use crate::visit::Visit;
use crate::{Pair, Pairs, Rule};

pub type Block = Vec<Statement>;

pub fn visit_block(pair: Pair) -> MyResult<Block> {
    let pairs = pair_inner_checked(pair, Rule::block)?;

    let mut block = Block::new();
    for pair in pairs {
        block.push(Statement::visit(pair)?)
    }
    Ok(block)
}

#[derive(Debug, Clone)]
pub enum Statement {
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

impl Visit for Statement {
    fn visit(pair: Pair) -> MyResult<Self> {
        let pairs = pair_inner_checked(pair, Rule::statement)?;
        todo!()
    }
}

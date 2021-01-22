use crate::define::VarDefine;
use crate::error::MyResult;
use crate::expr::Expr;
use crate::util::{PairExt, PairsExt};
use crate::visit::Visit;
use crate::{Pair, Rule};

pub type Block = Vec<Statement>;

pub fn visit_block(pair: Pair) -> MyResult<Block> {
    pair.into_inner_checked(Rule::block)?.visit_rest()
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

impl Visit<'_> for Statement {
    fn visit(pair: Pair) -> MyResult<Self> {
        let pair = pair.into_inner_checked(Rule::statement)?.next()?;

        match pair.as_rule() {
            Rule::ret => Ok(Self::Return {
                value: pair.into_inner().next().map(Pair::visit).transpose()?,
            }),
            Rule::brk => Ok(Self::Break),
            Rule::cont => Ok(Self::Continue),
            Rule::iff => {
                let mut pairs = pair.into_inner();

                Ok(Self::If {
                    cond: pairs.next()?.visit()?,
                    then: visit_block(pairs.next()?)?,
                    otherwise: visit_block(pairs.next()?)?,
                })
            }
            Rule::until => {
                let mut pairs = pair.into_inner();

                Ok(Self::Until {
                    cond: pairs.next()?.visit()?,
                    block: visit_block(pairs.next()?)?,
                })
            }
            Rule::forr => {
                let mut pairs = pair.into_inner();

                Ok(Self::For {
                    init: pairs.next()?.visit()?,
                    cond: pairs.next()?.visit()?,
                    update: pairs.next()?.visit::<Statement>()?.into(),
                    block: visit_block(pairs.next()?)?,
                })
            }
            Rule::func_call => {
                let mut pairs = pair.into_inner();

                Ok(Self::FuncCall {
                    name: pairs.next()?.as_str().into(),
                    args: pairs.visit_rest()?,
                })
            }
            Rule::var_assign => {
                let mut pairs = pair.into_inner();

                Ok(Self::VarAssign {
                    name: pairs.next()?.as_str().into(),
                    value: pairs.next()?.visit()?,
                })
            }
            Rule::var_define => Ok(Self::VarDefine(pair.visit()?)),

            rule => Err(rule.into()),
        }
    }
}

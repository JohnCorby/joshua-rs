use crate::define::VarDefine;
use crate::error::{unexpected_rule, MyResult};
use crate::expr::Expr;
use crate::parse::{Pair, Rule};
use crate::util::{PairExt, PairsExt};
use crate::visit::Visit;
use crate::Ref;

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
        update: Ref<Statement>,
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
        Ok(match pair.as_rule() {
            Rule::ret => Self::Return {
                value: pair.into_inner().next().map(Pair::visit).transpose()?,
            },
            Rule::brk => Self::Break,
            Rule::cont => Self::Continue,
            Rule::iff => {
                let mut pairs = pair.into_inner();

                Self::If {
                    cond: pairs.next()?.visit()?,
                    then: pairs.next()?.visit()?,
                    otherwise: pairs.next()?.visit()?,
                }
            }
            Rule::until => {
                let mut pairs = pair.into_inner();

                Self::Until {
                    cond: pairs.next()?.visit()?,
                    block: pairs.next()?.visit()?,
                }
            }
            Rule::forr => {
                let mut pairs = pair.into_inner();

                Self::For {
                    init: pairs.next()?.visit()?,
                    cond: pairs.next()?.visit()?,
                    update: pairs.next()?.visit::<Statement>()?.into(),
                    block: pairs.next()?.visit()?,
                }
            }
            Rule::func_call => {
                let mut pairs = pair.into_inner();

                Self::FuncCall {
                    name: pairs.next()?.as_str().into(),
                    args: pairs.visit_rest()?,
                }
            }
            Rule::var_assign => {
                let mut pairs = pair.into_inner();

                Self::VarAssign {
                    name: pairs.next()?.as_str().into(),
                    value: pairs.next()?.visit()?,
                }
            }
            Rule::var_define => Self::VarDefine(pair.visit()?),

            rule => unexpected_rule(rule)?,
        })
    }
}

pub type Block = Vec<Statement>;

impl Visit for Block {
    fn visit(pair: Pair) -> MyResult<Self> {
        pair.into_inner_checked(Rule::block)?.visit_rest()
    }
}

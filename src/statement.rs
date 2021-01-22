use crate::define::VarDefine;
use crate::error::MyResult;
use crate::expr::Expr;
use crate::util::pair_inner_checked;
use crate::visit::Visit;
use crate::{Pair, Rule};

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
        let pair = pair_inner_checked(pair, Rule::statement)?.next()?;

        match pair.as_rule() {
            Rule::ret => {
                let value = match pair.into_inner().next() {
                    Some(pair) => Some(Expr::visit(pair)?),
                    None => None,
                };

                Ok(Self::Return { value })
            }
            Rule::brk => Ok(Self::Break),
            Rule::cont => Ok(Self::Continue),
            Rule::iff => {
                let mut pairs = pair.into_inner();

                let cond = Expr::visit(pairs.next()?)?;
                let then = visit_block(pairs.next()?)?;
                let otherwise = visit_block(pairs.next()?)?;

                Ok(Self::If {
                    cond,
                    then,
                    otherwise,
                })
            }
            Rule::until => {
                let mut pairs = pair.into_inner();

                let cond = Expr::visit(pairs.next()?)?;
                let block = visit_block(pairs.next()?)?;

                Ok(Self::Until { cond, block })
            }
            Rule::forr => {
                let mut pairs = pair.into_inner();

                let init = VarDefine::visit(pairs.next()?)?;
                let cond = Expr::visit(pairs.next()?)?;
                let update = Self::visit(pairs.next()?)?.into();
                let block = visit_block(pairs.next()?)?;

                Ok(Self::For {
                    init,
                    cond,
                    update,
                    block,
                })
            }
            Rule::func_call => {
                let mut pairs = pair.into_inner();

                let name = pairs.next()?.as_str().into();
                let mut args = vec![];
                for pair in pairs {
                    args.push(Expr::visit(pair)?);
                }

                Ok(Self::FuncCall { name, args })
            }
            Rule::var_assign => {
                let mut pairs = pair.into_inner();

                let name = pairs.next()?.as_str().into();
                let value = Expr::visit(pairs.next()?)?;

                Ok(Self::VarAssign { name, value })
            }
            Rule::var_define => Ok(Self::VarDefine(VarDefine::visit(pair)?)),

            rule => Err(rule.into()),
        }
    }
}

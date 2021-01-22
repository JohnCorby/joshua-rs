use crate::error::{rule_unreachable, MyResult};
use crate::expr::Expr;
use crate::statement::{visit_block, Block};
use crate::util::{PairExt, PairsExt};
use crate::visit::Visit;
use crate::{Pair, Rule};

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
        body: Block,
    },
    Var(VarDefine),
}

impl Visit for Define {
    fn visit(pair: Pair) -> MyResult<Self> {
        let pair = pair.into_inner_checked(Rule::define)?.next()?;

        Ok(match pair.as_rule() {
            Rule::struct_define => {
                let mut pairs = pair.into_inner();

                Self::Struct {
                    name: pairs.next()?.as_str().into(),
                    body: pairs.visit_rest()?,
                }
            }
            Rule::func_define => {
                let mut pairs = pair.into_inner();

                let ty = pairs.next()?.as_str().into();
                let name = pairs.next()?.as_str().into();
                let mut args = vec![];
                while pairs.peek().is_some() && pairs.peek()?.as_rule() == Rule::var_define {
                    args.push(pairs.next()?.visit()?)
                }
                let body = visit_block(pairs.next()?)?;

                Self::Func {
                    ty,
                    name,
                    args,
                    body,
                }
            }
            Rule::var_define => Self::Var(pair.visit()?),

            rule => rule_unreachable(rule)?,
        })
    }
}

#[derive(Debug, Clone)]
pub struct VarDefine {
    ty: String,
    name: String,
    value: Option<Expr>,
}

impl Visit for VarDefine {
    fn visit(pair: Pair) -> MyResult<Self> {
        let mut pairs = pair.into_inner_checked(Rule::var_define)?;

        Ok(Self {
            ty: pairs.next()?.as_str().into(),
            name: pairs.next()?.as_str().into(),
            value: pairs.next().map(Pair::visit).transpose()?,
        })
    }
}

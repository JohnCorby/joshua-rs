use crate::error::{unexpected_rule, MyResult};
use crate::expr::Expr;
use crate::parse::{Pair, Rule};
use crate::statement::Block;
use crate::ty::Type;
use crate::util::{PairExt, PairsExt};
use crate::visit::Visit;

#[derive(Debug, Clone)]
pub enum Define {
    Struct {
        name: String,
        body: Vec<Define>,
    },
    Func {
        ty: Type,
        name: String,
        args: Vec<VarDefine>,
        body: Block,
    },
    Var(VarDefine),
}

impl Visit for Define {
    fn visit_impl(pair: Pair) -> MyResult<Self> {
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

                let ty = pairs.next()?.visit()?;
                let name = pairs.next()?.as_str().into();
                let mut args = vec![];
                while pairs.peek().is_some() && pairs.peek().unwrap().as_rule() == Rule::var_define
                {
                    args.push(pairs.next().unwrap().visit()?)
                }
                let body = pairs.next()?.visit()?;

                Self::Func {
                    ty,
                    name,
                    args,
                    body,
                }
            }
            Rule::var_define => Self::Var(pair.visit()?),

            rule => unexpected_rule(rule)?,
        })
    }
}

#[derive(Debug, Clone)]
pub struct VarDefine {
    ty: Type,
    name: String,
    value: Option<Expr>,
}

impl Visit for VarDefine {
    fn visit_impl(pair: Pair) -> MyResult<Self> {
        let mut pairs = pair.into_inner_checked(Rule::var_define)?;

        Ok(Self {
            ty: pairs.next()?.visit()?,
            name: pairs.next()?.as_str().into(),
            value: pairs.next().map(Pair::visit).transpose()?,
        })
    }
}

use crate::error::MyError::UnreachableRule;
use crate::error::MyResult;
use crate::expr::Expr;
use crate::statement::{visit_block, Block};
use crate::util::pair_inner_checked;
use crate::visit::Visit;
use crate::{Pair, Rule};
use std::backtrace::Backtrace;

pub type Program = Vec<Define>;

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
        let pair = pair_inner_checked(pair, Rule::define)?.next()?;

        match pair.as_rule() {
            Rule::struct_define => todo!(),
            Rule::func_define => {
                let mut pairs = pair.into_inner();

                let ty = pairs.next()?.as_str();
                let name = pairs.next()?.as_str();
                let mut args = vec![];
                while pairs.peek().is_some() && pairs.peek()?.as_rule() == Rule::var_define {
                    args.push(VarDefine::visit(pairs.next()?)?)
                }
                let body = visit_block(pairs.next()?)?;

                Ok(Self::Func {
                    ty: ty.into(),
                    name: name.into(),
                    args,
                    body,
                })
            }
            Rule::var_define => todo!(),

            rule => Err(rule.into()),
        }
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
        let mut pairs = pair_inner_checked(pair, Rule::var_define)?;

        let ty = pairs.next()?.as_str();
        let name = pairs.next()?.as_str();
        let value = match pairs.next() {
            Some(pair) => Some(Expr::visit(pair)?),
            None => None,
        };

        Ok(Self {
            ty: ty.into(),
            name: name.into(),
            value,
        })
    }
}

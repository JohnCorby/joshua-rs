use crate::error::MyError::UnreachableRule;
use crate::error::MyResult;
use crate::expr::Expr;
use crate::statement::{visit_block, Block};
use crate::visit::Visit;
use crate::{Pair, Rule};

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
        crate::check_pair!(pair, Rule::define);
        let pair = pair.into_inner().next()?;

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

            rule => Err(UnreachableRule(rule)),
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
        crate::check_pair!(pair, Rule::var_define);
        let pairs = pair.into_inner();
        todo!()
    }
}

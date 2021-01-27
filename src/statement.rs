use crate::define::VarDefine;
use crate::error::{unexpected_rule, MyResult};
use crate::expr::Expr;
use crate::gen::Gen;
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
        otherwise: Option<Block>,
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
    fn visit_impl(pair: Pair) -> MyResult<Self> {
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
                    otherwise: pairs.next().map(Pair::visit).transpose()?,
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

impl Gen for Statement {
    fn gen(self) -> MyResult<String> {
        Ok(match self {
            Statement::Return { value } => {
                let mut s = String::from("return");
                if let Some(value) = value {
                    s.push_str(&value.gen()?);
                }
                s.push(';');
                s
            }
            Statement::Break => "break;".into(),
            Statement::Continue => "continue".into(),
            Statement::If {
                cond,
                then,
                otherwise,
            } => {
                let mut s = format!("if({}) {}", cond.gen()?, then.gen()?);
                if let Some(otherwise) = otherwise {
                    s.push_str(&otherwise.gen()?);
                }
                s
            }
            Statement::Until { cond, block } => {
                format!("while(!({})) {}", cond.gen()?, block.gen()?)
            }
            Statement::For {
                init,
                cond,
                update,
                block,
            } => format!(
                "for({}; {}; {}) {}",
                init.gen()?,
                cond.gen()?,
                Statement::clone(&update).gen()?,
                block.gen()?
            ),
            Statement::FuncCall { name, args } => format!(
                "{}({})",
                name,
                args.into_iter()
                    .map(Expr::gen)
                    .collect::<MyResult<Vec<_>>>()?
                    .join(", "),
            ),
            Statement::VarAssign { name, value } => format!("{} = {};", name, value.gen()?),
            Statement::VarDefine(var_define) => var_define.gen()?,
        })
    }
}

pub type Block = Vec<Statement>;

impl Visit for Block {
    fn visit_impl(pair: Pair) -> MyResult<Self> {
        pair.into_inner_checked(Rule::block)?.visit_rest()
    }
}

impl Gen for Block {
    fn gen(self) -> MyResult<String> {
        Ok(format!(
            "{{\n{}\n}}",
            self.into_iter()
                .map(Statement::gen)
                .collect::<MyResult<Vec<_>>>()?
                .join("\n")
        ))
    }
}

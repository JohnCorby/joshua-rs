use crate::define::VarDefine;
use crate::error::{unexpected_rule, MyResult};
use crate::expr::Expr;
use crate::gen::Gen;
use crate::parse::{Pair, Rule};
use crate::pos::{AsPos, Pos};
use crate::scope::Scope;
use crate::util::{PairExt, PairsExt};
use crate::visit::Visit;
use std::fmt::Write;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub enum Statement {
    Return {
        pos: Pos,
        value: Option<Expr>,
    },
    Break {
        pos: Pos,
    },
    Continue {
        pos: Pos,
    },
    If {
        pos: Pos,
        cond: Expr,
        then: Block,
        otherwise: Option<Block>,
    },
    Until {
        pos: Pos,
        cond: Expr,
        block: Block,
    },
    For {
        pos: Pos,
        init: VarDefine,
        cond: Expr,
        update: Rc<Statement>,
        block: Block,
    },
    FuncCall(FuncCall),
    VarAssign {
        pos: Pos,
        name: String,
        value: Expr,
    },
    VarDefine(VarDefine),
}

impl Visit for Statement {
    fn visit_impl(pair: Pair) -> MyResult<Self> {
        Ok(match pair.as_rule() {
            Rule::ret => Self::Return {
                pos: pair.as_pos(),
                value: pair.into_inner().next().map(Pair::visit).transpose()?,
            },
            Rule::brk => Self::Break { pos: pair.as_pos() },
            Rule::cont => Self::Continue { pos: pair.as_pos() },
            Rule::iff => {
                let pos = pair.as_pos();
                let mut pairs = pair.into_inner();

                Self::If {
                    pos,
                    cond: pairs.next()?.visit()?,
                    then: pairs.next()?.visit()?,
                    otherwise: pairs.next().map(Pair::visit).transpose()?,
                }
            }
            Rule::until => {
                let pos = pair.as_pos();
                let mut pairs = pair.into_inner();

                Self::Until {
                    pos,
                    cond: pairs.next()?.visit()?,
                    block: pairs.next()?.visit()?,
                }
            }
            Rule::forr => {
                let pos = pair.as_pos();
                let mut pairs = pair.into_inner();

                Self::For {
                    pos,
                    init: pairs.next()?.visit()?,
                    cond: pairs.next()?.visit()?,
                    update: pairs.next()?.visit::<Statement>()?.into(),
                    block: pairs.next()?.visit()?,
                }
            }
            Rule::func_call => Self::FuncCall(pair.visit()?),
            Rule::var_assign => {
                let pos = pair.as_pos();
                let mut pairs = pair.into_inner();

                Self::VarAssign {
                    pos,
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
    fn pos(&self) -> Pos {
        match self {
            Statement::Return { pos, .. } => pos.clone(),
            Statement::Break { pos } => pos.clone(),
            Statement::Continue { pos } => pos.clone(),
            Statement::If { pos, .. } => pos.clone(),
            Statement::Until { pos, .. } => pos.clone(),
            Statement::For { pos, .. } => pos.clone(),
            Statement::FuncCall(func_call) => func_call.pos(),
            Statement::VarAssign { pos, .. } => pos.clone(),
            Statement::VarDefine(var_define) => var_define.pos(),
        }
    }

    fn gen_impl(self) -> MyResult<String> {
        Ok(match self {
            Self::Return { value, .. } => {
                let mut s = String::from("return");
                if let Some(value) = value {
                    write!(s, " {}", value.gen()?)?;
                }
                s.push(';');
                s
            }
            Self::Break { .. } => "break;".into(),
            Self::Continue { .. } => "continue;".into(),
            Self::If {
                cond,
                then,
                otherwise,
                ..
            } => {
                let mut s = format!("if({}) {}", cond.gen()?, then.gen()?);
                if let Some(otherwise) = otherwise {
                    s.push_str(&otherwise.gen()?);
                }
                s
            }
            Self::Until { cond, block, .. } => {
                format!("while(!({})) {}", cond.gen()?, block.gen()?)
            }
            Self::For {
                init,
                cond,
                update,
                block,
                ..
            } => format!(
                "for({}; {}; {}) {}",
                init.gen()?,
                cond.gen()?,
                Statement::clone(&update).gen()?.strip_suffix(';')?,
                block.gen()?
            ),
            Self::FuncCall(func_call) => format!("{};", func_call.gen()?),
            Self::VarAssign { name, value, .. } => {
                Scope::get_var(&name)?;
                format!("{} = {};", name, value.gen()?)
            }
            Self::VarDefine(var_define) => format!("{};", var_define.gen()?),
        })
    }
}

#[derive(Debug, Clone)]
pub struct Block {
    pos: Pos,
    statements: Vec<Statement>,
}

impl Visit for Block {
    fn visit_impl(pair: Pair) -> MyResult<Self> {
        Ok(Self {
            pos: pair.as_pos(),
            statements: pair.into_inner_checked(Rule::block)?.visit_rest()?,
        })
    }
}

impl Gen for Block {
    fn pos(&self) -> Pos {
        self.pos.clone()
    }

    fn gen_impl(self) -> MyResult<String> {
        Scope::push();
        let result = Ok(format!(
            "{{\n{}\n}}",
            self.statements
                .into_iter()
                .map(Statement::gen)
                .collect::<MyResult<Vec<_>>>()?
                .join("\n")
        ));
        Scope::pop()?;
        result
    }
}

#[derive(Debug, Clone)]
pub struct FuncCall {
    pos: Pos,
    name: String,
    args: Vec<Expr>,
}

impl Visit for FuncCall {
    fn visit_impl(pair: Pair) -> MyResult<Self> {
        let pos = pair.as_pos();
        let mut pairs = pair.into_inner_checked(Rule::func_call)?;

        Ok(Self {
            pos,
            name: pairs.next()?.as_str().into(),
            args: pairs.visit_rest()?,
        })
    }
}

impl Gen for FuncCall {
    fn pos(&self) -> Pos {
        self.pos.clone()
    }

    fn gen_impl(self) -> MyResult<String> {
        Ok(format!(
            "{}({})",
            self.name,
            self.args
                .into_iter()
                .map(Expr::gen)
                .collect::<MyResult<Vec<_>>>()?
                .join(", "),
        ))
    }
}

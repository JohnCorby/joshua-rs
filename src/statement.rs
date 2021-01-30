use crate::define::VarDefine;
use crate::error::{unexpected_rule, MyError, MyResult};
use crate::expr::Expr;
use crate::gen::Gen;
use crate::parse::{Pair, Rule};
use crate::pos::{AsPos, HasPos, Pos};
use crate::scope::Scope;
use crate::util::{PairExt, PairsExt};
use crate::visit::Visit;
use std::fmt::Write;

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
        update: Box<Statement>,
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
    fn visit_impl(pair: Pair) -> Self {
        match pair.as_rule() {
            Rule::ret => Self::Return {
                pos: pair.as_pos(),
                value: pair.into_inner().next().map(Pair::visit),
            },
            Rule::brk => Self::Break { pos: pair.as_pos() },
            Rule::cont => Self::Continue { pos: pair.as_pos() },
            Rule::iff => {
                let pos = pair.as_pos();
                let mut pairs = pair.into_inner();

                Self::If {
                    pos,
                    cond: pairs.next().unwrap().visit(),
                    then: pairs.next().unwrap().visit(),
                    otherwise: pairs.next().map(Pair::visit),
                }
            }
            Rule::until => {
                let pos = pair.as_pos();
                let mut pairs = pair.into_inner();

                Self::Until {
                    pos,
                    cond: pairs.next().unwrap().visit(),
                    block: pairs.next().unwrap().visit(),
                }
            }
            Rule::forr => {
                let pos = pair.as_pos();
                let mut pairs = pair.into_inner();

                Self::For {
                    pos,
                    init: pairs.next().unwrap().visit(),
                    cond: pairs.next().unwrap().visit(),
                    update: pairs.next().unwrap().visit::<Statement>().into(),
                    block: pairs.next().unwrap().visit(),
                }
            }
            Rule::func_call => Self::FuncCall(pair.visit()),
            Rule::var_assign => {
                let pos = pair.as_pos();
                let mut pairs = pair.into_inner();

                Self::VarAssign {
                    pos,
                    name: pairs.next().unwrap().as_str().into(),
                    value: pairs.next().unwrap().visit(),
                }
            }
            Rule::var_define => Self::VarDefine(pair.visit()),

            rule => unexpected_rule(rule),
        }
    }
}

impl HasPos for Statement {
    fn pos(&self) -> Pos {
        match self {
            Statement::Return { pos, .. } => *pos,
            Statement::Break { pos } => *pos,
            Statement::Continue { pos } => *pos,
            Statement::If { pos, .. } => *pos,
            Statement::Until { pos, .. } => *pos,
            Statement::For { pos, .. } => *pos,
            Statement::FuncCall(func_call) => func_call.pos(),
            Statement::VarAssign { pos, .. } => *pos,
            Statement::VarDefine(var_define) => var_define.pos(),
        }
    }
}
impl Gen for Statement {
    fn gen_impl(self) -> MyResult<String> {
        Ok(match self {
            Self::Return { value, .. } => {
                let mut s = String::from("return");
                if let Some(value) = value {
                    write!(s, " {}", value.gen()?).unwrap();
                }
                s.push(';');
                s
            }
            Self::Break { .. } => {
                if !Scope::current().is_loop() {
                    Err(MyError::from("break cant be used outside of loops"))?;
                }
                "break;".into()
            }
            Self::Continue { .. } => {
                if !Scope::current().is_loop() {
                    Err(MyError::from("continue cant be used outside of loops"))?;
                }
                "continue;".into()
            }
            Self::If {
                cond,
                then,
                otherwise,
                ..
            } => {
                let mut s = format!("if({}) ", cond.gen()?);
                Scope::push(false);
                s.write_str(&then.gen()?).unwrap();
                Scope::pop();
                if let Some(otherwise) = otherwise {
                    Scope::push(false);
                    s.push_str(&otherwise.gen()?);
                    Scope::pop();
                }
                s
            }
            Self::Until { cond, block, .. } => {
                let mut s = format!("while(!({})) ", cond.gen()?);
                Scope::push(true);
                s.write_str(&block.gen()?).unwrap();
                Scope::pop();
                s
            }
            Self::For {
                init,
                cond,
                update,
                block,
                ..
            } => {
                Scope::push(true);
                let s = format!(
                    "for({}; {}; {}) {}",
                    init.gen()?,
                    cond.gen()?,
                    update.gen()?.strip_suffix(';')?,
                    block.gen()?
                );
                Scope::pop();
                s
            }
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
    fn visit_impl(pair: Pair) -> Self {
        Self {
            pos: pair.as_pos(),
            statements: pair.into_inner_checked(Rule::block).visit_rest(),
        }
    }
}

impl HasPos for Block {
    fn pos(&self) -> Pos {
        self.pos
    }
}
impl Gen for Block {
    fn gen_impl(self) -> MyResult<String> {
        Ok(format!(
            "{{\n{}\n}}",
            self.statements
                .into_iter()
                .map(Statement::gen)
                .collect::<MyResult<Vec<_>>>()?
                .join("\n")
        ))
    }
}

#[derive(Debug, Clone)]
pub struct FuncCall {
    pos: Pos,
    name: String,
    args: Vec<Expr>,
}

impl Visit for FuncCall {
    fn visit_impl(pair: Pair) -> Self {
        let pos = pair.as_pos();
        let mut pairs = pair.into_inner_checked(Rule::func_call);

        Self {
            pos,
            name: pairs.next().unwrap().as_str().into(),
            args: pairs.visit_rest(),
        }
    }
}

impl HasPos for FuncCall {
    fn pos(&self) -> Pos {
        self.pos
    }
}
impl Gen for FuncCall {
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

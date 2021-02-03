use crate::define::VarDefine;
use crate::error::{unexpected_rule, MyError, MyResult};
use crate::expr::Expr;
use crate::gen::Gen;
use crate::parse::{Pair, Rule};
use crate::pos::{Pos, WithPos};
use crate::scope::Scope;
use crate::ty::{HasType, PrimitiveType, Type};
use crate::util::{PairExt, PairsExt};
use crate::visit::Visit;
use crate::with::ToWith;
use std::fmt::Write;

#[derive(Debug, Clone)]
pub enum Statement {
    Return(Option<WithPos<Expr>>),
    Break,
    Continue,
    If {
        cond: WithPos<Expr>,
        then: WithPos<Block>,
        otherwise: Option<WithPos<Block>>,
    },
    Until {
        cond: WithPos<Expr>,
        block: WithPos<Block>,
    },
    For {
        init: WithPos<VarDefine>,
        cond: WithPos<Expr>,
        update: Box<WithPos<Statement>>,
        block: WithPos<Block>,
    },
    FuncCall(FuncCall),
    VarAssign {
        name: WithPos<String>,
        value: WithPos<Expr>,
    },
    VarDefine(VarDefine),
}

impl Visit for Statement {
    fn visit_impl(pair: Pair) -> Self {
        match pair.as_rule() {
            Rule::ret => Self::Return(pair.into_inner().next().map(Pair::visit)),
            Rule::brk => Self::Break,
            Rule::cont => Self::Continue,
            Rule::iff => {
                let mut pairs = pair.into_inner();

                Self::If {
                    cond: pairs.next().unwrap().visit(),
                    then: pairs.next().unwrap().visit(),
                    otherwise: pairs.next().map(Pair::visit),
                }
            }
            Rule::until => {
                let mut pairs = pair.into_inner();

                Self::Until {
                    cond: pairs.next().unwrap().visit(),
                    block: pairs.next().unwrap().visit(),
                }
            }
            Rule::forr => {
                let mut pairs = pair.into_inner();

                Self::For {
                    init: pairs.next().unwrap().visit(),
                    cond: pairs.next().unwrap().visit(),
                    update: pairs.next().unwrap().visit::<Statement>().into(),
                    block: pairs.next().unwrap().visit(),
                }
            }
            Rule::func_call => Self::FuncCall(pair.visit().inner),
            Rule::var_assign => {
                let mut pairs = pair.into_inner();

                Self::VarAssign {
                    name: pairs.next().unwrap().as_str_with_pos(),
                    value: pairs.next().unwrap().visit(),
                }
            }
            Rule::var_define => Self::VarDefine(pair.visit().inner),

            rule => unexpected_rule(rule),
        }
    }
}

impl Gen for WithPos<Statement> {
    fn pos(&self) -> Pos {
        self.extra
    }

    fn gen_impl(self) -> MyResult<String> {
        Ok(match self.inner {
            Statement::Return(value) => {
                let mut s = String::from("return");
                if let Some(value) = value.clone() {
                    write!(s, " {}", value.gen()?).unwrap();
                }
                s.push(';');

                // type check
                value
                    .map(|value| value.ty())
                    .unwrap_or_else(|| PrimitiveType::Void.ty())
                    .check(&Scope::current().func_return_type())?;

                s
            }
            Statement::Break => {
                if !Scope::current().in_loop() {
                    return Err(MyError::from("break cant be used outside of loops"));
                }
                "break;".into()
            }
            Statement::Continue => {
                if !Scope::current().in_loop() {
                    return Err(MyError::from("continue cant be used outside of loops"));
                }
                "continue;".into()
            }
            Statement::If {
                cond,
                then,
                otherwise,
            } => {
                let mut s = format!("if({}) ", cond.clone().gen()?);
                let scope = Scope::new(false, None);
                s.write_str(&then.gen()?).unwrap();
                drop(scope);
                if let Some(otherwise) = otherwise {
                    let scope = Scope::new(false, None);
                    s.push_str(&otherwise.gen()?);
                    drop(scope);
                }

                // type check
                cond.ty().check(&PrimitiveType::Bool.ty())?;

                s
            }
            Statement::Until { cond, block, .. } => {
                let mut s = format!("while(!({})) ", cond.clone().gen()?);
                let scope = Scope::new(true, None);
                s.write_str(&block.gen()?).unwrap();
                drop(scope);

                // type check
                cond.ty().check(&PrimitiveType::Bool.ty())?;

                s
            }
            Statement::For {
                init,
                cond,
                update,
                block,
                ..
            } => {
                let scope = Scope::new(true, None);
                let s = format!(
                    "for({}; {}; {}) {}",
                    init.gen()?,
                    cond.clone().gen()?,
                    update.gen()?.strip_suffix(';')?,
                    block.gen()?
                );

                // type check
                cond.ty().check(&PrimitiveType::Bool.ty())?;

                drop(scope);
                s
            }
            Statement::FuncCall(func_call) => format!("{};", func_call.with(self.extra).gen()?),
            Statement::VarAssign { name, value, .. } => {
                let s = format!("{} = {};", *name, value.clone().gen()?);
                // type check
                value
                    .ty()
                    .check(&Scope::current().get_var(name.inner)?.ty())?;
                s
            }
            Statement::VarDefine(var_define) => format!("{};", var_define.with(self.extra).gen()?),
        })
    }
}

pub type Block = Vec<WithPos<Statement>>;

impl Visit for Block {
    fn visit_impl(pair: Pair) -> Self {
        pair.into_inner_checked(Rule::block).visit_rest()
    }
}

impl Gen for WithPos<Block> {
    fn pos(&self) -> Pos {
        self.extra
    }

    fn gen_impl(self) -> MyResult<String> {
        Ok(format!(
            "{{\n{}\n}}",
            self.inner
                .into_iter()
                .map(Gen::gen)
                .collect::<MyResult<Vec<_>>>()?
                .join("\n")
        ))
    }
}

#[derive(Debug, Clone)]
pub struct FuncCall {
    name: WithPos<String>,
    args: Vec<WithPos<Expr>>,
}

impl Visit for FuncCall {
    fn visit_impl(pair: Pair) -> Self {
        let mut pairs = pair.into_inner_checked(Rule::func_call);

        Self {
            name: pairs.next().unwrap().as_str_with_pos(),
            args: pairs.visit_rest(),
        }
    }
}

impl Gen for WithPos<FuncCall> {
    fn pos(&self) -> Pos {
        self.extra
    }

    fn gen_impl(self) -> MyResult<String> {
        let s = format!(
            "{}({})",
            *self.name,
            self.args
                .clone()
                .into_iter()
                .map(Gen::gen)
                .collect::<MyResult<Vec<_>>>()?
                .join(", "),
        );

        // type check
        Scope::current().get_func(
            &*self.name,
            self.args.iter().map(|arg| arg.ty()).collect::<Vec<_>>(),
        )?;

        Ok(s)
    }
}

impl HasType for FuncCall {
    fn ty(&self) -> Type {
        const GET_FUNC_ERR: &str =
            "cant get func symbol for HasType even though this should have already been checked";
        Scope::current()
            .get_func(
                &*self.name,
                self.args.iter().map(|arg| arg.ty()).collect::<Vec<_>>(),
            )
            .expect(GET_FUNC_ERR)
            .ty()
    }
}

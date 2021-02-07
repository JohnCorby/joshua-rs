use crate::cached::CachedString;
use crate::define::VarDefine;
use crate::error::{unexpected_rule, MyError, MyResult};
use crate::expr::Expr;
use crate::parse::{Node, Rule};
use crate::pass::{Gen, Visit};
use crate::scope::Scope;
use crate::span::Span;
use crate::ty::{HasType, PrimitiveType, Type};
use crate::with::{ToWith, WithSpan};
use std::fmt::Write;

#[derive(Debug, Clone)]
pub enum Statement {
    Return(Option<WithSpan<Expr>>),
    Break,
    Continue,
    If {
        cond: WithSpan<Expr>,
        then: WithSpan<Block>,
        otherwise: Option<WithSpan<Block>>,
    },
    Until {
        cond: WithSpan<Expr>,
        block: WithSpan<Block>,
    },
    For {
        init: WithSpan<VarDefine>,
        cond: WithSpan<Expr>,
        update: Box<WithSpan<Statement>>,
        block: WithSpan<Block>,
    },
    FuncCall(FuncCall),
    VarAssign {
        name: WithSpan<CachedString>,
        value: WithSpan<Expr>,
    },
    VarDefine(VarDefine),
}

impl Visit for Statement {
    fn visit_impl(node: Node) -> Self {
        match node.rule() {
            Rule::ret => Self::Return(node.children().next().map(Node::visit)),
            Rule::brk => Self::Break,
            Rule::cont => Self::Continue,
            Rule::iff => {
                let mut nodes = node.children();

                Self::If {
                    cond: nodes.next().unwrap().visit(),
                    then: nodes.next().unwrap().visit(),
                    otherwise: nodes.next().map(Node::visit),
                }
            }
            Rule::until => {
                let mut nodes = node.children();

                Self::Until {
                    cond: nodes.next().unwrap().visit(),
                    block: nodes.next().unwrap().visit(),
                }
            }
            Rule::forr => {
                let mut nodes = node.children();

                Self::For {
                    init: nodes.next().unwrap().visit(),
                    cond: nodes.next().unwrap().visit(),
                    update: nodes.next().unwrap().visit::<Statement>().into(),
                    block: nodes.next().unwrap().visit(),
                }
            }
            Rule::func_call => Self::FuncCall(node.visit().0),
            Rule::var_assign => {
                let mut nodes = node.children();

                Self::VarAssign {
                    name: nodes.next().unwrap().as_cached_str_with_span(),
                    value: nodes.next().unwrap().visit(),
                }
            }
            Rule::var_define => Self::VarDefine(node.visit().0),

            _ => unexpected_rule(node),
        }
    }
}

impl Gen for WithSpan<Statement> {
    fn span(&self) -> Span {
        self.1
    }

    fn gen_impl(self) -> MyResult<String> {
        Ok(match self.0 {
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
                    .check(Scope::current().func_return_type())?;

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
                cond.ty().check(PrimitiveType::Bool.ty())?;

                s
            }
            Statement::Until { cond, block } => {
                let mut s = format!("while(!({})) ", cond.clone().gen()?);
                let scope = Scope::new(true, None);
                s.write_str(&block.gen()?).unwrap();
                drop(scope);

                // type check
                cond.ty().check(PrimitiveType::Bool.ty())?;

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
                cond.ty().check(PrimitiveType::Bool.ty())?;

                drop(scope);
                s
            }
            Statement::FuncCall(func_call) => format!("{};", func_call.with(self.1).gen()?),
            Statement::VarAssign { name, value } => {
                let s = format!("{} = {};", name.to_string(), value.clone().gen()?);
                // type check
                value.ty().check(Scope::current().get_var(name.0)?.ty())?;
                s
            }
            Statement::VarDefine(var_define) => format!("{};", var_define.with(self.1).gen()?),
        })
    }
}

pub type Block = Vec<WithSpan<Statement>>;

impl Visit for Block {
    fn visit_impl(node: Node) -> Self {
        node.into_inner_checked(Rule::block).visit_rest()
    }
}

impl Gen for WithSpan<Block> {
    fn span(&self) -> Span {
        self.1
    }

    fn gen_impl(self) -> MyResult<String> {
        Ok(format!(
            "{{\n{}\n}}",
            self.0
                .into_iter()
                .map(Gen::gen)
                .collect::<MyResult<Vec<_>>>()?
                .join("\n")
        ))
    }
}

#[derive(Debug, Clone)]
pub struct FuncCall {
    name: WithSpan<CachedString>,
    args: Vec<WithSpan<Expr>>,
}

impl Visit for FuncCall {
    fn visit_impl(node: Node) -> Self {
        let mut nodes = node.into_inner_checked(Rule::func_call);

        Self {
            name: nodes.next().unwrap().as_cached_str_with_span(),
            args: nodes.visit_rest(),
        }
    }
}

impl Gen for WithSpan<FuncCall> {
    fn span(&self) -> Span {
        self.1
    }

    fn gen_impl(self) -> MyResult<String> {
        let s = format!(
            "{}({})",
            self.name.to_string(),
            self.args
                .clone()
                .into_iter()
                .map(Gen::gen)
                .collect::<MyResult<Vec<_>>>()?
                .join(", "),
        );

        // type check
        Scope::current().get_func(
            *self.name,
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
                *self.name,
                self.args.iter().map(|arg| arg.ty()).collect::<Vec<_>>(),
            )
            .expect(GET_FUNC_ERR)
            .ty()
    }
}

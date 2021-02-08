use crate::cached::CachedString;
use crate::define::VarDefine;
use crate::error::{err, unexpected_kind, MyResult};
use crate::expr::Expr;
use crate::parse::{Kind, Node};
use crate::pass::{Gen, Visit, WithSpan};
use crate::scope::Scope;
use crate::span::Span;
use crate::ty::{HasType, PrimitiveType, Type};
use crate::with::ToWith;
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
    VarAssign {
        name: WithSpan<CachedString>,
        value: WithSpan<Expr>,
    },
    VarDefine(VarDefine),
    Expr(Expr),
}

impl Visit for Statement {
    fn visit_impl(node: Node) -> Self {
        match node.kind() {
            Kind::ret => Self::Return(node.children().next().map(Node::visit)),
            Kind::brk => Self::Break,
            Kind::cont => Self::Continue,
            Kind::iff => {
                let mut nodes = node.children();

                Self::If {
                    cond: nodes.next().unwrap().visit(),
                    then: nodes.next().unwrap().visit(),
                    otherwise: nodes.next().map(Node::visit),
                }
            }
            Kind::until => {
                let mut nodes = node.children();

                Self::Until {
                    cond: nodes.next().unwrap().visit(),
                    block: nodes.next().unwrap().visit(),
                }
            }
            Kind::forr => {
                let mut nodes = node.children();

                Self::For {
                    init: nodes.next().unwrap().visit(),
                    cond: nodes.next().unwrap().visit(),
                    update: nodes.next().unwrap().visit::<Statement>().into(),
                    block: nodes.next().unwrap().visit(),
                }
            }
            Kind::var_assign => {
                let mut nodes = node.children();

                Self::VarAssign {
                    name: nodes.next().unwrap().as_cached_str_with_span(),
                    value: nodes.next().unwrap().visit(),
                }
            }
            Kind::var_define => Self::VarDefine(node.visit().0),
            Kind::expr => Self::Expr(node.visit().0),

            _ => unexpected_kind(node),
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
                    return err("break cant be used outside of loops");
                }
                "break;".into()
            }
            Statement::Continue => {
                if !Scope::current().in_loop() {
                    return err("continue cant be used outside of loops");
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
            Statement::VarAssign { name, value } => {
                let s = format!("{} = {};", *name, value.clone().gen()?);
                // type check
                value.ty().check(Scope::current().get_var(name.0)?.ty())?;
                s
            }
            Statement::VarDefine(var_define) => format!("{};", var_define.with(self.1).gen()?),
            Statement::Expr(expr) => format!("{};", expr.with(self.1).gen()?),
        })
    }
}

pub type Block = Vec<WithSpan<Statement>>;

impl Visit for Block {
    fn visit_impl(node: Node) -> Self {
        node.children_checked(Kind::block).visit_rest()
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
    pub name: WithSpan<CachedString>,
    pub args: Vec<WithSpan<Expr>>,
}

impl Visit for FuncCall {
    fn visit_impl(node: Node) -> Self {
        let mut nodes = node.children_checked(Kind::func_call);

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
            *self.name,
            self.args.iter().map(|arg| arg.ty()).collect::<Vec<_>>(),
        )?;

        Ok(s)
    }
}

impl HasType for FuncCall {
    fn ty(&self) -> Type {
        Scope::current()
            .get_func(
                *self.name,
                self.args.iter().map(|arg| arg.ty()).collect::<Vec<_>>(),
            )
            .unwrap()
            .ty()
    }
}

pub type CCode = Vec<CCodePart>;
#[derive(Debug, Clone)]
pub enum CCodePart {
    String(String),
    Expr(WithSpan<Expr>),
}
impl Visit for CCode {
    fn visit_impl(node: Node) -> Self {
        node.children_checked(Kind::c_code)
            .into_iter()
            .map(|node| match node.kind() {
                Kind::c_code_str => CCodePart::String(node.as_str().into()),
                Kind::expr => CCodePart::Expr(node.visit()),

                _ => unexpected_kind(node),
            })
            .collect()
    }
}

impl Gen for WithSpan<CCode> {
    fn span(&self) -> Span {
        self.1
    }

    fn gen_impl(self) -> MyResult<String> {
        self.0
            .into_iter()
            .map(|part| match part {
                CCodePart::String(string) => Ok(string),
                CCodePart::Expr(expr) => expr.gen(),
            })
            .collect()
    }
}

impl HasType for CCode {
    fn ty(&self) -> Type {
        Type::CCode
    }
}

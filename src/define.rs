use crate::cached::CachedString;
use crate::error::{unexpected_rule, MyResult};
use crate::expr::Expr;
use crate::parse::{Node, Rule};
use crate::pass::{Gen, Visit};
use crate::scope::{Scope, Symbol};
use crate::span::Span;
use crate::statement::{Block, CCode};
use crate::ty::{HasType, Type};
use crate::with::{ToWith, WithSpan};
use std::fmt::Write;

pub type Program = Vec<WithSpan<Define>>;

impl Visit for Program {
    fn visit_impl(node: Node) -> Self {
        node.into_inner_checked(Rule::program)
            .filter_map(|node| {
                // last rule is EOI. dont visit it
                if node.rule() == Rule::EOI {
                    return None;
                }
                Some(node.visit())
            })
            .collect()
    }
}

impl Gen for WithSpan<Program> {
    fn span(&self) -> Span {
        self.1
    }

    fn gen_impl(self) -> MyResult<String> {
        let scope = Scope::init();
        let s = self
            .0
            .into_iter()
            .map(Gen::gen)
            .collect::<MyResult<Vec<_>>>()?
            .join("\n");
        drop(scope);
        Ok(s)
    }
}

#[derive(Debug, Clone)]
pub enum Define {
    Struct {
        name: WithSpan<CachedString>,
        body: Vec<WithSpan<Define>>,
    },
    Func {
        ty: WithSpan<Type>,
        name: WithSpan<CachedString>,
        args: Vec<WithSpan<VarDefine>>,
        body: WithSpan<Block>,
    },
    Var(VarDefine),

    CCode(CCode),
}

impl Visit for Define {
    fn visit_impl(node: Node) -> Self {
        match node.rule() {
            Rule::struct_define => {
                let mut nodes = node.children();

                Self::Struct {
                    name: nodes.next().unwrap().as_cached_str_with_span(),
                    body: nodes.visit_rest(),
                }
            }
            Rule::func_define => {
                let mut nodes = node.children().peekable();

                let ty = nodes.next().unwrap().visit();
                let name = nodes.next().unwrap().as_cached_str_with_span();
                let mut args = vec![];
                while nodes.peek().is_some() && nodes.peek().unwrap().rule() == Rule::var_define {
                    args.push(nodes.next().unwrap().visit())
                }
                let body = nodes.next().unwrap().visit();

                Self::Func {
                    ty,
                    name,
                    args,
                    body,
                }
            }
            Rule::var_define => Self::Var(node.visit().0),

            Rule::c_code => Self::CCode(node.visit().0),

            _ => unexpected_rule(node),
        }
    }
}

impl Gen for WithSpan<Define> {
    fn span(&self) -> Span {
        self.1
    }

    fn gen_impl(self) -> MyResult<String> {
        Ok(match self.0 {
            Define::Struct { name, body } => {
                let s = format!(
                    "typedef struct {{\n{}\n}} {};",
                    body.clone()
                        .into_iter()
                        .map(Gen::gen)
                        .collect::<MyResult<Vec<_>>>()?
                        .join("\n"),
                    *name
                );

                Scope::current().add(Symbol::Struct {
                    name: *name,
                    field_types: body
                        .iter()
                        .filter_map(|define| {
                            if let Define::Var(VarDefine { ty, name, .. }) = define.0 {
                                Some((*name, *ty))
                            } else {
                                None
                            }
                        })
                        .collect(),
                })?;

                s
            }
            Define::Func {
                ty,
                name,
                args,
                body,
            } => {
                let scope = Scope::new(false, Some(ty.0));
                let s = format!(
                    "{} {}({}) {}",
                    ty.clone().gen()?,
                    *name,
                    args.clone()
                        .into_iter()
                        .map(Gen::gen)
                        .collect::<MyResult<Vec<_>>>()?
                        .join(", "),
                    body.gen()?
                );
                drop(scope);

                Scope::current().add(Symbol::Func {
                    ty: *ty,
                    name: *name,
                    arg_types: args.into_iter().map(|arg| *arg.ty).collect(),
                })?;

                s
            }
            Define::Var(var_define) => format!("{};", var_define.with(self.1).gen()?),

            Define::CCode(c_code) => c_code.with(self.1).gen()?,
        })
    }
}

#[derive(Debug, Clone)]
pub struct VarDefine {
    ty: WithSpan<Type>,
    name: WithSpan<CachedString>,
    value: Option<WithSpan<Expr>>,
}

impl Visit for VarDefine {
    fn visit_impl(node: Node) -> Self {
        let mut nodes = node.into_inner_checked(Rule::var_define);

        Self {
            ty: nodes.next().unwrap().visit(),
            name: nodes.next().unwrap().as_cached_str_with_span(),
            value: nodes.next().map(Node::visit),
        }
    }
}

impl Gen for WithSpan<VarDefine> {
    fn span(&self) -> Span {
        self.1
    }

    fn gen_impl(self) -> MyResult<String> {
        let mut s = format!("{} {}", self.ty.clone().gen()?, *self.name);
        if let Some(value) = self.value.clone() {
            write!(s, " = {}", value.gen()?).unwrap();
        }

        // type check
        if let Some(value) = &self.value {
            value.ty().check(*self.ty)?;
        }

        Scope::current().add(Symbol::Var {
            ty: self.ty.0,
            name: self.name.0,
        })?;

        Ok(s)
    }
}

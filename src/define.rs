use crate::error::{unexpected_rule, MyResult};
use crate::expr::Expr;
use crate::gen::Gen;
use crate::parse::{Pair, Rule};
use crate::pos::{Pos, WithPos};
use crate::scope::{Scope, Symbol};
use crate::statement::Block;
use crate::ty::{HasType, Type};
use crate::util::{PairExt, PairsExt};
use crate::visit::Visit;
use crate::with::ToWith;
use std::fmt::Write;

pub type Program = Vec<WithPos<Define>>;

impl Visit for Program {
    fn visit_impl(pair: Pair) -> Self {
        pair.into_inner_checked(Rule::program)
            .filter_map(|pair| {
                // last rule is EOI. dont visit it
                if pair.as_rule() == Rule::EOI {
                    return None;
                }
                Some(pair.visit())
            })
            .collect()
    }
}

impl Gen for WithPos<Program> {
    fn pos(&self) -> Pos {
        self.extra
    }

    fn gen_impl(self) -> MyResult<String> {
        let scope = Scope::init();
        let s = self
            .inner
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
        name: WithPos<String>,
        body: Vec<WithPos<Define>>,
    },
    Func {
        ty: WithPos<Type>,
        name: WithPos<String>,
        args: Vec<WithPos<VarDefine>>,
        body: WithPos<Block>,
    },
    Var(VarDefine),
}

impl Visit for Define {
    fn visit_impl(pair: Pair) -> Self {
        match pair.as_rule() {
            Rule::struct_define => {
                let mut pairs = pair.into_inner();

                Self::Struct {
                    name: pairs.next().unwrap().as_str_with_pos(),
                    body: pairs.visit_rest(),
                }
            }
            Rule::func_define => {
                let mut pairs = pair.into_inner();

                let ty = pairs.next().unwrap().visit();
                let name = pairs.next().unwrap().as_str_with_pos();
                let mut args = vec![];
                while pairs.peek().is_some() && pairs.peek().unwrap().as_rule() == Rule::var_define
                {
                    args.push(pairs.next().unwrap().visit())
                }
                let body = pairs.next().unwrap().visit();

                Self::Func {
                    ty,
                    name,
                    args,
                    body,
                }
            }
            Rule::var_define => Self::Var(pair.visit().inner),

            rule => unexpected_rule(rule),
        }
    }
}

impl Gen for WithPos<Define> {
    fn pos(&self) -> Pos {
        self.extra
    }

    fn gen_impl(self) -> MyResult<String> {
        Ok(match self.inner {
            Define::Struct { name, body } => {
                let s = format!(
                    "typedef struct {{\n{}\n}} {};",
                    body.into_iter()
                        .map(Gen::gen)
                        .collect::<MyResult<Vec<_>>>()?
                        .join("\n"),
                    *name
                );

                // fixme this is half-baked?
                Scope::current().add(Symbol::Type(Type::Named(name.inner)))?;

                s
            }
            Define::Func {
                ty,
                name,
                args,
                body,
                ..
            } => {
                let scope = Scope::new(false, Some(ty.clone().inner));
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
                    ty: ty.inner,
                    name: name.inner,
                    arg_types: args.into_iter().map(|arg| arg.inner.ty.inner).collect(),
                })?;

                s
            }
            Define::Var(var_define) => format!("{};", var_define.with(self.extra).gen()?),
        })
    }
}

#[derive(Debug, Clone)]
pub struct VarDefine {
    ty: WithPos<Type>,
    name: WithPos<String>,
    value: Option<WithPos<Expr>>,
}

impl Visit for VarDefine {
    fn visit_impl(pair: Pair) -> Self {
        let mut pairs = pair.into_inner_checked(Rule::var_define);

        Self {
            ty: pairs.next().unwrap().visit(),
            name: pairs.next().unwrap().as_str_with_pos(),
            value: pairs.next().map(Pair::visit),
        }
    }
}

impl Gen for WithPos<VarDefine> {
    fn pos(&self) -> Pos {
        self.extra
    }

    fn gen_impl(self) -> MyResult<String> {
        let mut s = format!("{} {}", self.ty.clone().gen()?, *self.name);
        if let Some(value) = self.value.clone() {
            write!(s, " = {}", value.gen()?).unwrap();
        }

        // type check
        if let Some(value) = &self.value {
            value.ty().check(&self.ty)?;
        }

        Scope::current().add(Symbol::Var {
            ty: self.ty.inner.clone(),
            name: self.inner.name.inner,
        })?;

        Ok(s)
    }
}

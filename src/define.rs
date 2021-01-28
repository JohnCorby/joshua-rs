use crate::error::{unexpected_rule, MyResult};
use crate::expr::Expr;
use crate::gen::Gen;
use crate::parse::{Pair, Rule};
use crate::statement::Block;
use crate::ty::Type;
use crate::util::{PairExt, PairsExt};
use crate::visit::Visit;
use std::fmt::Write;

#[derive(Debug, Clone)]
pub enum Define {
    Struct {
        name: String,
        body: Vec<Define>,
    },
    Func {
        ty: Type,
        name: String,
        args: Vec<VarDefine>,
        body: Block,
    },
    Var(VarDefine),
}

impl Visit for Define {
    fn visit_impl(pair: Pair) -> MyResult<Self> {
        Ok(match pair.as_rule() {
            Rule::struct_define => {
                let mut pairs = pair.into_inner();

                Self::Struct {
                    name: pairs.next()?.as_str().into(),
                    body: pairs.visit_rest()?,
                }
            }
            Rule::func_define => {
                let mut pairs = pair.into_inner();

                let ty = pairs.next()?.visit()?;
                let name = pairs.next()?.as_str().into();
                let mut args = vec![];
                while pairs.peek().is_some() && pairs.peek().unwrap().as_rule() == Rule::var_define
                {
                    args.push(pairs.next().unwrap().visit()?)
                }
                let body = pairs.next()?.visit()?;

                Self::Func {
                    ty,
                    name,
                    args,
                    body,
                }
            }
            Rule::var_define => Self::Var(pair.visit()?),

            rule => unexpected_rule(rule)?,
        })
    }
}

impl Gen for Define {
    fn gen(self) -> MyResult<String> {
        Ok(match self {
            Self::Struct { name, body } => format!(
                "typedef struct {{\n{}\n}} {};",
                body.into_iter()
                    .map(Define::gen)
                    .collect::<MyResult<Vec<_>>>()?
                    .join("\n"),
                name
            ),
            Self::Func {
                ty,
                name,
                args,
                body,
            } => {
                format!(
                    "{} {}({}) {}",
                    ty.gen()?,
                    name,
                    args.into_iter()
                        .map(VarDefine::gen)
                        .collect::<MyResult<Vec<_>>>()?
                        .join(", "),
                    body.gen()?
                )
            }
            Self::Var(var_define) => var_define.gen()?,
        })
    }
}

#[derive(Debug, Clone)]
pub struct VarDefine {
    ty: Type,
    name: String,
    value: Option<Expr>,
}

impl Visit for VarDefine {
    fn visit_impl(pair: Pair) -> MyResult<Self> {
        let mut pairs = pair.into_inner_checked(Rule::var_define)?;

        Ok(Self {
            ty: pairs.next()?.visit()?,
            name: pairs.next()?.as_str().into(),
            value: pairs.next().map(Pair::visit).transpose()?,
        })
    }
}

impl Gen for VarDefine {
    fn gen(self) -> MyResult<String> {
        let mut s = format!("{} {}", self.ty.gen()?, self.name);
        if let Some(value) = self.value {
            write!(s, " = {}", value.gen()?)?;
        }
        Ok(s)
    }
}

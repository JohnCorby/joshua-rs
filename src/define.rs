use crate::error::{unexpected_rule, MyResult};
use crate::expr::Expr;
use crate::gen::Gen;
use crate::parse::{Pair, Rule};
use crate::pos::{AsPos, HasPos, Pos};
use crate::scope::{Scope, Symbol};
use crate::statement::Block;
use crate::ty::{HasType, Type};
use crate::util::{PairExt, PairsExt};
use crate::visit::Visit;
use std::fmt::Write;

#[derive(Debug, Clone)]
pub struct Program {
    pos: Pos,
    defines: Vec<Define>,
}

impl Visit for Program {
    fn visit_impl(pair: Pair) -> Self {
        Self {
            pos: pair.as_pos(),
            defines: pair
                .into_inner_checked(Rule::program)
                .filter_map(|pair| {
                    // last rule is EOI. dont visit it
                    if pair.as_rule() == Rule::EOI {
                        return None;
                    }
                    Some(pair.visit())
                })
                .collect(),
        }
    }
}

impl HasPos for Program {
    fn pos(&self) -> Pos {
        self.pos
    }
}
impl Gen for Program {
    fn gen_impl(self) -> MyResult<String> {
        let scope = Scope::init();
        let s = self
            .defines
            .into_iter()
            .map(Define::gen)
            .collect::<MyResult<Vec<_>>>()?
            .join("\n");
        drop(scope);
        Ok(s)
    }
}

#[derive(Debug, Clone)]
pub enum Define {
    Struct {
        pos: Pos,
        name: String,
        body: Vec<Define>,
    },
    Func {
        pos: Pos,
        ty: Type,
        name: String,
        args: Vec<VarDefine>,
        body: Block,
    },
    Var(VarDefine),
}

impl Visit for Define {
    fn visit_impl(pair: Pair) -> Self {
        match pair.as_rule() {
            Rule::struct_define => {
                let pos = pair.as_pos();
                let mut pairs = pair.into_inner();

                Self::Struct {
                    pos,
                    name: pairs.next().unwrap().as_str().into(),
                    body: pairs.visit_rest(),
                }
            }
            Rule::func_define => {
                let pos = pair.as_pos();
                let mut pairs = pair.into_inner();

                let ty = pairs.next().unwrap().visit();
                let name = pairs.next().unwrap().as_str().into();
                let mut args = vec![];
                while pairs.peek().is_some() && pairs.peek().unwrap().as_rule() == Rule::var_define
                {
                    args.push(pairs.next().unwrap().visit())
                }
                let body = pairs.next().unwrap().visit();

                Self::Func {
                    pos,
                    ty,
                    name,
                    args,
                    body,
                }
            }
            Rule::var_define => Self::Var(pair.visit()),

            rule => unexpected_rule(rule),
        }
    }
}

impl HasPos for Define {
    fn pos(&self) -> Pos {
        match self {
            Define::Struct { pos, .. } => *pos,
            Define::Func { pos, .. } => *pos,
            Define::Var(var_define) => var_define.pos(),
        }
    }
}
impl Gen for Define {
    fn gen_impl(self) -> MyResult<String> {
        Ok(match self {
            Self::Struct { pos, name, body } => {
                let s = format!(
                    "typedef struct {{\n{}\n}} {};",
                    body.into_iter()
                        .map(Define::gen)
                        .collect::<MyResult<Vec<_>>>()?
                        .join("\n"),
                    name
                );

                // fixme this is half-baked?
                Scope::current().add(Symbol::Type(Type::Named { pos, name }))?;

                s
            }
            Self::Func {
                ty,
                name,
                args,
                body,
                ..
            } => {
                let scope = Scope::new(false, Some(ty.clone()));
                let s = format!(
                    "{} {}({}) {}",
                    ty.clone().gen()?,
                    name,
                    args.clone()
                        .into_iter()
                        .map(VarDefine::gen)
                        .collect::<MyResult<Vec<_>>>()?
                        .join(", "),
                    body.gen()?
                );
                drop(scope);

                Scope::current().add(Symbol::Func {
                    ty,
                    name,
                    arg_types: args.into_iter().map(|arg| arg.ty).collect(),
                })?;

                s
            }
            Self::Var(var_define) => var_define.gen()?,
        })
    }
}

#[derive(Debug, Clone)]
pub struct VarDefine {
    pos: Pos,
    ty: Type,
    name: String,
    value: Option<Expr>,
}

impl Visit for VarDefine {
    fn visit_impl(pair: Pair) -> Self {
        let pos = pair.as_pos();
        let mut pairs = pair.into_inner_checked(Rule::var_define);

        Self {
            pos,
            ty: pairs.next().unwrap().visit(),
            name: pairs.next().unwrap().as_str().into(),
            value: pairs.next().map(Pair::visit),
        }
    }
}

impl HasPos for VarDefine {
    fn pos(&self) -> Pos {
        self.pos
    }
}
impl Gen for VarDefine {
    fn gen_impl(self) -> MyResult<String> {
        let value_ty = self.value.as_ref().map(|value| value.ty());
        let ty = self.ty.clone();

        let mut s = format!("{} {}", self.ty.gen()?, self.name);
        if let Some(value) = self.value {
            write!(s, " = {}", value.gen()?).unwrap();
        }

        // type check
        if let Some(value_ty) = value_ty {
            value_ty.check(&ty)?;
        }

        Scope::current().add(Symbol::Var {
            ty,
            name: self.name,
        })?;

        Ok(s)
    }
}

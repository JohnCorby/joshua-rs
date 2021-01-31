use crate::error::{unexpected_rule, MyResult};
use crate::expr::Expr;
use crate::gen::Gen;
use crate::parse::{Pair, Rule};
use crate::pos::{AsPos, HasPos, Pos};
use crate::scope::{Scope, Symbol};
use crate::statement::Block;
use crate::ty::Type;
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
        Scope::push(false);

        Type::init()?;

        let result = Ok(self
            .defines
            .into_iter()
            .map(Define::gen)
            .collect::<MyResult<Vec<_>>>()?
            .join("\n"));

        Scope::pop();
        result
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
            Self::Struct { name, body, .. } => {
                // todo add as type
                // Scope::current().add(Symbol::Type { name: name.clone() })?;

                format!(
                    "typedef struct {{\n{}\n}} {};",
                    body.into_iter()
                        .map(Define::gen)
                        .collect::<MyResult<Vec<_>>>()?
                        .join("\n"),
                    name
                )
            }
            Self::Func {
                ty,
                name,
                args,
                body,
                ..
            } => {
                Scope::current().add(Symbol::Func {
                    ty: ty.clone(),
                    name: name.clone(),
                    arg_types: args.iter().map(|arg| arg.ty.clone()).collect(),
                })?;

                Scope::push(true);
                let s = format!(
                    "{} {}({}) {}",
                    ty.gen()?,
                    name,
                    args.into_iter()
                        .map(VarDefine::gen)
                        .collect::<MyResult<Vec<_>>>()?
                        .join(", "),
                    body.gen()?
                );
                Scope::pop();
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
        Scope::current().add(Symbol::Var {
            ty: self.ty.clone(),
            name: self.name.clone(),
        })?;

        let mut s = format!("{} {}", self.ty.gen()?, self.name);
        if let Some(value) = self.value {
            write!(s, " = {}", value.gen()?).unwrap();
        }
        Ok(s)
    }
}

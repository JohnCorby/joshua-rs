use crate::cached::CachedString;
use crate::error::{unexpected_kind, MyResult};
use crate::expr::Expr;
use crate::parse::{Kind, Node};
use crate::scope::{Scope, Symbol};
use crate::span::Span;
use crate::statement::{Block, CCode};
use crate::ty::{Type, TypeKind};
use crate::util::{Mangle, Visit};
use std::collections::HashMap;
use std::fmt::Write;

#[derive(Debug, Clone)]
pub struct Program(Vec<Define>);

impl Visit for Program {
    fn visit(node: Node) -> Self {
        Self(
            node.children_checked(Kind::program)
                .filter_map(|node| match node.kind() {
                    Kind::EOI => None,
                    _ => Some(node.visit()),
                })
                .collect(),
        )
    }
}

impl Program {
    pub fn gen(self) -> MyResult<String> {
        let scope = Scope::init();
        let s = self
            .0
            .into_iter()
            .map(Define::gen)
            .collect::<MyResult<Vec<_>>>()?
            .join("\n");
        drop(scope);
        Ok(s)
    }
}

#[derive(Debug, Clone)]
pub struct Define {
    span: Span,
    kind: DefineKind,
}
#[derive(Debug, Clone)]
pub enum DefineKind {
    Struct {
        name: CachedString,
        body: Vec<Define>,
    },
    Func {
        ty: Type,
        name: CachedString,
        args: Vec<VarDefine>,
        body: Block,
    },
    Var(VarDefine),

    CCode(CCode),
}

impl Visit for Define {
    fn visit(node: Node) -> Self {
        let span = node.span();
        use DefineKind::*;
        let kind = match node.kind() {
            Kind::struct_define => {
                let mut nodes = node.children();

                Struct {
                    name: nodes.next().unwrap().as_str().into(),
                    body: nodes.visit_rest(),
                }
            }
            Kind::func_define => {
                let mut nodes = node.children().peekable();

                let ty = nodes.next().unwrap().visit();
                let name = nodes.next().unwrap().as_str().into();
                let mut args = vec![];
                while nodes.peek().is_some() && nodes.peek().unwrap().kind() == Kind::var_define {
                    args.push(nodes.next().unwrap().visit())
                }
                let body = nodes.next().unwrap().visit();

                Func {
                    ty,
                    name,
                    args,
                    body,
                }
            }
            Kind::var_define => Var(node.visit()),

            Kind::c_code => CCode(node.visit()),

            _ => unexpected_kind(node),
        };

        Self { span, kind }
    }
}

impl Define {
    pub fn gen(self) -> MyResult<String> {
        use DefineKind::*;
        Ok(match self.kind {
            Struct { name, mut body } => {
                Scope::current().add(
                    Symbol::Struct {
                        name,
                        field_types: {
                            let mut field_types = HashMap::new();
                            for define in &mut body {
                                if let Var(VarDefine {
                                    name, ref mut ty, ..
                                }) = define.kind
                                {
                                    field_types.insert(name, ty.init_ty()?).unwrap_none();
                                }
                            }
                            field_types
                        },
                    },
                    self.span,
                )?;

                format!(
                    "typedef struct {{\n{}\n}} {};",
                    body.into_iter()
                        .map(Define::gen)
                        .collect::<MyResult<Vec<_>>>()?
                        .join("\n"),
                    name
                )
            }
            Func {
                mut ty,
                name,
                mut args,
                body,
            } => {
                let arg_types = args
                    .iter_mut()
                    .map(|arg| arg.ty.init_ty())
                    .collect::<MyResult<Vec<_>>>()?;

                // don't mangle func main (entry point)
                let mut name_gen = name.to_string();
                if name_gen != "main" {
                    name_gen = format!(
                        "{}({})",
                        name_gen,
                        arg_types
                            .iter()
                            .map(TypeKind::to_string)
                            .collect::<Vec<_>>()
                            .join(", ")
                    )
                    .mangle();
                }

                Scope::current().add(
                    Symbol::Func {
                        ty: ty.init_ty()?,
                        name,
                        arg_types,
                    },
                    self.span,
                )?;

                let scope = Scope::new(false, ty.init_ty()?);
                let s = format!(
                    "{} {}({}) {}",
                    ty.gen()?,
                    name_gen,
                    args.into_iter()
                        .map(VarDefine::gen)
                        .collect::<MyResult<Vec<_>>>()?
                        .join(", "),
                    body.gen()?
                );
                scope.check_return_called(self.span)?;
                drop(scope);

                s
            }
            Var(var_define) => format!("{};", var_define.gen()?),

            CCode(c_code) => c_code.gen()?,
        })
    }
}

#[derive(Debug, Clone)]
pub struct VarDefine {
    span: Span,
    ty: Type,
    name: CachedString,
    value: Option<Expr>,
}

impl Visit for VarDefine {
    fn visit(node: Node) -> Self {
        let span = node.span();
        let mut nodes = node.children_checked(Kind::var_define);

        Self {
            span,
            ty: nodes.next().unwrap().visit(),
            name: nodes.next().unwrap().as_str().into(),
            value: nodes.next().map(Node::visit),
        }
    }
}

impl VarDefine {
    pub fn gen(mut self) -> MyResult<String> {
        Scope::current().add(
            Symbol::Var {
                ty: self.ty.init_ty()?,
                name: self.name,
            },
            self.span,
        )?;

        // type check
        if let Some(value) = &mut self.value {
            value.init_ty()?.check(&self.ty.init_ty()?, self.span)?;
        }

        let mut s = format!("{} {}", self.ty.gen()?, self.name.to_string().mangle());
        if let Some(value) = self.value {
            write!(s, " = {}", value.gen()?).unwrap();
        }

        Ok(s)
    }
}

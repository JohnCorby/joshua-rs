use crate::cached::CachedString;
use crate::error::{unexpected_kind, Context, MyResult};
use crate::expr::Expr;
use crate::parse::{Kind, Node};
use crate::scope::{Scope, Symbol};
use crate::span::Span;
use crate::statement::{Block, CCode};
use crate::ty::Type;
use crate::util::{Mangle, Visit};
use std::collections::HashMap;
use std::fmt::Write;

#[derive(Debug, Clone)]
pub struct Program {
    defines: Vec<Define>,
}

impl Visit for Program {
    fn visit(node: Node) -> Self {
        Self {
            defines: node
                .children_checked(Kind::program)
                .filter_map(|node| match node.kind() {
                    Kind::EOI => None,
                    _ => Some(node.visit()),
                })
                .collect(),
        }
    }
}

impl Program {
    pub fn gen(self) -> MyResult<String> {
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
            Struct { name, body } => {
                Scope::current()
                    .add(Symbol::Struct {
                        name,
                        field_types: {
                            let mut map = HashMap::new();
                            for define in &body {
                                if let Var(VarDefine { name, mut ty, .. }) = define.kind {
                                    map.insert(name, ty.ty().ctx(self.span)?).unwrap_none();
                                }
                            }
                            map
                        },
                    })
                    .ctx(self.span)?;

                format!(
                    "typedef struct {{\n{}\n}} {};",
                    body.into_iter()
                        .map(Define::gen)
                        .collect::<MyResult<Vec<_>>>()
                        .ctx(self.span)?
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
                Scope::current()
                    .add(Symbol::Func {
                        ty: ty.ty().ctx(self.span)?,
                        name,
                        arg_types: args
                            .iter_mut()
                            .map(|arg| arg.ty.ty())
                            .collect::<MyResult<Vec<_>>>()
                            .ctx(self.span)?,
                    })
                    .ctx(self.span)?;

                let scope = Scope::new(false, Some(ty.ty().ctx(self.span)?));
                let s = format!(
                    "{} {}({}) {}",
                    ty.gen().ctx(self.span)?,
                    name.to_string().mangle(),
                    args.into_iter()
                        .map(VarDefine::gen)
                        .collect::<MyResult<Vec<_>>>()
                        .ctx(self.span)?
                        .join(", "),
                    body.gen().ctx(self.span)?
                );
                drop(scope);

                s
            }
            Var(var_define) => format!("{};", var_define.gen().ctx(self.span)?),

            CCode(c_code) => c_code.gen().ctx(self.span)?,
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
        Scope::current()
            .add(Symbol::Var {
                ty: self.ty.ty().ctx(self.span)?,
                name: self.name,
            })
            .ctx(self.span)?;

        // type check
        if let Some(value) = &mut self.value {
            value
                .ty()
                .ctx(self.span)?
                .check(&self.ty.ty().ctx(self.span)?)
                .ctx(self.span)?;
        }

        let mut s = format!(
            "{} {}",
            self.ty.gen().ctx(self.span)?,
            self.name.to_string().mangle()
        );
        if let Some(value) = self.value {
            write!(s, " = {}", value.gen().ctx(self.span)?).unwrap();
        }

        Ok(s)
    }
}

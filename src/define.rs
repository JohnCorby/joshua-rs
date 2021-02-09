use crate::cached::CachedString;
use crate::error::{unexpected_kind, MyResult};
use crate::expr::Expr;
use crate::parse::{Kind, Node};
use crate::pass::{Gen, InitType, Visit};
use crate::scope::{Scope, Symbol};
use crate::span::Span;
use crate::statement::{Block, CCode};
use crate::ty::Type;
use std::fmt::Write;

#[derive(Debug, Clone)]
pub struct Program {
    span: Span,
    defines: Vec<Define>,
}

impl Visit for Program {
    fn visit_impl(node: Node) -> Self {
        let span = node.span();
        let defines = node
            .children_checked(Kind::program)
            .filter_map(|node| {
                // last kind is EOI. dont visit it
                if node.kind() == Kind::EOI {
                    return None;
                }
                Some(node.visit())
            })
            .collect();

        Self { span, defines }
    }
}

impl Gen for Program {
    fn span(&self) -> Span {
        self.span
    }

    fn gen_impl(self) -> MyResult<String> {
        let scope = Scope::init();
        let s = self
            .defines
            .into_iter()
            .map(Gen::gen)
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
    fn visit_impl(node: Node) -> Self {
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

impl Gen for Define {
    fn span(&self) -> Span {
        self.span
    }

    fn gen_impl(self) -> MyResult<String> {
        use DefineKind::*;
        Ok(match self.kind {
            Struct { name, body } => {
                Scope::current().add(Symbol::Struct {
                    name,
                    field_types: body
                        .iter()
                        .filter_map(|define| match &define.kind {
                            Var(var_define) => Some((var_define.name, var_define.ty.kind)),
                            _ => None,
                        })
                        .collect(),
                })?;

                format!(
                    "typedef struct {{\n{}\n}} {};",
                    body.into_iter()
                        .map(Gen::gen)
                        .collect::<MyResult<Vec<_>>>()?
                        .join("\n"),
                    name
                )
            }
            Func {
                ty,
                name,
                args,
                body,
            } => {
                Scope::current().add(Symbol::Func {
                    ty: ty.kind,
                    name,
                    arg_types: args.iter().map(|arg| arg.ty.kind).collect(),
                })?;

                let scope = Scope::new(false, Some(ty.kind));
                let s = format!(
                    "{} {}({}) {}",
                    ty.gen()?,
                    name,
                    args.into_iter()
                        .map(Gen::gen)
                        .collect::<MyResult<Vec<_>>>()?
                        .join(", "),
                    body.gen()?
                );
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
    fn visit_impl(node: Node) -> Self {
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

impl Gen for VarDefine {
    fn span(&self) -> Span {
        self.span
    }

    fn gen_impl(mut self) -> MyResult<String> {
        Scope::current().add(Symbol::Var {
            ty: self.ty.kind,
            name: self.name,
        })?;

        // type check
        if let Some(value) = &mut self.value {
            value.init_type()?.check(&self.ty.kind)?;
        }

        let mut s = format!("{} {}", self.ty.gen()?, self.name);
        if let Some(value) = self.value {
            write!(s, " = {}", value.gen()?).unwrap();
        }

        Ok(s)
    }
}

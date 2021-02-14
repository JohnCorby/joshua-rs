use crate::cached::CachedString;
use crate::error::{unexpected_kind, Res};
use crate::expr::{Expr, VisitIdent};
use crate::parse::{Kind, Node};
use crate::scope::{Scope, Symbol};
use crate::span::Span;
use crate::statement::{Block, CCode};
use crate::ty::{Type, TypeKind};
use crate::util::{Mangle, Visit};
use std::collections::HashMap;

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
    pub fn gen(self, c_code: &mut String) -> Res<()> {
        let scope = Scope::init(c_code);
        for define in self.0 {
            define.gen(c_code)?;
            c_code.push('\n')
        }
        if c_code.ends_with('\n') {
            c_code.pop();
        }
        drop(scope);
        Ok(())
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
                    name: nodes.next().unwrap().visit_ident(),
                    body: nodes.visit_rest(),
                }
            }
            Kind::func_define => {
                let mut nodes = node.children().peekable();

                let ty = nodes.next().unwrap().visit();
                let name = nodes.next().unwrap().visit_ident();
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
    pub fn gen(self, c_code: &mut String) -> Res<()> {
        use DefineKind::*;
        match self.kind {
            Struct { name, body } => {
                Scope::current().add(
                    Symbol::Struct {
                        name,
                        field_types: {
                            let mut field_types = HashMap::new();
                            for define in &body {
                                if let Var(VarDefine { name, ty, .. }) = &define.kind {
                                    field_types.insert(*name, ty.init_ty()?).unwrap_none();
                                }
                            }
                            field_types
                        },
                    },
                    self.span,
                )?;

                c_code.push_str("typedef struct {\n");
                for define in body {
                    define.gen(c_code)?;
                    c_code.push('\n')
                }
                c_code.push_str("} ");
                c_code.push_str(&name.to_string());
                c_code.push(';')
            }
            Func {
                ty,
                name,
                args,
                body,
            } => {
                let arg_types = args
                    .iter()
                    .map(|arg| arg.ty.init_ty())
                    .collect::<Res<Vec<_>>>()?;

                // don't mangle func main (entry point)
                let mut name_mangled = name.to_string();
                if name_mangled != "main" {
                    name_mangled = format!(
                        "{}({})",
                        name_mangled,
                        arg_types
                            .iter()
                            .map(TypeKind::name)
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
                ty.gen(c_code)?;
                c_code.push(' ');
                c_code.push_str(&name_mangled);
                c_code.push('(');
                for arg in args {
                    arg.gen(c_code)?;
                    c_code.push_str(", ")
                }
                if c_code.ends_with(", ") {
                    c_code.pop();
                    c_code.pop();
                }
                c_code.push_str(") ");
                body.gen(c_code)?;
                scope.check_return_called(self.span)?;
                drop(scope);
            }
            Var(var_define) => {
                var_define.gen(c_code)?;
                c_code.push(';')
            }

            CCode(_c_code) => _c_code.gen(c_code)?,
        }
        Ok(())
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
            name: nodes.next().unwrap().visit_ident(),
            value: nodes.next().map(Node::visit),
        }
    }
}

impl VarDefine {
    pub fn gen(self, c_code: &mut String) -> Res<()> {
        Scope::current().add(
            Symbol::Var {
                ty: self.ty.init_ty()?,
                name: self.name,
            },
            self.span,
        )?;

        // type check
        if let Some(value) = &self.value {
            value.init_ty()?.check(&self.ty.init_ty()?, self.span)?;
        }

        self.ty.gen(c_code)?;
        c_code.push(' ');
        c_code.push_str(&self.name.to_string().mangle());
        if let Some(value) = self.value {
            c_code.push_str(" = ");
            value.gen(c_code)?
        }

        Ok(())
    }
}

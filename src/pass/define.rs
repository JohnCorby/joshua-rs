use crate::context::Ctx;
use crate::error::unexpected_kind;
use crate::parse::{Kind, Node};
use crate::pass::expr::Expr;
use crate::pass::statement::{Block, CCode};
use crate::pass::ty::TypeNode;
use crate::span::Span;
use crate::util::interned_str::InternedStr;
use crate::util::{Mangle, Visit};

#[derive(Debug, Clone)]
pub struct Program<'i>(pub Vec<Define<'i>>);

impl<'i> Visit<'i> for Program<'i> {
    fn visit(node: Node<'i>, ctx: &mut Ctx<'i>) -> Self {
        Self(
            node.children_checked(Kind::program)
                .filter_map(|node| match node.kind() {
                    Kind::EOI => None,
                    _ => Some(node.visit(ctx)),
                })
                .collect(),
        )
    }
}

impl<'i> Program<'i> {
    pub fn gen(self, ctx: &mut Ctx<'i>) {
        for define in self.0 {
            define.gen(ctx);
            ctx.o.push('\n')
        }
        if ctx.o.ends_with('\n') {
            ctx.o.pop();
        }
    }
}

#[derive(Debug, Clone)]
pub struct Define<'i> {
    pub span: Span<'i>,
    pub kind: DefineKind<'i>,
}
#[derive(Debug, Clone)]
pub enum DefineKind<'i> {
    Struct {
        name: InternedStr<'i>,
        body: Vec<Define<'i>>,
    },
    Func {
        ty_node: TypeNode<'i>,
        name: InternedStr<'i>,
        generic_placeholders: Vec<InternedStr<'i>>,
        args: Vec<VarDefine<'i>>,
        body: Block<'i>,
    },
    Var(VarDefine<'i>),

    CCode(CCode<'i>),
}

impl<'i> Visit<'i> for Define<'i> {
    fn visit(node: Node<'i>, ctx: &mut Ctx<'i>) -> Self {
        let span = node.span();
        use DefineKind::*;
        let kind = match node.kind() {
            Kind::struct_define => {
                let mut nodes = node.children();

                Struct {
                    name: nodes.next().unwrap().visit_ident(ctx),
                    body: nodes.visit_rest(ctx),
                }
            }
            Kind::func_define => {
                let mut nodes = node.children().peekable();

                let ty_node = nodes.next().unwrap().visit(ctx);
                let name = nodes.next().unwrap().visit_ident(ctx);
                let generic_placeholders = nodes
                    .next()
                    .unwrap()
                    .children_checked(Kind::func_define_generics)
                    .map(|node| node.visit_ident(ctx))
                    .collect();
                let mut args = vec![];
                while nodes.peek().is_some() && nodes.peek().unwrap().kind() == Kind::var_define {
                    args.push(nodes.next().unwrap().visit(ctx))
                }
                let body = nodes.next().unwrap().visit(ctx);

                Func {
                    ty_node,
                    name,
                    generic_placeholders,
                    args,
                    body,
                }
            }
            Kind::var_define => Var(node.visit(ctx)),

            Kind::c_code => CCode(node.visit(ctx)),

            _ => unexpected_kind(node),
        };

        Self { span, kind }
    }
}

impl<'i> Define<'i> {
    pub fn gen(self, ctx: &mut Ctx<'i>) {
        use DefineKind::*;
        match self.kind {
            Struct { name, body } => {
                ctx.o.push_str("struct ");
                ctx.o.push_str(&name.mangle());
                ctx.o.push_str(" {\n");
                for define in body {
                    define.gen(ctx);
                    ctx.o.push('\n')
                }
                ctx.o.push_str("};");
            }
            Func {
                ty_node,
                name,
                generic_placeholders,
                args,
                body,
            } => {
                // if !generic_placeholders.is_empty() {
                //     Self {
                //         span: self.span,
                //         kind: Func {
                //             ty_node,
                //             name,
                //             generic_placeholders,
                //             args,
                //             body,
                //         },
                //     }
                //     .gen_generic(ctx)?
                // } else {
                // todo generics
                ty_node.gen(ctx);
                ctx.o.push(' ');
                ctx.o.push_str(
                    &name.mangle_func(&args.iter().map(|it| *it.ty_node.ty).collect::<Vec<_>>()),
                );
                ctx.o.push('(');
                for arg in args {
                    arg.gen(ctx);
                    ctx.o.push_str(", ")
                }
                if ctx.o.ends_with(", ") {
                    ctx.o.pop();
                    ctx.o.pop();
                }
                ctx.o.push_str(") ");
                body.gen(ctx);
                // }
            }
            Var(var_define) => {
                var_define.gen(ctx);
                ctx.o.push(';')
            }

            CCode(c_code) => c_code.gen(ctx),
        }
    }
}

#[derive(Debug, Clone)]
pub struct VarDefine<'i> {
    pub span: Span<'i>,
    pub ty_node: TypeNode<'i>,
    pub name: InternedStr<'i>,
    pub value: Option<Expr<'i>>,
}

impl<'i> Visit<'i> for VarDefine<'i> {
    fn visit(node: Node<'i>, ctx: &mut Ctx<'i>) -> Self {
        let span = node.span();
        let mut nodes = node.children_checked(Kind::var_define);

        Self {
            span,
            ty_node: nodes.next().unwrap().visit(ctx),
            name: nodes.next().unwrap().visit_ident(ctx),
            value: nodes.next().map(|node| node.visit(ctx)),
        }
    }
}

impl<'i> VarDefine<'i> {
    pub fn gen(self, ctx: &mut Ctx<'i>) {
        self.ty_node.gen(ctx);
        ctx.o.push(' ');
        ctx.o.push_str(&self.name.mangle());
        if let Some(value) = self.value {
            ctx.o.push_str(" = ");
            value.gen(ctx)
        }
    }
}

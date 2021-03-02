use crate::context::Ctx;
use crate::error::unexpected_kind;
use crate::parse::{Kind, Node};
use crate::pass::define::VarDefine;
use crate::pass::expr::Expr;
use crate::pass::ty::{LiteralType, Type};
use crate::span::Span;
use crate::util::Visit;

#[derive(Debug, Clone)]
pub struct Statement<'i> {
    pub span: Span<'i>,
    pub kind: StatementKind<'i>,
}
#[derive(Debug, Clone)]
#[allow(clippy::large_enum_variant)]
pub enum StatementKind<'i> {
    Return(Option<Expr<'i>>),
    Break,
    Continue,
    If {
        cond: Expr<'i>,
        then: Block<'i>,
        otherwise: Option<Block<'i>>,
    },
    Until {
        cond: Expr<'i>,
        block: Block<'i>,
    },
    For {
        init: VarDefine<'i>,
        cond: Expr<'i>,
        update: Box<Statement<'i>>,
        block: Block<'i>,
    },
    ExprAssign {
        lvalue: Expr<'i>,
        rvalue: Expr<'i>,
    },
    VarDefine(VarDefine<'i>),
    Expr(Expr<'i>),
}

impl Visit<'i> for Statement<'i> {
    fn visit(node: Node<'i>, ctx: &mut Ctx<'i>) -> Self {
        let span = node.span();
        use StatementKind::*;
        let kind = match node.kind() {
            Kind::ret => Return(node.children().next().map(|node| node.visit(ctx))),
            Kind::brk => Break,
            Kind::cont => Continue,
            Kind::iff => {
                let mut nodes = node.children();

                If {
                    cond: nodes.next().unwrap().visit(ctx),
                    then: nodes.next().unwrap().visit(ctx),
                    otherwise: nodes.next().map(|node| node.visit(ctx)),
                }
            }
            Kind::until => {
                let mut nodes = node.children();

                Until {
                    cond: nodes.next().unwrap().visit(ctx),
                    block: nodes.next().unwrap().visit(ctx),
                }
            }
            Kind::forr => {
                let mut nodes = node.children();

                For {
                    init: nodes.next().unwrap().visit(ctx),
                    cond: nodes.next().unwrap().visit(ctx),
                    update: nodes.next().unwrap().visit::<Statement<'i>>(ctx).into(),
                    block: nodes.next().unwrap().visit(ctx),
                }
            }
            Kind::expr_assign => {
                let mut nodes = node.children();

                ExprAssign {
                    lvalue: nodes.next().unwrap().visit(ctx),
                    rvalue: nodes.next().unwrap().visit(ctx),
                }
            }
            Kind::var_define => VarDefine(node.visit(ctx)),
            Kind::expr => Expr(node.visit(ctx)),

            _ => unexpected_kind(node),
        };

        Self { span, kind }
    }
}

impl Statement<'i> {
    pub fn gen(self, ctx: &mut Ctx<'i>) {
        use StatementKind::*;
        match self.kind {
            Return(value) => {
                ctx.o.push_str("return");
                if let Some(value) = value {
                    ctx.o.push(' ');
                    value.gen(ctx)
                }
                ctx.o.push(';');
            }
            Break => ctx.o.push_str("break;"),
            Continue => ctx.o.push_str("continue;"),
            If {
                cond,
                then,
                otherwise,
            } => {
                ctx.o.push_str("if (");
                cond.gen(ctx);
                ctx.o.push_str(") ");
                then.gen(ctx);
                if let Some(otherwise) = otherwise {
                    otherwise.gen(ctx);
                }
            }
            Until { cond, block } => {
                ctx.o.push_str("while (!(");
                cond.gen(ctx);
                ctx.o.push_str(")) ");
                block.gen(ctx);
            }
            For {
                init,
                cond,
                update,
                block,
            } => {
                ctx.o.push_str("for (");
                init.gen(ctx);
                ctx.o.push_str("; ");

                cond.gen(ctx);
                ctx.o.push_str("; ");
                update.gen(ctx);
                ctx.o.pop(); // for update statement's semicolon
                ctx.o.push_str(") ");
                block.gen(ctx);
            }
            ExprAssign { lvalue, rvalue } => {
                lvalue.gen(ctx);
                ctx.o.push_str(" = ");
                rvalue.gen(ctx);
                ctx.o.push(';');
            }
            VarDefine(var_define) => {
                var_define.gen(ctx);
                ctx.o.push(';');
            }
            Expr(expr) => {
                expr.gen(ctx);
                ctx.o.push(';');
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct Block<'i>(pub Vec<Statement<'i>>);

impl Visit<'i> for Block<'i> {
    fn visit(node: Node<'i>, ctx: &mut Ctx<'i>) -> Self {
        Self(node.children_checked(Kind::block).visit_rest(ctx))
    }
}

impl Block<'i> {
    pub fn gen(self, ctx: &mut Ctx<'i>) {
        ctx.o.push_str("{\n");
        for statement in self.0 {
            statement.gen(ctx);
            ctx.o.push('\n')
        }
        ctx.o.push('}');
    }
}

#[derive(Debug, Clone)]
pub struct CCode<'i>(pub Vec<CCodePart<'i>>);

#[derive(Debug, Clone)]
pub enum CCodePart<'i> {
    String(&'i str),
    Expr(Expr<'i>),
}

impl Visit<'i> for CCode<'i> {
    fn visit(node: Node<'i>, ctx: &mut Ctx<'i>) -> Self {
        Self(
            node.children_checked(Kind::c_code)
                .into_iter()
                .map(|node| match node.kind() {
                    Kind::c_code_str => CCodePart::String(node.str()),
                    Kind::expr => CCodePart::Expr(node.visit(ctx)),

                    _ => unexpected_kind(node),
                })
                .collect(),
        )
    }
}

impl CCode<'i> {
    pub fn ty(&self) -> Type<'i> {
        LiteralType::CCode.ty()
    }

    pub fn gen(self, ctx: &mut Ctx<'i>) {
        ctx.o.push_str("/*<{*/");
        for part in self.0 {
            match part {
                CCodePart::String(str) => ctx.o.push_str(str),
                CCodePart::Expr(expr) => expr.gen(ctx),
            }
        }
        ctx.o.push_str("/*}>*/");
    }
}

use crate::context::Ctx;
use crate::error::{err, unexpected_kind, Res};
use crate::parse::{Kind, Node};
use crate::pass::define::VarDefine;
use crate::pass::expr::Expr;
use crate::pass::ty::{LiteralType, PrimitiveType, Type};
use crate::span::Span;
use crate::util::Visit;
use std::rc::Rc;

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
        update: Rc<Statement<'i>>,
        block: Block<'i>,
    },
    ExprAssign {
        lvalue: Expr<'i>,
        rvalue: Expr<'i>,
    },
    VarDefine(VarDefine<'i>),
    Expr(Expr<'i>),
}

impl<'i> Visit<'i> for Statement<'i> {
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

impl<'i> Statement<'i> {
    pub fn gen(self, ctx: &mut Ctx<'i>) -> Res<'i, ()> {
        use StatementKind::*;
        match self.kind {
            Return(mut value) => {
                ctx.scopes.return_called();

                // type check
                value
                    .as_mut()
                    .map(|value| value.init_ty(ctx))
                    .transpose()?
                    .unwrap_or_else(|| PrimitiveType::Void.ty())
                    .check(ctx.scopes.func_return_type(), Some(self.span))?;

                ctx.o.push_str("return");
                if let Some(value) = value {
                    ctx.o.push(' ');
                    value.gen(ctx)?
                }
                ctx.o.push(';');
            }
            Break => {
                if !ctx.scopes.in_loop() {
                    return err("break can't be used outside of loops", Some(self.span));
                }
                ctx.o.push_str("break;")
            }
            Continue => {
                if !ctx.scopes.in_loop() {
                    return err("continue can't be used outside of loops", Some(self.span));
                }
                ctx.o.push_str("continue;")
            }
            If {
                cond,
                then,
                otherwise,
            } => {
                // type check
                cond.init_ty(ctx)?
                    .check(PrimitiveType::Bool.ty(), Some(self.span))?;

                ctx.o.push_str("if (");
                cond.gen(ctx)?;
                ctx.o.push_str(") ");
                ctx.scopes.push(false, None);
                then.gen(ctx)?;
                ctx.scopes.pop();
                if let Some(otherwise) = otherwise {
                    ctx.scopes.push(false, None);
                    otherwise.gen(ctx)?;
                    ctx.scopes.pop();
                }
            }
            Until { cond, block } => {
                // type check
                cond.init_ty(ctx)?
                    .check(PrimitiveType::Bool.ty(), Some(self.span))?;

                ctx.o.push_str("while (!(");
                cond.gen(ctx)?;
                ctx.o.push_str(")) ");
                ctx.scopes.push(true, None);
                block.gen(ctx)?;
                ctx.scopes.pop();
            }
            For {
                init,
                cond,
                update,
                block,
            } => {
                ctx.scopes.push(true, None);
                ctx.o.push_str("for (");
                init.gen(ctx)?;
                ctx.o.push_str("; ");

                // type check
                cond.init_ty(ctx)?
                    .check(PrimitiveType::Bool.ty(), Some(self.span))?;

                cond.gen(ctx)?;
                ctx.o.push_str("; ");
                update.gen(ctx)?;
                ctx.o.pop(); // for update statement's semicolon
                ctx.o.push_str(") ");
                block.gen(ctx)?;
                ctx.scopes.pop();
            }
            ExprAssign { lvalue, rvalue } => {
                // type check
                lvalue.check_assignable(Some(self.span))?;
                rvalue
                    .init_ty(ctx)?
                    .check(lvalue.init_ty(ctx)?, Some(self.span))?;

                lvalue.gen(ctx)?;
                ctx.o.push_str(" = ");
                rvalue.gen(ctx)?;
                ctx.o.push(';');
            }
            VarDefine(var_define) => {
                var_define.gen(ctx)?;
                ctx.o.push(';');
            }
            Expr(expr) => {
                // type check
                expr.init_ty(ctx)?;
                expr.gen(ctx)?;
                ctx.o.push(';');
            }
        }
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct Block<'i>(pub Vec<Statement<'i>>);

impl<'i> Visit<'i> for Block<'i> {
    fn visit(node: Node<'i>, ctx: &mut Ctx<'i>) -> Self {
        Self(node.children_checked(Kind::block).visit_rest(ctx))
    }
}

impl<'i> Block<'i> {
    pub fn gen(self, ctx: &mut Ctx<'i>) -> Res<'i, ()> {
        ctx.o.push_str("{\n");
        for statement in self.0 {
            statement.gen(ctx)?;
            ctx.o.push('\n')
        }
        ctx.o.push('}');
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct CCode<'i> {
    parts: Vec<CCodePart<'i>>,
}
#[derive(Debug, Clone)]
pub enum CCodePart<'i> {
    String(&'i str),
    Expr(Expr<'i>),
}

impl<'i> Visit<'i> for CCode<'i> {
    fn visit(node: Node<'i>, ctx: &mut Ctx<'i>) -> Self {
        Self {
            parts: node
                .children_checked(Kind::c_code)
                .into_iter()
                .map(|node| match node.kind() {
                    Kind::c_code_str => CCodePart::String(node.str()),
                    Kind::expr => CCodePart::Expr(node.visit(ctx)),

                    _ => unexpected_kind(node),
                })
                .collect(),
        }
    }
}

impl<'i> CCode<'i> {
    pub fn ty(&self) -> Type<'i> {
        LiteralType::CCode.ty()
    }

    pub fn gen(self, ctx: &mut Ctx<'i>) -> Res<'i, ()> {
        ctx.o.push_str("/*<{*/");
        for part in self.parts {
            match part {
                CCodePart::String(str) => ctx.o.push_str(str),
                CCodePart::Expr(expr) => {
                    // type check
                    expr.init_ty(ctx)?;
                    expr.gen(ctx)?
                }
            }
        }
        ctx.o.push_str("/*}>*/");
        Ok(())
    }
}

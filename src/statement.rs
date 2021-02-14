use crate::define::VarDefine;
use crate::error::{err, unexpected_kind, Res};
use crate::expr::Expr;
use crate::parse::{Kind, Node};
use crate::scope::Scope;
use crate::span::Span;
use crate::ty::{PrimitiveType, TypeKind};
use crate::util::Visit;

#[derive(Debug, Clone)]
pub struct Statement {
    span: Span,
    kind: StatementKind,
}
#[derive(Debug, Clone)]
#[allow(clippy::large_enum_variant)]
pub enum StatementKind {
    Return(Option<Expr>),
    Break,
    Continue,
    If {
        cond: Expr,
        then: Block,
        otherwise: Option<Block>,
    },
    Until {
        cond: Expr,
        block: Block,
    },
    For {
        init: VarDefine,
        cond: Expr,
        update: Box<Statement>,
        block: Block,
    },
    ExprAssign {
        lvalue: Expr,
        rvalue: Expr,
    },
    VarDefine(VarDefine),
    Expr(Expr),
}

impl Visit for Statement {
    fn visit(node: Node) -> Self {
        let span = node.span();
        use StatementKind::*;
        let kind = match node.kind() {
            Kind::ret => Return(node.children().next().map(Node::visit)),
            Kind::brk => Break,
            Kind::cont => Continue,
            Kind::iff => {
                let mut nodes = node.children();

                If {
                    cond: nodes.next().unwrap().visit(),
                    then: nodes.next().unwrap().visit(),
                    otherwise: nodes.next().map(Node::visit),
                }
            }
            Kind::until => {
                let mut nodes = node.children();

                Until {
                    cond: nodes.next().unwrap().visit(),
                    block: nodes.next().unwrap().visit(),
                }
            }
            Kind::forr => {
                let mut nodes = node.children();

                For {
                    init: nodes.next().unwrap().visit(),
                    cond: nodes.next().unwrap().visit(),
                    update: nodes.next().unwrap().visit::<Statement>().into(),
                    block: nodes.next().unwrap().visit(),
                }
            }
            Kind::expr_assign => {
                let mut nodes = node.children();

                ExprAssign {
                    lvalue: nodes.next().unwrap().visit(),
                    rvalue: nodes.next().unwrap().visit(),
                }
            }
            Kind::var_define => VarDefine(node.visit()),
            Kind::expr => Expr(node.visit()),

            _ => unexpected_kind(node),
        };

        Self { span, kind }
    }
}

impl Statement {
    pub fn gen(self, c_code: &mut String) -> Res<()> {
        use StatementKind::*;
        match self.kind {
            Return(mut value) => {
                Scope::current().return_called();

                // type check
                value
                    .as_mut()
                    .map(|value| value.init_ty())
                    .transpose()?
                    .unwrap_or_else(|| PrimitiveType::Void.ty())
                    .check(&Scope::current().func_return_type(), self.span)?;

                c_code.push_str("return");
                if let Some(value) = value {
                    c_code.push(' ');
                    value.gen(c_code)?
                }
                c_code.push(';');
            }
            Break => {
                if !Scope::current().in_loop() {
                    return err("break cant be used outside of loops", self.span);
                }
                c_code.push_str("break;")
            }
            Continue => {
                if !Scope::current().in_loop() {
                    return err("continue cant be used outside of loops", self.span);
                }
                c_code.push_str("continue;")
            }
            If {
                cond,
                then,
                otherwise,
            } => {
                // type check
                cond.init_ty()?
                    .check(&PrimitiveType::Bool.ty(), self.span)?;

                c_code.push_str("if (");
                cond.gen(c_code)?;
                c_code.push_str(") ");
                let scope = Scope::new(false, None);
                then.gen(c_code)?;
                drop(scope);
                if let Some(otherwise) = otherwise {
                    let scope = Scope::new(false, None);
                    otherwise.gen(c_code)?;
                    drop(scope);
                }
            }
            Until { cond, block } => {
                // type check
                cond.init_ty()?
                    .check(&PrimitiveType::Bool.ty(), self.span)?;

                c_code.push_str("while (!(");
                cond.gen(c_code)?;
                c_code.push_str(")) ");
                let scope = Scope::new(true, None);
                block.gen(c_code)?;
                drop(scope);
            }
            For {
                init,
                cond,
                update,
                block,
            } => {
                let scope = Scope::new(true, None);
                c_code.push_str("for (");
                init.gen(c_code)?;
                c_code.push_str("; ");

                // type check
                cond.init_ty()?
                    .check(&PrimitiveType::Bool.ty(), self.span)?;

                cond.gen(c_code)?;
                c_code.push_str("; ");
                update.gen(c_code)?;
                c_code.pop(); // for update statement's semicolon
                c_code.push_str(") ");
                block.gen(c_code)?;
                drop(scope);
            }
            ExprAssign { lvalue, rvalue } => {
                // type check
                lvalue.check_assignable(self.span)?;
                rvalue.init_ty()?.check(&lvalue.init_ty()?, self.span)?;

                lvalue.gen(c_code)?;
                c_code.push_str(" = ");
                rvalue.gen(c_code)?;
                c_code.push(';');
            }
            VarDefine(var_define) => {
                var_define.gen(c_code)?;
                c_code.push(';');
            }
            Expr(expr) => {
                // type check
                expr.init_ty()?;
                expr.gen(c_code)?;
                c_code.push(';');
            }
        }
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct Block(Vec<Statement>);

impl Visit for Block {
    fn visit(node: Node) -> Self {
        Self(node.children_checked(Kind::block).visit_rest())
    }
}

impl Block {
    pub fn gen(self, c_code: &mut String) -> Res<()> {
        c_code.push_str("{\n");
        for statement in self.0 {
            statement.gen(c_code)?;
            c_code.push('\n')
        }
        c_code.push('}');
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct CCode {
    parts: Vec<CCodePart>,
}
#[derive(Debug, Clone)]
pub enum CCodePart {
    String(String),
    Expr(Expr),
}

impl Visit for CCode {
    fn visit(node: Node) -> Self {
        Self {
            parts: node
                .children_checked(Kind::c_code)
                .into_iter()
                .map(|node| match node.kind() {
                    Kind::c_code_str => CCodePart::String(node.as_str().into()),
                    Kind::expr => CCodePart::Expr(node.visit()),

                    _ => unexpected_kind(node),
                })
                .collect(),
        }
    }
}

impl CCode {
    pub fn ty(&self) -> TypeKind {
        TypeKind::CCode
    }

    pub fn gen(self, c_code: &mut String) -> Res<()> {
        for part in self.parts {
            match part {
                CCodePart::String(string) => c_code.push_str(&string),
                CCodePart::Expr(expr) => {
                    // type check
                    expr.init_ty()?;
                    expr.gen(c_code)?
                }
            }
        }
        Ok(())
    }
}

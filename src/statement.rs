use crate::cached::CachedString;
use crate::define::VarDefine;
use crate::error::{err, unexpected_kind, MyResult};
use crate::expr::Expr;
use crate::init_cached::InitCached;
use crate::parse::{Kind, Node};
use crate::scope::Scope;
use crate::span::Span;
use crate::ty::{PrimitiveType, TypeKind};
use crate::util::{Mangle, Visit};
use std::fmt::Write;

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
    pub fn gen(self) -> MyResult<String> {
        use StatementKind::*;
        Ok(match self.kind {
            Return(mut value) => {
                // type check
                value
                    .as_mut()
                    .map(|value| value.init_ty())
                    .transpose()?
                    .unwrap_or_else(|| PrimitiveType::Void.ty())
                    .check(&Scope::current().func_return_type(), self.span)?;

                let mut s = "return".to_string();
                if let Some(value) = value {
                    s.push_str(&value.gen()?);
                }
                s.push(';');

                s
            }
            Break => {
                if !Scope::current().in_loop() {
                    return err("break cant be used outside of loops", self.span);
                }
                "break;".into()
            }
            Continue => {
                if !Scope::current().in_loop() {
                    return err("continue cant be used outside of loops", self.span);
                }
                "continue;".into()
            }
            If {
                mut cond,
                then,
                otherwise,
            } => {
                // type check
                cond.init_ty()?
                    .check(&PrimitiveType::Bool.ty(), self.span)?;

                let mut s = format!("if({}) ", cond.gen()?);
                let scope = Scope::new(false, None);
                s.push_str(&then.gen()?);
                drop(scope);
                if let Some(otherwise) = otherwise {
                    let scope = Scope::new(false, None);
                    s.push_str(&otherwise.gen()?);
                    drop(scope);
                }

                s
            }
            Until { mut cond, block } => {
                // type check
                cond.init_ty()?
                    .check(&PrimitiveType::Bool.ty(), self.span)?;

                let mut s = format!("while(!({})) ", cond.gen()?);
                let scope = Scope::new(true, None);
                s.push_str(&block.gen()?);
                drop(scope);

                s
            }
            For {
                init,
                mut cond,
                update,
                block,
                ..
            } => {
                let scope = Scope::new(true, None);
                let mut s = format!("for({}; ", init.gen()?);

                // type check
                cond.init_ty()?
                    .check(&PrimitiveType::Bool.ty(), self.span)?;

                write!(
                    s,
                    "{}; {}) {}",
                    cond.gen()?,
                    update.gen()?.strip_suffix(';').unwrap(),
                    block.gen()?
                )
                .unwrap();
                drop(scope);
                s
            }
            ExprAssign {
                mut lvalue,
                mut rvalue,
            } => {
                // type check
                lvalue.check_assignable(self.span)?;
                rvalue.init_ty()?.check(&lvalue.init_ty()?, self.span)?;

                format!("{} = {};", lvalue.gen()?, rvalue.gen()?)
            }
            VarDefine(var_define) => format!("{};", var_define.gen()?),
            Expr(mut expr) => {
                // type check
                expr.init_ty()?;
                format!("{};", expr.gen()?)
            }
        })
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
    pub fn gen(self) -> MyResult<String> {
        Ok(format!(
            "{{\n{}\n}}",
            self.0
                .into_iter()
                .map(Statement::gen)
                .collect::<MyResult<Vec<_>>>()?
                .join("\n")
        ))
    }
}

#[derive(Debug, Clone)]
pub struct FuncCall {
    span: Span,
    pub name: CachedString,
    pub args: Vec<Expr>,
    ty: InitCached<TypeKind>,
}

impl Visit for FuncCall {
    fn visit(node: Node) -> Self {
        let span = node.span();
        let mut nodes = node.children_checked(Kind::func_call);

        Self {
            span,
            name: nodes.next().unwrap().as_str().into(),
            args: nodes.visit_rest(),
            ty: Default::default(),
        }
    }
}

impl FuncCall {
    pub fn init_ty(&mut self) -> MyResult<TypeKind> {
        let span = self.span;
        let name = self.name;
        let args = &mut self.args;
        self.ty
            .get_or_try_init(|| {
                Ok(Scope::current()
                    .get_func(
                        name,
                        &mut args
                            .iter_mut()
                            .map(|arg| arg.init_ty())
                            .collect::<MyResult<Vec<_>>>()?,
                        span,
                    )?
                    .ty())
            })
            .map(|r| *r)
    }

    pub fn gen(self) -> MyResult<String> {
        let s = format!(
            "{}({})",
            self.name.to_string().mangle(),
            self.args
                .into_iter()
                .map(Expr::gen)
                .collect::<MyResult<Vec<_>>>()?
                .join(", "),
        );

        Ok(s)
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

    pub fn gen(self) -> MyResult<String> {
        self.parts
            .into_iter()
            .map(|part| match part {
                CCodePart::String(string) => Ok(string),
                CCodePart::Expr(mut expr) => {
                    // type check
                    expr.init_ty()?;
                    expr.gen()
                }
            })
            .collect()
    }
}

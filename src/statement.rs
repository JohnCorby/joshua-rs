use crate::cached::CachedString;
use crate::define::VarDefine;
use crate::error::{err, unexpected_kind, MyResult};
use crate::expr::Expr;
use crate::late_init::LateInit;
use crate::parse::{Kind, Node};
use crate::pass::{Gen, InitType, Visit};
use crate::scope::Scope;
use crate::span::Span;
use crate::ty::{PrimitiveType, TypeKind};
use std::fmt::Write;

#[derive(Debug, Clone)]
pub struct Statement {
    span: Span,
    kind: StatementKind,
}
#[derive(Debug, Clone)]
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
    VarAssign {
        name: CachedString,
        value: Expr,
    },
    VarDefine(VarDefine),
    Expr(Expr),
}

impl Visit for Statement {
    fn visit_impl(node: Node) -> Self {
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
            Kind::var_assign => {
                let mut nodes = node.children();

                VarAssign {
                    name: nodes.next().unwrap().as_str().into(),
                    value: nodes.next().unwrap().visit(),
                }
            }
            Kind::var_define => VarDefine(node.visit()),
            Kind::expr => Expr(node.visit()),

            _ => unexpected_kind(node),
        };

        Self { span, kind }
    }
}

impl Gen for Statement {
    fn span(&self) -> Span {
        self.span
    }

    fn gen_impl(self) -> MyResult<String> {
        use StatementKind::*;
        Ok(match self.kind {
            Return(value) => {
                // type check
                value
                    .as_ref()
                    .map(|value| *value.ty)
                    .unwrap_or_else(|| PrimitiveType::Void.ty())
                    .check(&Scope::current().func_return_type())?;

                let mut s = String::from("return");
                if let Some(value) = value {
                    write!(s, " {}", value.gen()?).unwrap();
                }
                s.push(';');

                s
            }
            Break => {
                if !Scope::current().in_loop() {
                    return err("break cant be used outside of loops");
                }
                "break;".into()
            }
            Continue => {
                if !Scope::current().in_loop() {
                    return err("continue cant be used outside of loops");
                }
                "continue;".into()
            }
            If {
                cond,
                then,
                otherwise,
            } => {
                // type check
                cond.ty.check(&PrimitiveType::Bool.ty())?;

                let mut s = format!("if({}) ", cond.gen()?);
                let scope = Scope::new(false, None);
                s.write_str(&then.gen()?).unwrap();
                drop(scope);
                if let Some(otherwise) = otherwise {
                    let scope = Scope::new(false, None);
                    s.push_str(&otherwise.gen()?);
                    drop(scope);
                }

                s
            }
            Until { cond, block } => {
                // type check
                cond.ty.check(&PrimitiveType::Bool.ty())?;

                let mut s = format!("while(!({})) ", cond.gen()?);
                let scope = Scope::new(true, None);
                s.write_str(&block.gen()?).unwrap();
                drop(scope);

                s
            }
            For {
                init,
                cond,
                update,
                block,
                ..
            } => {
                let scope = Scope::new(true, None);

                // type check
                cond.ty.check(&PrimitiveType::Bool.ty())?;

                let s = format!(
                    "for({}; {}; {}) {}",
                    init.gen()?,
                    cond.gen()?,
                    update.gen()?.strip_suffix(';')?,
                    block.gen()?
                );
                drop(scope);
                s
            }
            VarAssign { name, value } => {
                // type check
                value.ty.check(&Scope::current().get_var(name)?.ty())?;

                format!("{} = {};", name, value.gen()?)
            }
            VarDefine(var_define) => format!("{};", var_define.gen()?),
            Expr(expr) => format!("{};", expr.gen()?),
        })
    }
}

#[derive(Debug, Clone)]
pub struct Block {
    span: Span,
    statements: Vec<Statement>,
}

impl Visit for Block {
    fn visit_impl(node: Node) -> Self {
        let span = node.span();
        let statements = node.children_checked(Kind::block).visit_rest();

        Self { span, statements }
    }
}

impl Gen for Block {
    fn span(&self) -> Span {
        self.span
    }

    fn gen_impl(self) -> MyResult<String> {
        Ok(format!(
            "{{\n{}\n}}",
            self.statements
                .into_iter()
                .map(Gen::gen)
                .collect::<MyResult<Vec<_>>>()?
                .join("\n")
        ))
    }
}

#[derive(Debug, Clone)]
pub struct FuncCall {
    pub span: Span,
    pub name: CachedString,
    pub args: Vec<Expr>,
    pub ty: LateInit<TypeKind>,
}

impl Visit for FuncCall {
    fn visit_impl(node: Node) -> Self {
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

impl InitType for FuncCall {
    fn span(&self) -> Span {
        self.span
    }

    fn init_type_impl(self) -> MyResult<TypeKind> {
        let ty = Scope::current()
            .get_func(
                self.name,
                self.args.iter().map(|arg| *arg.ty).collect::<Vec<_>>(),
            )
            .unwrap()
            .ty();

        self.ty.init(ty);
        Ok(ty)
    }
}

impl Gen for FuncCall {
    fn span(&self) -> Span {
        self.span
    }

    fn gen_impl(self) -> MyResult<String> {
        let s = format!(
            "{}({})",
            self.name,
            self.args
                .clone()
                .into_iter()
                .map(Gen::gen)
                .collect::<MyResult<Vec<_>>>()?
                .join(", "),
        );

        // type check
        Scope::current().get_func(
            self.name,
            self.args.iter().map(|arg| *arg.ty).collect::<Vec<_>>(),
        )?;

        Ok(s)
    }
}

#[derive(Debug, Clone)]
pub struct CCode {
    span: Span,
    parts: Vec<CCodePart>,
}
#[derive(Debug, Clone)]
pub enum CCodePart {
    String(String),
    Expr(Expr),
}

impl Visit for CCode {
    fn visit_impl(node: Node) -> Self {
        let span = node.span();
        let parts = node
            .children_checked(Kind::c_code)
            .into_iter()
            .map(|node| match node.kind() {
                Kind::c_code_str => CCodePart::String(node.as_str().into()),
                Kind::expr => CCodePart::Expr(node.visit()),

                _ => unexpected_kind(node),
            })
            .collect();

        Self { span, parts }
    }
}

impl CCode {
    pub fn ty(&self) -> TypeKind {
        TypeKind::CCode
    }
}

impl Gen for CCode {
    fn span(&self) -> Span {
        self.span
    }

    fn gen_impl(self) -> MyResult<String> {
        self.parts
            .into_iter()
            .map(|part| match part {
                CCodePart::String(string) => Ok(string),
                CCodePart::Expr(expr) => expr.gen(),
            })
            .collect()
    }
}

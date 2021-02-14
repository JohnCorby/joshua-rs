//! handle the painful process that is parsing expressions

use crate::cached::CachedString;
use crate::error::{err, unexpected_kind, Res};
use crate::init_cached::InitCached;
use crate::parse::{Kind, Node};
use crate::scope::{Scope, Symbol};
use crate::span::Span;
use crate::statement::CCode;
use crate::ty::{LiteralType, PrimitiveType, Type, TypeKind};
use crate::util::{Mangle, Visit};

#[derive(Debug, Clone)]
pub struct Expr {
    span: Span,
    kind: ExprKind,
    ty: InitCached<TypeKind>,
}
#[derive(Debug, Clone)]
pub enum ExprKind {
    Binary {
        left: Box<Expr>,
        op: CachedString,
        right: Box<Expr>,
    },
    Unary {
        op: CachedString,
        thing: Box<Expr>,
    },
    Cast {
        thing: Box<Expr>,
        ty: Type,
    },

    MethodCall {
        receiver: Box<Expr>,
        func_call: FuncCall,
    },
    Field {
        receiver: Box<Expr>,
        var: CachedString,
    },

    // primary
    Literal(Literal),
    FuncCall(FuncCall),
    Var(CachedString),

    CCode(CCode),
}

impl Visit for Expr {
    fn visit(node: Node) -> Self {
        let span = node.span();
        use ExprKind::*;
        let kind = match node.kind() {
            Kind::expr => node.children().next().unwrap().visit::<Expr>().kind,
            Kind::equality_expr | Kind::compare_expr | Kind::add_expr | Kind::mul_expr => {
                // left assoc
                let mut nodes = node.children();

                let mut left = nodes.next().unwrap().visit::<Expr>();
                while let Some(op) = nodes.next() {
                    left.kind = Binary {
                        left: left.clone().into(),
                        op: op.as_str().into(),
                        right: nodes.next().unwrap().visit::<Expr>().into(),
                    }
                }

                left.kind
            }
            Kind::unary_expr => {
                // right assoc
                let mut rev_nodes = node.children().rev();

                let mut thing = rev_nodes.next().unwrap().visit::<Expr>();
                for op in rev_nodes {
                    thing.kind = Unary {
                        op: op.as_str().into(),
                        thing: thing.clone().into(),
                    }
                }

                thing.kind
            }
            Kind::cast_expr => {
                // left assoc
                let mut nodes = node.children();

                let mut thing = nodes.next().unwrap().visit::<Expr>();
                for ty_node in nodes {
                    thing.kind = Cast {
                        thing: thing.clone().into(),
                        ty: ty_node.visit(),
                    }
                }

                thing.kind
            }

            Kind::dot_expr => {
                // left assoc
                let mut nodes = node.children();

                let mut left = nodes.next().unwrap().visit::<Expr>();
                for right in nodes {
                    left.kind = match right.kind() {
                        Kind::func_call => MethodCall {
                            receiver: left.clone().into(),
                            func_call: right.visit(),
                        },
                        Kind::ident => Field {
                            receiver: left.clone().into(),
                            var: right.visit_ident(),
                        },

                        _ => unexpected_kind(right),
                    }
                }

                left.kind
            }

            // primary
            Kind::literal => Literal(node.children().next().unwrap().visit()),
            Kind::func_call => FuncCall(node.visit()),
            Kind::ident => Var(node.as_str().into()),

            Kind::c_code => CCode(node.visit()),

            _ => unexpected_kind(node),
        };

        Self {
            span,
            kind,
            ty: Default::default(),
        }
    }
}

impl Expr {
    pub fn init_ty(&mut self) -> Res<TypeKind> {
        let span = self.span;
        let kind = &mut self.kind;
        self.ty
            .get_or_try_init(|| {
                use ExprKind::*;
                Ok(match kind {
                    Binary { left, op, right } => Scope::current()
                        .get_func(*op, [left.init_ty()?, right.init_ty()?], span)?
                        .ty(),
                    Unary { op, thing } => Scope::current()
                        .get_func(*op, [thing.init_ty()?], span)?
                        .ty(),
                    Cast { thing, ty } => Scope::current()
                        .get_func(
                            format!("as {}", ty.init_ty()?.name()).into(),
                            [thing.init_ty()?],
                            span,
                        )?
                        .ty(),

                    MethodCall {
                        receiver,
                        func_call,
                    } => {
                        let mut arg_types = func_call
                            .args
                            .iter_mut()
                            .map(|arg| arg.init_ty())
                            .collect::<Res<Vec<_>>>()?;
                        arg_types.insert(0, receiver.init_ty()?);

                        Scope::current()
                            .get_func(func_call.name, arg_types, span)?
                            .ty()
                    }
                    Field { receiver, var } => {
                        let struct_name = match receiver.init_ty()? {
                            TypeKind::Struct(struct_name) => struct_name,
                            ty => {
                                return err(format!("expected struct type, but got {}", ty), span)
                            }
                        };
                        let symbol = Scope::current().get_struct(struct_name, span)?;
                        let field_types = match &symbol {
                            Symbol::Struct { field_types, .. } => field_types,
                            _ => {
                                return err(
                                    format!("expected struct symbol, but got {}", symbol),
                                    span,
                                )
                            }
                        };
                        match field_types.get(&var) {
                            Some(&field_type) => field_type,
                            None => {
                                return err(format!("no field named {} in {}", var, symbol), span)
                            }
                        }
                    }

                    Literal(literal) => literal.ty(),
                    FuncCall(func_call) => func_call.init_ty()?,
                    Var(name) => Scope::current().get_var(*name, span)?.ty(),

                    CCode(c_code) => c_code.ty(),
                })
            })
            .copied()
    }

    pub fn check_assignable(&self, span: impl Into<Option<Span>>) -> Res<()> {
        use ExprKind::*;
        let is_assignable = matches!(self.kind, Field { .. } | Var(_));

        if !is_assignable {
            err("expr is not assignable", span)
        } else {
            Ok(())
        }
    }

    pub fn gen(self) -> Res<String> {
        use ExprKind::*;
        Ok(match self.kind {
            Binary { left, op, right } => format!("({} {} {})", left.gen()?, op, right.gen()?),
            Unary { op, thing } => format!("({}{})", op, thing.gen()?),
            Cast { thing, ty } => format!("(({}) {})", ty.gen()?, thing.gen()?),

            MethodCall {
                receiver,
                mut func_call,
            } => {
                func_call.args.insert(0, *receiver);
                func_call.gen()?
            }
            Field { receiver, var } => {
                format!("{}.{}", receiver.gen()?, var.to_string().mangle())
            }

            Literal(literal) => literal.gen(),
            FuncCall(func_call) => func_call.gen()?,
            Var(name) => name.to_string().mangle(),

            CCode(c_code) => c_code.gen()?,
        })
    }
}

#[derive(Debug, Clone)]
pub enum Literal {
    Float(f64),
    Int(i64),
    Bool(bool),
    Char(char),
    Str(String),
}

impl Visit for Literal {
    fn visit(node: Node) -> Self {
        use Literal::*;
        match node.kind() {
            Kind::float_literal => Float(node.as_str().parse().unwrap()),
            Kind::int_literal => Int(node.as_str().parse().unwrap()),
            Kind::bool_literal => Bool(node.as_str().parse().unwrap()),
            Kind::char_literal => Char(node.children().next().unwrap().as_str().parse().unwrap()),
            Kind::str_literal => Str(node.children().next().unwrap().as_str().into()),

            _ => unexpected_kind(node),
        }
    }
}

impl Literal {
    pub fn ty(&self) -> TypeKind {
        use Literal::*;
        match self {
            Float(_) => LiteralType::Float.ty(),
            Int(_) => LiteralType::Int.ty(),
            Bool(_) => PrimitiveType::Bool.ty(),
            Char(_) => PrimitiveType::Char.ty(),
            Str(_) => todo!("usage of string literals is not yet supported"),
        }
    }

    pub fn gen(self) -> String {
        use Literal::*;
        match self {
            Float(value) => value.to_string(),
            Int(value) => value.to_string(),
            Bool(value) => (value as u8).to_string(),
            Char(value) => format!("'{}'", value),
            Str(value) => format!("\"{}\"", value),
        }
    }
}

pub trait VisitIdent {
    fn visit_ident(self) -> CachedString;
}
impl VisitIdent for Node<'_> {
    fn visit_ident(self) -> CachedString {
        let str = self.as_str();
        str.strip_prefix('`')
            .unwrap_or(str)
            .strip_suffix('`')
            .unwrap_or(str)
            .into()
    }
}

#[derive(Debug, Clone)]
pub struct FuncCall {
    span: Span,
    name: CachedString,
    args: Vec<Expr>,
    ty: InitCached<TypeKind>,
}

impl Visit for FuncCall {
    fn visit(node: Node) -> Self {
        let span = node.span();
        let mut nodes = node.children_checked(Kind::func_call);

        Self {
            span,
            name: nodes.next().unwrap().visit_ident(),
            args: nodes.visit_rest(),
            ty: Default::default(),
        }
    }
}

impl FuncCall {
    pub fn init_ty(&mut self) -> Res<TypeKind> {
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
                            .collect::<Res<Vec<_>>>()?,
                        span,
                    )?
                    .ty())
            })
            .copied()
    }

    pub fn gen(mut self) -> Res<String> {
        let arg_types = self
            .args
            .iter_mut()
            .map(|arg| arg.init_ty())
            .collect::<Res<Vec<_>>>()?;

        // don't mangle func main (entry point)
        let mut name_gen = self.name.to_string();
        if name_gen != "main" {
            name_gen = format!(
                "{}({})",
                name_gen,
                arg_types
                    .iter()
                    .map(TypeKind::name)
                    .collect::<Vec<_>>()
                    .join(", ")
            )
            .mangle();
        }

        Ok(format!(
            "{}({})",
            name_gen,
            self.args
                .into_iter()
                .map(Expr::gen)
                .collect::<Res<Vec<_>>>()?
                .join(", "),
        ))
    }
}

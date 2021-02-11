//! handle the painful process that is parsing expressions

use crate::cached::CachedString;
use crate::error::{err, unexpected_kind, MyResult};
use crate::init_cached::InitCached;
use crate::parse::{Kind, Node};
use crate::scope::{Scope, Symbol};
use crate::span::Span;
use crate::statement::{CCode, FuncCall};
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
        let ty = InitCached::new("expr type");
        use ExprKind::*;
        let kind = match node.kind() {
            Kind::expr => node.children().next().unwrap().visit::<Expr>().kind,
            Kind::equality_expr | Kind::compare_expr | Kind::add_expr | Kind::mul_expr => {
                // left assoc
                let mut nodes = node.children();

                let mut left = nodes.next().unwrap().visit::<Expr>();
                let mut span = left.span;
                let mut kind = left.kind;
                while let Some(op) = nodes.next() {
                    left = Expr { span, kind, ty };
                    span = left.span;
                    kind = Binary {
                        left: left.into(),
                        op: op.as_str().into(),
                        right: nodes.next().unwrap().visit::<Expr>().into(),
                    };
                }

                kind
            }
            Kind::unary_expr => {
                // right assoc
                let mut rev_nodes = node.children().rev();

                let mut thing = rev_nodes.next().unwrap().visit::<Expr>();
                let mut span = thing.span;
                let mut kind = thing.kind;
                for op in rev_nodes {
                    thing = Expr { span, kind, ty };
                    span = thing.span;
                    kind = Unary {
                        op: op.as_str().into(),
                        thing: thing.into(),
                    };
                }

                kind
            }
            Kind::cast_expr => {
                // left assoc
                let mut nodes = node.children();

                let mut thing = nodes.next().unwrap().visit::<Expr>();
                let mut span = thing.span;
                let mut kind = thing.kind;
                for ty_node in nodes {
                    thing = Expr { span, kind, ty };
                    span = thing.span;
                    kind = Cast {
                        thing: thing.into(),
                        ty: ty_node.visit(),
                    }
                }

                kind
            }

            Kind::dot_expr => {
                // left assoc
                let mut nodes = node.children();

                let mut left = nodes.next().unwrap().visit::<Expr>();
                let mut kind = left.kind;
                let mut span = left.span;
                for right in nodes {
                    left = Expr { span, kind, ty };
                    span = left.span;
                    kind = match right.kind() {
                        Kind::func_call => MethodCall {
                            receiver: left.into(),
                            func_call: right.visit(),
                        },
                        Kind::ident => Field {
                            receiver: left.into(),
                            var: right.as_str().into(),
                        },

                        _ => unexpected_kind(right),
                    }
                }

                kind
            }

            // primary
            Kind::literal => Literal(node.children().next().unwrap().visit()),
            Kind::func_call => FuncCall(node.visit()),
            Kind::ident => Var(node.as_str().into()),

            Kind::c_code => CCode(node.visit()),

            _ => unexpected_kind(node),
        };

        Self { span, kind, ty }
    }
}

impl Expr {
    pub fn init_ty(&mut self) -> MyResult<TypeKind> {
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
                            format!("as {}", ty.init_ty()?).into(),
                            [thing.init_ty()?],
                            span,
                        )?
                        .ty(),

                    MethodCall {
                        receiver,
                        func_call,
                    } => {
                        let receiver_ty = receiver.init_ty()?;
                        let struct_name = match receiver_ty {
                            TypeKind::Struct(struct_name) => struct_name,
                            _ => {
                                return err(
                                    format!("expected struct type, but got {}", receiver_ty),
                                    span,
                                )
                            }
                        };
                        let name = format!("{}::{}", struct_name, func_call.name).into();
                        let mut arg_types = func_call
                            .args
                            .iter_mut()
                            .map(|arg| arg.init_ty())
                            .collect::<MyResult<Vec<_>>>()?;
                        arg_types.insert(0, receiver_ty);

                        Scope::current().get_func(name, arg_types, span)?.ty()
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
            .map(|r| *r)
    }

    pub fn check_assignable(&self, span: impl Into<Option<Span>>) -> MyResult<()> {
        use ExprKind::*;
        let is_assignable = matches!(self.kind, Field { .. } | Var(_));

        if !is_assignable {
            err("expr is not assignable", span)
        } else {
            Ok(())
        }
    }

    pub fn gen(self) -> MyResult<String> {
        use ExprKind::*;
        Ok(match self.kind {
            Binary { left, op, right } => format!("({} {} {})", left.gen()?, op, right.gen()?),
            Unary { op, thing } => format!("({}{})", op, thing.gen()?),
            Cast { thing, ty } => format!("(({}) {})", ty.gen()?, thing.gen()?),

            MethodCall {
                mut receiver,
                mut func_call,
            } => {
                let struct_name = match receiver.init_ty()? {
                    TypeKind::Struct(struct_name) => struct_name,
                    ty => return err(format!("expected struct type, but got {}", ty), self.span),
                };
                func_call.name = format!("{}::{}", struct_name, func_call.name).into();
                func_call.args.insert(0, *receiver);

                func_call.gen()?
            }
            Field { receiver, var } => {
                format!("({}.{})", receiver.gen()?, var.to_string().mangle())
            }

            Literal(literal) => literal.gen()?,
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

    pub fn gen(self) -> MyResult<String> {
        use Literal::*;
        Ok(match self {
            Float(value) => value.to_string(),
            Int(value) => value.to_string(),
            Bool(value) => (value as u8).to_string(),
            Char(value) => format!("'{}'", value),
            Str(value) => format!("\"{}\"", value),
        })
    }
}

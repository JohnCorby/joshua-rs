//! handle the painful process that is parsing expressions

use crate::context::Ctx;
use crate::error::{err, unexpected_kind, Res};
use crate::interned_string::{Intern, InternedStr};
use crate::parse::{Kind, Node};
use crate::scope::Symbol;
use crate::span::Span;
use crate::statement::CCode;
use crate::ty::{LiteralType, PrimitiveType, Type, TypeNode};
use crate::util::{Mangle, Visit};
use std::lazy::OnceCell;

#[derive(Debug, Clone)]
pub struct Expr<'i> {
    span: Span<'i>,
    kind: ExprKind<'i>,
    ty: OnceCell<Type<'i>>,
}
#[derive(Debug, Clone)]
pub enum ExprKind<'i> {
    Binary {
        left: Box<Expr<'i>>,
        op: InternedStr<&'i str>,
        right: Box<Expr<'i>>,
    },
    Unary {
        op: InternedStr<&'i str>,
        thing: Box<Expr<'i>>,
    },
    Cast {
        thing: Box<Expr<'i>>,
        ty_node: TypeNode<'i>,
    },

    MethodCall {
        receiver: Box<Expr<'i>>,
        func_call: FuncCall<'i>,
    },
    Field {
        receiver: Box<Expr<'i>>,
        var: InternedStr<&'i str>,
    },

    // primary
    Literal(Literal<'i>),
    FuncCall(FuncCall<'i>),
    Var(InternedStr<&'i str>),

    CCode(CCode<'i>),
}

impl<'i> Visit<'i> for Expr<'i> {
    fn visit(node: Node<'i>, ctx: &mut Ctx<'i>) -> Self {
        let span = node.span();
        use ExprKind::*;
        let kind = match node.kind() {
            Kind::expr => node.children().next().unwrap().visit::<Expr<'i>>(ctx).kind,
            Kind::equality_expr | Kind::compare_expr | Kind::add_expr | Kind::mul_expr => {
                // left assoc
                let mut nodes = node.children();

                let mut left = nodes.next().unwrap().visit::<Expr<'i>>(ctx);
                while let Some(op) = nodes.next() {
                    left.kind = Binary {
                        left: left.clone().into(),
                        op: op.str().intern(ctx),
                        right: nodes.next().unwrap().visit::<Expr<'i>>(ctx).into(),
                    }
                }

                left.kind
            }
            Kind::unary_expr => {
                // right assoc
                let mut rev_nodes = node.children().rev();

                let mut thing = rev_nodes.next().unwrap().visit::<Expr<'i>>(ctx);
                for op in rev_nodes {
                    thing.kind = Unary {
                        op: op.str().intern(ctx),
                        thing: thing.clone().into(),
                    }
                }

                thing.kind
            }
            Kind::cast_expr => {
                // left assoc
                let mut nodes = node.children();

                let mut thing = nodes.next().unwrap().visit::<Expr<'i>>(ctx);
                for ty_node in nodes {
                    thing.kind = Cast {
                        thing: thing.clone().into(),
                        ty_node: ty_node.visit(ctx),
                    }
                }

                thing.kind
            }

            Kind::dot_expr => {
                // left assoc
                let mut nodes = node.children();

                let mut left = nodes.next().unwrap().visit::<Expr<'i>>(ctx);
                for right in nodes {
                    left.kind = match right.kind() {
                        Kind::func_call => MethodCall {
                            receiver: left.clone().into(),
                            func_call: right.visit(ctx),
                        },
                        Kind::ident => Field {
                            receiver: left.clone().into(),
                            var: right.visit_ident(ctx),
                        },

                        _ => unexpected_kind(right),
                    }
                }

                left.kind
            }

            // primary
            Kind::literal => Literal(node.children().next().unwrap().visit(ctx)),
            Kind::func_call => FuncCall(node.visit(ctx)),
            Kind::ident => Var(node.visit_ident(ctx)),

            Kind::c_code => CCode(node.visit(ctx)),

            _ => unexpected_kind(node),
        };

        Self {
            span,
            kind,
            ty: Default::default(),
        }
    }
}

impl<'i> Expr<'i> {
    pub fn init_ty(&self, ctx: &mut Ctx<'i>) -> Res<'i, Type<'i>> {
        self.ty
            .get_or_try_init(|| {
                use ExprKind::*;
                Ok(match &self.kind {
                    Binary { left, op, right } => {
                        let left = left.init_ty(ctx)?;
                        let right = right.init_ty(ctx)?;
                        ctx.scopes.get_func(*op, [left, right], self.span)?.ty()
                    }
                    Unary { op, thing } => {
                        let thing = thing.init_ty(ctx)?;
                        ctx.scopes.get_func(*op, [thing], self.span)?.ty()
                    }
                    Cast { thing, ty_node } => {
                        // fixme literals are hacky as shit
                        if let Type::Literal(_) = thing.init_ty(ctx)? {
                            ty_node.init_ty(ctx)?
                        } else {
                            // let name = format!("as {}", ty_node.init_ty(ctx)?.name());
                            let name = "bruh".intern(ctx);
                            let thing = thing.init_ty(ctx)?;
                            ctx.scopes.get_func(name, [thing], self.span)?.ty()
                        }
                    }

                    MethodCall {
                        receiver,
                        func_call,
                    } => {
                        let mut arg_types = func_call
                            .args
                            .iter()
                            .map(|arg| arg.init_ty(ctx))
                            .collect::<Res<'i, Vec<_>>>()?;
                        arg_types.insert(0, receiver.init_ty(ctx)?);

                        ctx.scopes
                            .get_func(func_call.name, arg_types, self.span)?
                            .ty()
                    }
                    Field { receiver, var } => {
                        let struct_name = match receiver.init_ty(ctx)? {
                            Type::Struct(struct_name) => struct_name,
                            ty => {
                                return err(
                                    format!("expected struct type, but got {}", ty),
                                    self.span,
                                )
                            }
                        };
                        let symbol = ctx.scopes.get_struct(struct_name, self.span)?;
                        let field_types = match &symbol {
                            Symbol::Struct { field_types, .. } => field_types,
                            _ => {
                                return err(
                                    format!("expected struct symbol, but got {}", symbol),
                                    self.span,
                                )
                            }
                        };
                        match field_types.get(&var) {
                            Some(&field_type) => field_type,
                            None => {
                                return err(
                                    format!("no field named {} in {}", var, symbol),
                                    self.span,
                                )
                            }
                        }
                    }

                    Literal(literal) => literal.ty(),
                    FuncCall(func_call) => func_call.init_ty(ctx)?,
                    Var(name) => ctx.scopes.get_var(*name, self.span)?.ty(),

                    CCode(c_code) => c_code.ty(),
                })
            })
            .copied()
    }

    pub fn check_assignable(&self, span: impl Into<Option<Span<'i>>>) -> Res<'i, ()> {
        use ExprKind::*;
        let is_assignable = matches!(self.kind, Field { .. } | Var(_));

        if !is_assignable {
            err("expr is not assignable", span)
        } else {
            Ok(())
        }
    }

    pub fn gen(self, ctx: &mut Ctx<'i>) -> Res<'i, ()> {
        use ExprKind::*;
        match self.kind {
            Binary { left, op, right } => {
                // c_code.push('(');
                // left.gen(c_code)?;
                // c_code.push(' ');
                // c_code.push_str(&op.to_string());
                // c_code.push(' ');
                // right.gen(c_code)?;
                // c_code.push(')');
                self::FuncCall {
                    span: self.span,
                    name: op,
                    args: vec![*left, *right],
                    ty: self.ty,
                }
                .gen(ctx)?;
            }
            Unary { op, thing } => {
                // c_code.push_str(&op.to_string());
                // thing.gen(c_code)?;
                self::FuncCall {
                    span: self.span,
                    name: op,
                    args: vec![*thing],
                    ty: self.ty,
                }
                .gen(ctx)?;
            }
            Cast { thing, ty_node } => {
                // fixme literals are hacky as shit
                if let Type::Literal(_) = thing.init_ty(ctx)? {
                    ctx.o.push('(');
                    ty_node.gen(ctx)?;
                    ctx.o.push_str(") ");
                    thing.gen(ctx)?;
                } else {
                    // let name = format!("as {}", ty_node.init_ty(ctx)?.name());
                    self::FuncCall {
                        span: self.span,
                        // name: name.intern(ctx),
                        name: "hello".intern(ctx),
                        args: vec![*thing],
                        ty: self.ty,
                    }
                    .gen(ctx)?;
                }
            }

            MethodCall {
                receiver,
                mut func_call,
            } => {
                func_call.args.insert(0, *receiver);
                func_call.gen(ctx)?
            }
            Field { receiver, var } => {
                receiver.gen(ctx)?;
                ctx.o.push('.');
                ctx.o.push_str(&var.mangle());
            }

            Literal(literal) => literal.gen(ctx),
            FuncCall(func_call) => func_call.gen(ctx)?,
            Var(name) => ctx.o.push_str(&name.mangle()),

            CCode(c_code) => c_code.gen(ctx)?,
        }
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct FuncCall<'i> {
    span: Span<'i>,
    name: InternedStr<&'i str>,
    args: Vec<Expr<'i>>,
    ty: OnceCell<Type<'i>>,
}

impl<'i> Visit<'i> for FuncCall<'i> {
    fn visit(node: Node<'i>, ctx: &mut Ctx<'i>) -> Self {
        let span = node.span();
        let mut nodes = node.children_checked(Kind::func_call);

        Self {
            span,
            name: nodes.next().unwrap().visit_ident(ctx),
            args: nodes.visit_rest(ctx),
            ty: Default::default(),
        }
    }
}

impl<'i> FuncCall<'i> {
    pub fn init_ty(&self, ctx: &mut Ctx<'i>) -> Res<'i, Type<'i>> {
        self.ty
            .get_or_try_init(|| {
                let arg_types = self
                    .args
                    .iter()
                    .map(|arg| arg.init_ty(ctx))
                    .collect::<Res<'i, Vec<_>>>()?;
                Ok(ctx.scopes.get_func(self.name, arg_types, self.span)?.ty())
            })
            .copied()
    }

    pub fn gen(self, ctx: &mut Ctx<'i>) -> Res<'i, ()> {
        let arg_types = self
            .args
            .iter()
            .map(|arg| arg.init_ty(ctx))
            .collect::<Res<'i, Vec<_>>>()?;

        // don't mangle func main (entry point)
        let mut name_mangled = self.name.to_string();
        if name_mangled != "main" {
            name_mangled = format!(
                "{}({})",
                name_mangled,
                arg_types
                    .iter()
                    .map(|ty| ty.name())
                    .collect::<Vec<_>>()
                    .join(", ")
            )
            .mangle();
        }

        ctx.o.push_str(&name_mangled);
        ctx.o.push('(');
        for arg in self.args {
            arg.gen(ctx)?;
            ctx.o.push_str(", ")
        }
        if ctx.o.ends_with(", ") {
            ctx.o.pop();
            ctx.o.pop();
        }
        ctx.o.push(')');
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub enum Literal<'i> {
    Float(f64),
    Int(i64),
    Bool(bool),
    Char(char),
    Str(&'i str),
}

impl<'i> Visit<'i> for Literal<'i> {
    fn visit(node: Node<'i>, _: &mut Ctx<'i>) -> Self {
        use Literal::*;
        match node.kind() {
            Kind::float_literal => Float(node.str().parse().unwrap()),
            Kind::int_literal => Int(node.str().parse().unwrap()),
            Kind::bool_literal => Bool(node.str().parse().unwrap()),
            Kind::char_literal => Char(node.children().next().unwrap().str().parse().unwrap()),
            Kind::str_literal => Str(node.children().next().unwrap().str()),

            _ => unexpected_kind(node),
        }
    }
}

impl<'i> Literal<'i> {
    pub fn ty(&self) -> Type<'i> {
        use Literal::*;
        match self {
            Float(_) => LiteralType::Float.ty(),
            Int(_) => LiteralType::Int.ty(),
            Bool(_) => PrimitiveType::Bool.ty(),
            Char(_) => PrimitiveType::Char.ty(),
            Str(_) => todo!("usage of string literals is not yet supported"),
        }
    }

    pub fn gen(self, ctx: &mut Ctx<'i>) {
        use Literal::*;
        ctx.o.push_str(&match self {
            Float(value) => value.to_string(),
            Int(value) => value.to_string(),
            Bool(value) => (value as u8).to_string(),
            Char(value) => format!("'{}'", value),
            Str(value) => format!("\"{}\"", value),
        })
    }
}

impl<'i> Node<'i> {
    pub fn visit_ident(&self, ctx: &mut Ctx<'i>) -> InternedStr<&'i str> {
        let str = self.str();
        str.strip_prefix('`')
            .unwrap_or(str)
            .strip_suffix('`')
            .unwrap_or(str)
            .intern(ctx)
    }
}

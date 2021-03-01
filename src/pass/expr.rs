//! handle the painful process that is parsing expressions

use crate::context::Ctx;
use crate::error::{err, unexpected_kind, Res};
use crate::parse::{Kind, Node};
use crate::pass::statement::CCode;
use crate::pass::ty::{LiteralType, PrimitiveType, Type, TypeNode};
use crate::span::Span;
use crate::util::interned_str::{Intern, InternedStr};
use crate::util::late_init::LateInit;
use crate::util::{Mangle, Visit};

#[derive(Debug, Clone)]
pub struct Expr<'i> {
    pub span: Span<'i>,
    pub kind: ExprKind<'i>,
    pub ty: LateInit<Type<'i>>,
}
#[derive(Debug, Clone)]
pub enum ExprKind<'i> {
    Binary {
        left: Box<Expr<'i>>,
        op: InternedStr<'i>,
        right: Box<Expr<'i>>,
    },
    Unary {
        op: InternedStr<'i>,
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
        var: InternedStr<'i>,
    },

    // primary
    Literal(Literal<'i>),
    FuncCall(FuncCall<'i>),
    Var(InternedStr<'i>),

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
    pub fn check_assignable(&self, span: Option<Span<'i>>) -> Res<'i, ()> {
        use ExprKind::*;
        let is_assignable = matches!(self.kind, Field { .. } | Var(_));

        if !is_assignable {
            err("expr is not assignable", span)
        } else {
            Ok(())
        }
    }

    pub fn gen(self, ctx: &mut Ctx<'i>) {
        use ExprKind::*;
        match self.kind {
            Binary { left, op, right } => {
                self::FuncCall {
                    span: self.span,
                    name: op,
                    generic_replacements: vec![],
                    args: vec![*left, *right],
                    ty: Default::default(),
                }
                .gen(ctx);
            }
            Unary { op, thing } => {
                self::FuncCall {
                    span: self.span,
                    name: op,
                    generic_replacements: vec![],
                    args: vec![*thing],
                    ty: Default::default(),
                }
                .gen(ctx);
            }
            Cast { thing, ty_node } => {
                // fixme literal casting is hacky as shit
                if let Type::Literal(_) = *thing.ty {
                    ctx.o.push('(');
                    ty_node.gen(ctx);
                    ctx.o.push_str(") ");
                    thing.gen(ctx);
                } else {
                    self::FuncCall {
                        span: self.span,
                        name: format!("as {}", ty_node.ty.name()).intern(ctx),
                        generic_replacements: vec![],
                        args: vec![*thing],
                        ty: Default::default(),
                    }
                    .gen(ctx);
                }
            }

            MethodCall {
                receiver,
                mut func_call,
            } => {
                func_call.args.insert(0, *receiver);
                func_call.gen(ctx)
            }
            Field { receiver, var } => {
                receiver.gen(ctx);
                ctx.o.push('.');
                ctx.o.push_str(&var.mangle());
            }

            Literal(literal) => literal.gen(ctx),
            FuncCall(func_call) => func_call.gen(ctx),
            Var(name) => ctx.o.push_str(&name.mangle()),

            CCode(c_code) => c_code.gen(ctx),
        }
    }
}

#[derive(Debug, Clone)]
pub struct FuncCall<'i> {
    pub span: Span<'i>,
    pub name: InternedStr<'i>,
    pub generic_replacements: Vec<TypeNode<'i>>,
    pub args: Vec<Expr<'i>>,
    pub ty: LateInit<Type<'i>>,
}

impl<'i> Visit<'i> for FuncCall<'i> {
    fn visit(node: Node<'i>, ctx: &mut Ctx<'i>) -> Self {
        let span = node.span();
        let mut nodes = node.children_checked(Kind::func_call);

        Self {
            span,
            name: nodes.next().unwrap().visit_ident(ctx),
            generic_replacements: nodes
                .next()
                .unwrap()
                .children_checked(Kind::func_call_generics)
                .map(|node| node.visit(ctx))
                .collect(),
            args: nodes.visit_rest(ctx),
            ty: Default::default(),
        }
    }
}

impl<'i> FuncCall<'i> {
    pub fn gen(self, ctx: &mut Ctx<'i>) {
        ctx.o.push_str(
            &self
                .name
                .mangle_func(&self.args.iter().map(|it| *it.ty).collect::<Vec<_>>()),
        );
        ctx.o.push('(');
        for arg in self.args {
            arg.gen(ctx);
            ctx.o.push_str(", ")
        }
        if ctx.o.ends_with(", ") {
            ctx.o.pop();
            ctx.o.pop();
        }
        ctx.o.push(')');
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
    pub fn visit_ident(&self, ctx: &mut Ctx<'i>) -> InternedStr<'i> {
        let str = self.str();
        str.strip_prefix('`')
            .unwrap_or(str)
            .strip_suffix('`')
            .unwrap_or(str)
            .intern(ctx)
    }
}

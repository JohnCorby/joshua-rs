//! handle the painful process that is parsing expressions

use crate::cached::CachedString;
use crate::error::{err, unexpected_rule, MyResult};
use crate::parse::{Node, Rule};
use crate::pass::{Gen, Visit};
use crate::scope::{Scope, Symbol};
use crate::span::Span;
use crate::statement::{CCode, FuncCall};
use crate::ty::{HasType, LiteralType, PrimitiveType, Type};
use crate::with::{ToWith, WithSpan};

#[derive(Debug, Clone)]
pub enum Expr {
    Binary {
        left: Box<WithSpan<Expr>>,
        op: WithSpan<CachedString>,
        right: Box<WithSpan<Expr>>,
    },
    Unary {
        op: WithSpan<CachedString>,
        thing: Box<WithSpan<Expr>>,
    },
    Cast {
        thing: Box<WithSpan<Expr>>,
        ty: WithSpan<Type>,
    },

    MethodCall {
        receiver: Box<WithSpan<Expr>>,
        func_call: WithSpan<FuncCall>,
    },
    Field {
        receiver: Box<WithSpan<Expr>>,
        var: WithSpan<CachedString>,
    },

    // primary
    Literal(Literal),
    FuncCall(FuncCall),
    Var(CachedString),

    CCode(CCode),
}

impl Visit for Expr {
    fn visit_impl(node: Node) -> Self {
        let span = node.span();
        match node.rule() {
            Rule::expr => node.children().next().unwrap().visit().0,
            Rule::equality_expr | Rule::compare_expr | Rule::add_expr | Rule::mul_expr => {
                // left assoc
                let mut nodes = node.children();

                let mut left: WithSpan<Expr> = nodes.next().unwrap().visit();
                while let Some(op) = nodes.next() {
                    left = Self::Binary {
                        left: left.into(),
                        op: op.as_cached_str_with_span(),
                        right: nodes.next().unwrap().visit::<Expr>().into(),
                    }
                    .with(span)
                }

                left.0
            }
            Rule::unary_expr => {
                // right assoc
                let mut rev_nodes = node.children().rev();

                let mut thing: WithSpan<Expr> = rev_nodes.next().unwrap().visit();
                for op in rev_nodes {
                    thing = Self::Unary {
                        op: op.as_cached_str_with_span(),
                        thing: thing.into(),
                    }
                    .with(span)
                }

                thing.0
            }
            Rule::cast_expr => {
                // left assoc
                let mut nodes = node.children();

                let mut thing: WithSpan<Expr> = nodes.next().unwrap().visit();
                for ty in nodes {
                    thing = Self::Cast {
                        thing: thing.into(),
                        ty: ty.visit(),
                    }
                    .with(span)
                }

                thing.0
            }

            Rule::dot_expr => {
                // left assoc
                let mut nodes = node.children();

                let mut left: WithSpan<Expr> = nodes.next().unwrap().visit();
                for right in nodes {
                    left = match right.rule() {
                        Rule::func_call => Self::MethodCall {
                            receiver: left.into(),
                            func_call: right.visit(),
                        },
                        Rule::ident => Self::Field {
                            receiver: left.into(),
                            var: right.as_cached_str_with_span(),
                        },

                        _ => unexpected_rule(right),
                    }
                    .with(span)
                }

                left.0
            }

            // primary
            Rule::literal => Self::Literal(node.children().next().unwrap().visit().0),
            Rule::func_call => Self::FuncCall(node.visit().0),
            Rule::ident => Self::Var(node.as_str().into()),

            Rule::c_code => Self::CCode(node.visit().0),

            _ => unexpected_rule(node),
        }
    }
}

impl Gen for WithSpan<Expr> {
    fn span(&self) -> Span {
        self.1
    }

    fn gen_impl(self) -> MyResult<String> {
        Ok(match self.0 {
            Expr::Binary { left, op, right } => {
                let s = format!("({} {} {})", left.clone().gen()?, *op, right.clone().gen()?);
                // type check
                Scope::current().get_func(*op, [left.ty(), right.ty()])?;
                s
            }
            Expr::Unary { op, thing } => {
                let s = format!("({}{})", *op, thing.clone().gen()?);
                // type check
                Scope::current().get_func(*op, [thing.ty()])?;
                s
            }
            Expr::Cast { thing, ty } => {
                let s = format!("(({}) {})", ty.gen()?, thing.clone().gen()?);
                // type check
                Scope::current().get_func(format!("as {}", *ty).into(), [thing.ty()])?;
                s
            }

            Expr::MethodCall {
                receiver,
                mut func_call,
            } => {
                let struct_name = match receiver.ty() {
                    Type::Named(struct_name) => struct_name,
                    ty => return err(format!("expected struct type, but got {}", ty)),
                };
                func_call.name = func_call
                    .name
                    .map(|name| format!("{}.{}", struct_name, name).into());
                func_call.args.insert(0, *receiver);

                func_call.gen()?
            }
            Expr::Field { receiver, var } => {
                let s = format!("({}.{})", receiver.clone().gen()?, *var);

                // symbol check
                let struct_name = match receiver.ty() {
                    Type::Named(struct_name) => struct_name,
                    ty => return err(format!("expected struct type, but got {}", ty)),
                };
                let symbol = Scope::current().get_type(struct_name).unwrap();
                let field_types = match &symbol {
                    Symbol::Struct { field_types, .. } => field_types,
                    _ => return err(format!("expected struct symbol, but got {}", symbol)),
                };
                match field_types.get(&var) {
                    Some(field_type) => *field_type,
                    None => return err(format!("no field named {} in {}", *var, symbol)),
                };

                s
            }

            Expr::Literal(literal) => literal.with(self.1).gen()?,
            Expr::FuncCall(func_call) => func_call.with(self.1).gen()?,
            Expr::Var(name) => {
                Scope::current().get_var(name)?;
                name.to_string()
            }

            Expr::CCode(c_code) => c_code.with(self.1).gen()?,
        })
    }
}

impl HasType for Expr {
    fn ty(&self) -> Type {
        match self {
            Expr::Binary { left, op, right } => Scope::current()
                .get_func(op.0, [left.ty(), right.ty()])
                .unwrap()
                .ty(),
            Expr::Unary { op, thing } => {
                Scope::current().get_func(op.0, [thing.ty()]).unwrap().ty()
            }
            Expr::Cast { thing, ty } => Scope::current()
                .get_func(format!("as {}", **ty).into(), [thing.ty()])
                .unwrap()
                .ty(),

            Expr::MethodCall {
                receiver,
                func_call,
            } => {
                let receiver_ty = receiver.ty();
                let struct_name = match receiver_ty {
                    Type::Named(struct_name) => struct_name,
                    _ => unreachable!(),
                };
                let name = *func_call
                    .name
                    .map(|name| format!("{}.{}", struct_name, name).into());
                let mut arg_types = func_call
                    .args
                    .iter()
                    .map(|arg| arg.ty())
                    .collect::<Vec<_>>();
                arg_types.insert(0, receiver_ty);

                Scope::current().get_func(name, arg_types).unwrap().ty()
            }
            Expr::Field { receiver, var } => {
                let struct_name = match receiver.ty() {
                    Type::Named(struct_name) => struct_name,
                    _ => unreachable!(),
                };
                let field_types = match Scope::current().get_type(struct_name).unwrap() {
                    Symbol::Struct { field_types, .. } => field_types,
                    _ => unreachable!(),
                };
                *field_types.get(var).unwrap()
            }

            Expr::Literal(literal) => literal.ty(),
            Expr::FuncCall(func_call) => func_call.ty(),
            Expr::Var(name) => Scope::current().get_var(*name).unwrap().ty(),

            Expr::CCode(c_code) => c_code.ty(),
        }
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
    fn visit_impl(node: Node) -> Self {
        match node.rule() {
            Rule::float_literal => Self::Float(node.as_str().parse().unwrap()),
            Rule::int_literal => Self::Int(node.as_str().parse().unwrap()),
            Rule::bool_literal => Self::Bool(node.as_str().parse().unwrap()),
            Rule::char_literal => {
                Self::Char(node.children().next().unwrap().as_str().parse().unwrap())
            }
            Rule::str_literal => Self::Str(node.children().next().unwrap().as_str().into()),

            _ => unexpected_rule(node),
        }
    }
}

impl Gen for WithSpan<Literal> {
    fn span(&self) -> Span {
        self.1
    }

    fn gen_impl(self) -> MyResult<String> {
        Ok(match &*self {
            Literal::Float(value) => value.to_string(),
            Literal::Int(value) => value.to_string(),
            Literal::Bool(value) => (*value as u8).to_string(),
            Literal::Char(value) => format!("'{}'", value),
            Literal::Str(value) => format!("\"{}\"", value),
        })
    }
}

impl HasType for Literal {
    fn ty(&self) -> Type {
        match self {
            Self::Float(_) => LiteralType::Float.ty(),
            Self::Int(_) => LiteralType::Int.ty(),
            Self::Bool(_) => PrimitiveType::Bool.ty(),
            Self::Char(_) => PrimitiveType::Char.ty(),
            Self::Str(_) => todo!("usage of string literals is not yet supported"),
        }
    }
}

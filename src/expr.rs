//! handle the painful process that is parsing expressions

use crate::cached::CachedString;
use crate::error::{err, unexpected_kind, MyResult};
use crate::late_init::LateInit;
use crate::parse::{Kind, Node};
use crate::pass::{Gen, InitType, Visit};
use crate::scope::{Scope, Symbol};
use crate::span::Span;
use crate::statement::{CCode, FuncCall};
use crate::ty::{LiteralType, PrimitiveType, Type};

#[derive(Debug, Clone)]
pub struct Expr {
    span: Span,
    kind: ExprKind,
    ty: LateInit<Type>,
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
    fn visit_impl(node: Node) -> Self {
        use ExprKind::*;
        let kind = match node.kind() {
            Kind::expr => node.children().next().unwrap().visit().0,
            Kind::equality_expr | Kind::compare_expr | Kind::add_expr | Kind::mul_expr => {
                // left assoc
                let mut nodes = node.children();

                let mut left: Expr = nodes.next().unwrap().visit();
                while let Some(op) = nodes.next() {
                    left.kind = Binary {
                        left: left.into(),
                        op: op.as_str().into(),
                        right: nodes.next().unwrap().visit::<Expr>().into(),
                    }
                }

                left.kind
            }
            Kind::unary_expr => {
                // right assoc
                let mut rev_nodes = node.children().rev();

                let mut thing: Expr = rev_nodes.next().unwrap().visit();
                for op in rev_nodes {
                    thing.kind = Unary {
                        op: op.as_cached_str_with_span(),
                        thing: thing.into(),
                    }
                }

                thing.kind
            }
            Kind::cast_expr => {
                // left assoc
                let mut nodes = node.children();

                let mut thing: Expr = nodes.next().unwrap().visit();
                for ty in nodes {
                    thing.kind = Cast {
                        thing: thing.into(),
                        ty: ty.visit(),
                    }
                }

                thing.kind
            }

            Kind::dot_expr => {
                // left assoc
                let mut nodes = node.children();

                let mut left: Expr = nodes.next().unwrap().visit();
                for right in nodes {
                    left.kind = match right.kind() {
                        Kind::func_call => MethodCall {
                            receiver: left.into(),
                            func_call: right.visit(),
                        },
                        Kind::ident => Field {
                            receiver: left.into(),
                            var: right.as_cached_str_with_span(),
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
            span: node.span(),
            kind,
            ty: Default::default(),
        }
    }
}

impl InitType for Expr {
    fn span(&self) -> Span {
        self.span
    }
    fn ty(&self) -> Type {
        *self.ty
    }

    fn init_type_impl(self) -> MyResult<Self> {
        use ExprKind::*;
        *self.ty = match &self.kind {
            Binary { left, op, right } => Scope::current()
                .get_func(*op, [left.init_type()?.ty(), right.init_type()?.ty()])?
                .ty(),
            Unary { op, thing } => Scope::current()
                .get_func(*op, [thing.init_type()?.ty()])?
                .ty(),
            Cast { thing, ty } => Scope::current()
                .get_func(format!("as {}", *ty).into(), [thing.init_type()?.ty()])?
                .ty(),

            MethodCall {
                receiver,
                func_call,
            } => {
                let receiver_ty = receiver.init_type()?.ty();
                let struct_name = match receiver_ty {
                    Type::Struct(struct_name) => struct_name,
                    ty => return err(format!("expected struct type, but got {}", ty)),
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

                Scope::current().get_func(name, arg_types)?.ty()
            }
            Field { receiver, var } => {
                let struct_name = match receiver.ty() {
                    Type::Struct(struct_name) => struct_name,
                    ty => return err(format!("expected struct type, but got {}", ty)),
                };
                let symbol = Scope::current().get_struct(struct_name).unwrap();
                let field_types = match symbol {
                    Symbol::Struct { field_types, .. } => field_types,
                    _ => return err(format!("expected struct symbol, but got {}", symbol)),
                };
                match field_types.get(&var) {
                    Some(&field_type) => field_type,
                    None => return err(format!("no field named {} in {}", *var, symbol)),
                }
            }

            Literal(literal) => literal.ty(),
            FuncCall(func_call) => func_call.ty(),
            Var(name) => Scope::current().get_var(*name).unwrap().ty(),

            CCode(c_code) => c_code.ty(),
        };

        Ok(self)
    }
}

impl Gen for Expr {
    fn span(&self) -> Span {
        self.span
    }

    fn gen_impl(self) -> MyResult<String> {
        use ExprKind::*;
        Ok(match self.kind {
            Binary { left, op, right } => format!("({} {} {})", left.gen()?, op, right.gen()?),
            Unary { op, thing } => format!("({}{})", op, thing.gen()?),
            Cast { thing, ty } => format!("(({}) {})", ty.gen()?, thing.gen()?),

            MethodCall {
                receiver,
                mut func_call,
            } => {
                let struct_name = match receiver.ty() {
                    Type::Struct(struct_name) => struct_name,
                    ty => return err(format!("expected struct type, but got {}", ty)),
                };
                func_call.name = func_call
                    .name
                    .map(|name| format!("{}.{}", struct_name, name).into());
                func_call.args.insert(0, *receiver);

                func_call.gen()?
            }
            Field { receiver, var } => {
                let s = format!("({}.{})", receiver.clone().gen()?, *var);

                // symbol check
                let struct_name = match receiver.ty() {
                    Type::Struct(struct_name) => struct_name,
                    ty => return err(format!("expected struct type, but got {}", ty)),
                };
                let symbol = Scope::current().get_struct(struct_name).unwrap();
                let field_types = match &symbol {
                    Symbol::Struct { field_types, .. } => field_types,
                    _ => return err(format!("expected struct symbol, but got {}", symbol)),
                };
                if !field_types.contains_key(&var) {
                    return err(format!("no field named {} in {}", *var, symbol));
                }

                s
            }

            Literal(literal) => literal.with(self.1).gen()?,
            FuncCall(func_call) => func_call.with(self.1).gen()?,
            Var(name) => {
                Scope::current().get_var(name)?;
                name.to_string()
            }

            CCode(c_code) => c_code.with(self.1).gen()?,
        })
    }
}

#[derive(Debug, Clone)]
pub struct Literal {
    span: Span,
    kind: LiteralKind,
    ty: LateInit<Type>,
}
#[derive(Debug, Clone)]
pub enum LiteralKind {
    Float(f64),
    Int(i64),
    Bool(bool),
    Char(char),
    Str(String),
}

impl Visit for Literal {
    fn visit_impl(node: Node) -> Self {
        use LiteralKind::*;
        let kind = match node.kind() {
            Kind::float_literal => Float(node.as_str().parse().unwrap()),
            Kind::int_literal => Int(node.as_str().parse().unwrap()),
            Kind::bool_literal => Bool(node.as_str().parse().unwrap()),
            Kind::char_literal => Char(node.children().next().unwrap().as_str().parse().unwrap()),
            Kind::str_literal => Str(node.children().next().unwrap().as_str().into()),

            _ => unexpected_kind(node),
        };

        Self {
            span: node.span(),
            kind,
            ty: Default::default(),
        }
    }
}

impl InitType for Literal {
    fn span(&self) -> Span {
        self.span
    }
    fn ty(&self) -> Type {
        *self.ty
    }

    fn init_type_impl(self) -> MyResult<Self> {
        use LiteralKind::*;
        *self.ty = match self.kind {
            Float(_) => LiteralType::Float.ty(),
            Int(_) => LiteralType::Int.ty(),
            Bool(_) => PrimitiveType::Bool.ty(),
            Char(_) => PrimitiveType::Char.ty(),
            Str(_) => todo!("usage of string literals is not yet supported"),
        };

        Ok(self)
    }
}

impl Gen for Literal {
    fn span(&self) -> Span {
        self.span
    }

    fn gen_impl(self) -> MyResult<String> {
        use LiteralKind::*;
        Ok(match self.kind {
            Float(value) => value.to_string(),
            Int(value) => value.to_string(),
            Bool(value) => (value as u8).to_string(),
            Char(value) => format!("'{}'", value),
            Str(value) => format!("\"{}\"", value),
        })
    }
}

//! turns pest nodes into pre-type-checked ast

use crate::error::unexpected_kind;
use crate::parse::{Kind, Node, Nodes};
use crate::pass::ast1::*;
use crate::pass::{Ident, Literal};
use crate::util::IterExt;

pub trait Visit {
    fn visit(node: Node) -> Self;
}

impl Node {
    pub fn visit<V: Visit>(self) -> V {
        V::visit(self)
    }
}

impl Nodes {
    /// visits any not iterated nodes,
    /// short circuiting if any of them error
    pub fn visit_rest<V: Visit>(self) -> Vec<V> {
        self.map(|it| it.visit()).collect()
    }
}

impl Visit for Ident {
    fn visit(node: Node) -> Self {
        debug_assert_eq!(node.kind(), Kind::ident);
        let str = node.str();
        Self(
            node.span(),
            str.strip_prefix('`')
                .unwrap_or(str)
                .strip_suffix('`')
                .unwrap_or(str),
        )
    }
}

impl Visit for Program {
    fn visit(node: Node) -> Self {
        Self(
            node.children_checked(Kind::program)
                .filter_map(|node| match node.kind() {
                    Kind::EOI => None,
                    _ => Some(node.visit()),
                })
                .vec()
                .into(),
        )
    }
}

impl Visit for Define {
    fn visit(node: Node) -> Self {
        let span = node.span();
        use Define::*;
        match node.kind() {
            Kind::struct_define => {
                let mut nodes = node.children();

                Struct {
                    span,
                    name: nodes.next().unwrap().visit(),
                    generic_placeholders: nodes
                        .next()
                        .unwrap()
                        .children_checked(Kind::generic_placeholders)
                        .map(|it| it.visit())
                        .vec()
                        .into(),
                    body: nodes.visit_rest().into(),
                }
            }
            Kind::func_define => {
                let mut nodes = node.children().peekable();

                let ty = nodes.next().unwrap().visit();
                let receiver_ty = nodes
                    .next()
                    .unwrap()
                    .children_checked(Kind::func_receiver_ty)
                    .next()
                    .map(|it| it.visit());
                let name = nodes.next().unwrap().visit();
                let generic_placeholders = nodes
                    .next()
                    .unwrap()
                    .children_checked(Kind::generic_placeholders)
                    .map(|it| it.visit())
                    .vec()
                    .into();
                let mut args = vec![];
                while nodes.peek().is_some() && nodes.peek().unwrap().kind() == Kind::var_define {
                    args.push(nodes.next().unwrap().visit())
                }
                let body = nodes.next().unwrap().visit();

                Func {
                    span,
                    ty,
                    receiver_ty,
                    name,
                    generic_placeholders,
                    args: args.into(),
                    body,
                }
            }
            Kind::var_define => Var(node.visit()),

            Kind::c_code => CCode(node.visit()),

            _ => unexpected_kind(node),
        }
    }
}

impl Visit for VarDefine {
    fn visit(node: Node) -> Self {
        let span = node.span();
        let mut nodes = node.children_checked(Kind::var_define);

        Self {
            span,
            ty: nodes.next().unwrap().visit(),
            name: nodes.next().unwrap().visit(),
            value: nodes.next().map(|it| it.visit()),
        }
    }
}

impl Visit for Statement {
    fn visit(node: Node) -> Self {
        let span = node.span();
        use Statement::*;
        match node.kind() {
            Kind::ret => Return(node.span(), node.children().next().map(|it| it.visit())),
            Kind::brk => Break(node.span()),
            Kind::cont => Continue(node.span()),
            Kind::iff => {
                let mut nodes = node.children();

                If {
                    span,
                    cond: nodes.next().unwrap().visit(),
                    then: nodes.next().unwrap().visit(),
                    otherwise: nodes.next().map(|it| it.visit()),
                }
            }
            Kind::until => {
                let mut nodes = node.children();

                Until {
                    span,
                    cond: nodes.next().unwrap().visit(),
                    block: nodes.next().unwrap().visit(),
                }
            }
            Kind::forr => {
                let mut nodes = node.children();

                For {
                    span,
                    init: nodes.next().unwrap().visit(),
                    cond: nodes.next().unwrap().visit(),
                    update: nodes.next().unwrap().visit::<Statement>().into(),
                    block: nodes.next().unwrap().visit(),
                }
            }
            Kind::expr_assign => {
                let mut nodes = node.children();

                ExprAssign {
                    span,
                    lvalue: nodes.next().unwrap().visit(),
                    rvalue: nodes.next().unwrap().visit(),
                }
            }

            Kind::struct_define | Kind::func_define | Kind::var_define => Define(node.visit()),
            Kind::expr => Expr(node.visit()),

            _ => unexpected_kind(node),
        }
    }
}

impl Visit for Block {
    fn visit(node: Node) -> Self {
        Self(node.children_checked(Kind::block).visit_rest().into())
    }
}

impl Visit for CCode {
    fn visit(node: Node) -> Self {
        Self(
            node.children_checked(Kind::c_code)
                .into_iter()
                .map(|it| match it.kind() {
                    Kind::c_code_str => CCodePart::String(it.str()),
                    Kind::expr => CCodePart::Expr(it.visit()),

                    _ => unexpected_kind(it),
                })
                .vec()
                .into(),
        )
    }
}

impl Visit for Expr {
    fn visit(node: Node) -> Self {
        let span = node.span();
        use Expr::*;
        match node.kind() {
            Kind::expr => node.children().next().unwrap().visit(),
            Kind::equality_expr | Kind::compare_expr | Kind::add_expr | Kind::mul_expr => {
                // left assoc
                let mut nodes = node.children();

                let mut left = nodes.next().unwrap().visit::<Expr>();
                while let Some(op) = nodes.next() {
                    let old_left = left.clone();
                    let op = Ident(op.span(), op.str());
                    let right = nodes.next().unwrap().visit::<Expr>();
                    left = FuncCall(self::FuncCall {
                        span,
                        receiver_ty: None,
                        name: op,
                        generic_replacements: Default::default(),
                        args: vec![old_left, right].into(),
                    })
                }

                left
            }
            Kind::unary_expr => {
                // right assoc
                let mut rev_nodes = node.children().rev();

                let mut thing = rev_nodes.next().unwrap().visit::<Expr>();
                for op in rev_nodes {
                    let op = Ident(op.span(), op.str());
                    let old_thing = thing.clone();
                    thing = FuncCall(self::FuncCall {
                        span,
                        receiver_ty: None,
                        name: op,
                        generic_replacements: Default::default(),
                        args: vec![old_thing].into(),
                    })
                }

                thing
            }
            Kind::cast_expr => {
                // left assoc
                let mut nodes = node.children();

                let mut thing = nodes.next().unwrap().visit::<Expr>();
                for ty in nodes {
                    thing = Cast {
                        span,
                        thing: thing.clone().into(),
                        ty: ty.visit(),
                    }
                }

                thing
            }

            Kind::dot_expr => {
                // left assoc
                let mut nodes = node.children();

                let mut left = nodes.next().unwrap().visit::<Expr>();
                for right in nodes {
                    let old_left = left.clone();
                    left = match right.kind() {
                        Kind::func_call => MethodCall {
                            span,
                            receiver: old_left.into(),
                            func_call: right.visit(),
                        },
                        Kind::ident => Field {
                            span,
                            receiver: old_left.into(),
                            name: right.visit(),
                        },

                        _ => unexpected_kind(right),
                    }
                }

                left
            }

            // primary
            Kind::literal => Literal(node.children().next().unwrap().visit()),
            Kind::func_call => FuncCall(node.visit()),
            Kind::ident => Var(node.visit()),

            Kind::c_code => CCode(span, node.visit()),

            _ => unexpected_kind(node),
        }
    }
}

impl Visit for FuncCall {
    fn visit(node: Node) -> Self {
        let span = node.span();
        let mut nodes = node.children_checked(Kind::func_call);

        Self {
            span,
            receiver_ty: nodes
                .next()
                .unwrap()
                .children_checked(Kind::func_receiver_ty)
                .next()
                .map(|it| it.visit()),
            name: nodes.next().unwrap().visit(),
            generic_replacements: nodes
                .next()
                .unwrap()
                .children_checked(Kind::generic_replacements)
                .map(|it| it.visit())
                .vec()
                .into(),
            args: nodes.visit_rest().into(),
        }
    }
}

impl Visit for Literal {
    fn visit(node: Node) -> Self {
        use Literal::*;
        match node.kind() {
            Kind::float_literal => Float(node.span(), node.str().parse().unwrap()),
            Kind::int_literal => Int(node.span(), node.str().parse().unwrap()),
            Kind::bool_literal => Bool(node.span(), node.str().parse().unwrap()),
            Kind::char_literal => Char(
                node.span(),
                node.children().next().unwrap().str().parse().unwrap(),
            ),
            Kind::str_literal => StrZ(node.span(), node.children().next().unwrap().str()), // todo maybe unescape this so we can actually count the characters?

            _ => unexpected_kind(node),
        }
    }
}

impl Visit for Type {
    fn visit(node: Node) -> Self {
        let node = node.children_checked(Kind::ty).next().unwrap();
        let span = node.span();
        use Type::*;
        match node.kind() {
            Kind::primitive => Primitive(node.str().parse().unwrap()),
            Kind::ptr => Ptr(node.children().next().unwrap().visit::<Type>().into()),
            Kind::named => {
                let mut nodes = node.children();
                Named {
                    name: nodes.next().unwrap().visit(),
                    generic_replacements: nodes
                        .next()
                        .unwrap()
                        .children_checked(Kind::generic_replacements)
                        .map(|it| it.visit())
                        .vec()
                        .into(),
                }
            }
            Kind::auto => Auto,

            _ => unexpected_kind(node),
        }
    }
}

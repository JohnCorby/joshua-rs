//! turns pest nodes into pre-type-checked ast

use crate::error::unexpected_kind;
use crate::parse::{Kind, Node, Nodes};
use crate::pass::ast1::*;
use crate::pass::ast2::Literal;
use crate::util::IterExt;

pub trait Visit {
    fn visit(node: Node) -> Self;
}

impl Node {
    pub fn visit<V: Visit>(self) -> V {
        V::visit(self)
    }

    fn visit_ident(&self) -> &'static str {
        debug_assert_eq!(self.kind(), Kind::ident);
        let str = self.str();
        str.strip_prefix('`')
            .unwrap_or(str)
            .strip_suffix('`')
            .unwrap_or(str)
    }
}

impl Nodes {
    /// visits any not iterated nodes,
    /// short circuiting if any of them error
    pub fn visit_rest<V: Visit>(self) -> Vec<V> {
        self.map(|node| node.visit()).collect()
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
        use DefineKind::*;
        let kind = match node.kind() {
            Kind::struct_define => {
                let mut nodes = node.children();

                Struct {
                    name: nodes.next().unwrap().visit_ident(),
                    generic_placeholders: nodes
                        .next()
                        .unwrap()
                        .children_checked(Kind::generic_placeholders)
                        .map(|node| node.visit_ident())
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
                    .map(|node| node.visit());
                let name = nodes.next().unwrap().visit_ident();
                let generic_placeholders = nodes
                    .next()
                    .unwrap()
                    .children_checked(Kind::generic_placeholders)
                    .map(|node| node.visit_ident())
                    .vec()
                    .into();
                let mut args = vec![];
                while nodes.peek().is_some() && nodes.peek().unwrap().kind() == Kind::var_define {
                    args.push(nodes.next().unwrap().visit())
                }
                let body = nodes.next().unwrap().visit();

                Func {
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
        };

        Self { span, kind }
    }
}

impl Visit for VarDefine {
    fn visit(node: Node) -> Self {
        let span = node.span();
        let mut nodes = node.children_checked(Kind::var_define);

        Self {
            span,
            ty: nodes.next().unwrap().visit(),
            name: nodes.next().unwrap().visit_ident(),
            value: nodes.next().map(|node| node.visit()),
        }
    }
}

impl Visit for Statement {
    fn visit(node: Node) -> Self {
        let span = node.span();
        use StatementKind::*;
        let kind = match node.kind() {
            Kind::ret => Return(node.children().next().map(|node| node.visit())),
            Kind::brk => Break,
            Kind::cont => Continue,
            Kind::iff => {
                let mut nodes = node.children();

                If {
                    cond: nodes.next().unwrap().visit(),
                    then: nodes.next().unwrap().visit(),
                    otherwise: nodes.next().map(|node| node.visit()),
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

            Kind::struct_define | Kind::func_define | Kind::var_define => Define(node.visit()),
            Kind::expr => Expr(node.visit()),

            _ => unexpected_kind(node),
        };

        Self { span, kind }
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
                .map(|node| match node.kind() {
                    Kind::c_code_str => CCodePart::String(node.str()),
                    Kind::expr => CCodePart::Expr(node.visit()),

                    _ => unexpected_kind(node),
                })
                .vec()
                .into(),
        )
    }
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
                    let old_left = left.clone();
                    let op = op.str();
                    let right = nodes.next().unwrap().visit::<Expr>();
                    left.kind = FuncCall(self::FuncCall {
                        span,
                        receiver_ty: None,
                        name: op,
                        generic_replacements: Default::default(),
                        args: vec![old_left, right].into(),
                    })
                }

                left.kind
            }
            Kind::unary_expr => {
                // right assoc
                let mut rev_nodes = node.children().rev();

                let mut thing = rev_nodes.next().unwrap().visit::<Expr>();
                for op in rev_nodes {
                    let op = op.str();
                    let old_thing = thing.clone();
                    thing.kind = FuncCall(self::FuncCall {
                        span,
                        receiver_ty: None,
                        name: op,
                        generic_replacements: Default::default(),
                        args: vec![old_thing].into(),
                    })
                }

                thing.kind
            }
            Kind::cast_expr => {
                // left assoc
                let mut nodes = node.children();

                let mut thing = nodes.next().unwrap().visit::<Expr>();
                for ty in nodes {
                    thing.kind = Cast {
                        thing: thing.clone().into(),
                        ty: ty.visit(),
                    }
                }

                thing.kind
            }

            Kind::dot_expr => {
                // left assoc
                let mut nodes = node.children();

                let mut left = nodes.next().unwrap().visit::<Expr>();
                for right in nodes {
                    let old_left = left.clone();
                    left.kind = match right.kind() {
                        Kind::func_call => MethodCall {
                            receiver: old_left.into(),
                            func_call: right.visit(),
                        },
                        Kind::ident => Field {
                            receiver: old_left.into(),
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
            Kind::ident => Var(node.visit_ident()),

            Kind::c_code => CCode(node.visit()),

            _ => unexpected_kind(node),
        };

        Self { span, kind }
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
                .map(|node| node.visit()),
            name: nodes.next().unwrap().visit_ident(),
            generic_replacements: nodes
                .next()
                .unwrap()
                .children_checked(Kind::generic_replacements)
                .map(|node| node.visit())
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
            Kind::float_literal => Float(node.str().parse().unwrap()),
            Kind::int_literal => Int(node.str().parse().unwrap()),
            Kind::bool_literal => Bool(node.str().parse().unwrap()),
            Kind::char_literal => Char(node.children().next().unwrap().str().parse().unwrap()),
            Kind::str_literal => StrZ(node.children().next().unwrap().str()), // todo maybe unescape this so we can actually count the characters?

            _ => unexpected_kind(node),
        }
    }
}

impl Visit for Type {
    fn visit(node: Node) -> Self {
        let node = node.children_checked(Kind::ty).next().unwrap();
        let span = node.span();
        use TypeKind::*;
        let ty = match node.kind() {
            Kind::primitive => Primitive(node.str().parse().unwrap()),
            Kind::ptr => Ptr(node.children().next().unwrap().visit::<Type>().into()),
            Kind::named => {
                let mut nodes = node.children();
                Named {
                    name: nodes.next().unwrap().visit_ident(),
                    generic_replacements: nodes
                        .next()
                        .unwrap()
                        .children_checked(Kind::generic_replacements)
                        .map(|node| node.visit())
                        .vec()
                        .into(),
                }
            }
            Kind::auto => Auto,

            _ => unexpected_kind(node),
        };

        Self { span, kind: ty }
    }
}

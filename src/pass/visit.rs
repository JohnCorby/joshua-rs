use crate::context::Ctx;
use crate::error::unexpected_kind;
use crate::parse::{Kind, Node, Nodes};
use crate::pass::ast::*;
use crate::util::interned_str::Intern;

pub trait Visit<'i>: Sized {
    fn visit(node: Node<'i>, ctx: &mut Ctx<'i>) -> Self;
}

impl Node<'i> {
    pub fn visit<V: Visit<'i>>(self, ctx: &mut Ctx<'i>) -> V {
        V::visit(self, ctx)
    }
}

impl Nodes<'i> {
    /// visits any not iterated nodes,
    /// short circuiting if any of them error
    pub fn visit_rest<V: Visit<'i>>(self, ctx: &mut Ctx<'i>) -> Vec<V> {
        self.map(|node| node.visit(ctx)).collect()
    }
}

impl Visit<'i> for Program<'i> {
    fn visit(node: Node<'i>, ctx: &mut Ctx<'i>) -> Self {
        Self(
            node.children_checked(Kind::program)
                .filter_map(|node| match node.kind() {
                    Kind::EOI => None,
                    _ => Some(node.visit(ctx)),
                })
                .collect(),
        )
    }
}

impl Visit<'i> for Define<'i> {
    fn visit(node: Node<'i>, ctx: &mut Ctx<'i>) -> Self {
        let span = node.span();
        use DefineKind::*;
        let kind = match node.kind() {
            Kind::struct_define => {
                let mut nodes = node.children();

                Struct {
                    name: nodes.next().unwrap().visit_ident(ctx),
                    body: nodes.visit_rest(ctx),
                }
            }
            Kind::func_define => {
                let mut nodes = node.children().peekable();

                let ty_node = nodes.next().unwrap().visit(ctx);
                let name = nodes.next().unwrap().visit_ident(ctx);
                let generic_placeholders = nodes
                    .next()
                    .unwrap()
                    .children_checked(Kind::func_define_generics)
                    .map(|node| node.visit_ident(ctx))
                    .collect();
                let mut args = vec![];
                while nodes.peek().is_some() && nodes.peek().unwrap().kind() == Kind::var_define {
                    args.push(nodes.next().unwrap().visit(ctx))
                }
                let body = nodes.next().unwrap().visit(ctx);

                Func {
                    ty_node,
                    name,
                    generic_placeholders,
                    args,
                    body,
                }
            }
            Kind::var_define => Var(node.visit(ctx)),

            Kind::c_code => CCode(node.visit(ctx)),

            _ => unexpected_kind(node),
        };

        Self { span, kind }
    }
}

impl Visit<'i> for VarDefine<'i> {
    fn visit(node: Node<'i>, ctx: &mut Ctx<'i>) -> Self {
        let span = node.span();
        let mut nodes = node.children_checked(Kind::var_define);

        Self {
            span,
            ty_node: nodes.next().unwrap().visit(ctx),
            name: nodes.next().unwrap().visit_ident(ctx),
            value: nodes.next().map(|node| node.visit(ctx)),
        }
    }
}

impl Visit<'i> for Statement<'i> {
    fn visit(node: Node<'i>, ctx: &mut Ctx<'i>) -> Self {
        let span = node.span();
        use StatementKind::*;
        let kind = match node.kind() {
            Kind::ret => Return(node.children().next().map(|node| node.visit(ctx))),
            Kind::brk => Break,
            Kind::cont => Continue,
            Kind::iff => {
                let mut nodes = node.children();

                If {
                    cond: nodes.next().unwrap().visit(ctx),
                    then: nodes.next().unwrap().visit(ctx),
                    otherwise: nodes.next().map(|node| node.visit(ctx)),
                }
            }
            Kind::until => {
                let mut nodes = node.children();

                Until {
                    cond: nodes.next().unwrap().visit(ctx),
                    block: nodes.next().unwrap().visit(ctx),
                }
            }
            Kind::forr => {
                let mut nodes = node.children();

                For {
                    init: nodes.next().unwrap().visit(ctx),
                    cond: nodes.next().unwrap().visit(ctx),
                    update: nodes.next().unwrap().visit::<Statement<'i>>(ctx).into(),
                    block: nodes.next().unwrap().visit(ctx),
                }
            }
            Kind::expr_assign => {
                let mut nodes = node.children();

                ExprAssign {
                    lvalue: nodes.next().unwrap().visit(ctx),
                    rvalue: nodes.next().unwrap().visit(ctx),
                }
            }
            Kind::var_define => VarDefine(node.visit(ctx)),
            Kind::expr => Expr(node.visit(ctx)),

            _ => unexpected_kind(node),
        };

        Self { span, kind }
    }
}

impl Visit<'i> for Block<'i> {
    fn visit(node: Node<'i>, ctx: &mut Ctx<'i>) -> Self {
        Self(node.children_checked(Kind::block).visit_rest(ctx))
    }
}

impl Visit<'i> for CCode<'i> {
    fn visit(node: Node<'i>, ctx: &mut Ctx<'i>) -> Self {
        Self(
            node.children_checked(Kind::c_code)
                .into_iter()
                .map(|node| match node.kind() {
                    Kind::c_code_str => CCodePart::String(node.str()),
                    Kind::expr => CCodePart::Expr(node.visit(ctx)),

                    _ => unexpected_kind(node),
                })
                .collect(),
        )
    }
}

impl Visit<'i> for Expr<'i> {
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
                    let old_left = left.clone();
                    let op = op.str().intern(ctx);
                    let right = nodes.next().unwrap().visit::<Expr<'i>>(ctx);
                    left.kind = FuncCall(self::FuncCall {
                        span,
                        name: op,
                        generic_replacements: vec![],
                        args: vec![old_left, right],
                        ty: Default::default(),
                    })
                }

                left.kind
            }
            Kind::unary_expr => {
                // right assoc
                let mut rev_nodes = node.children().rev();

                let mut thing = rev_nodes.next().unwrap().visit::<Expr<'i>>(ctx);
                for op in rev_nodes {
                    let op = op.str().intern(ctx);
                    let old_thing = thing.clone();
                    thing.kind = FuncCall(self::FuncCall {
                        span,
                        name: op,
                        generic_replacements: vec![],
                        args: vec![old_thing],
                        ty: Default::default(),
                    })
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
                        Kind::func_call => {
                            let receiver = left.clone();
                            let mut func_call = right.visit::<self::FuncCall<'i>>(ctx);
                            func_call.args.insert(0, receiver);
                            FuncCall(func_call)
                        }
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

impl Visit<'i> for FuncCall<'i> {
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

impl Visit<'i> for Literal<'i> {
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

impl Visit<'i> for TypeNode<'i> {
    fn visit(node: Node<'i>, ctx: &mut Ctx<'i>) -> Self {
        let node = node.children_checked(Kind::ty).next().unwrap();
        let span = node.span();
        use TypeKind::*;
        let ty = match node.kind() {
            Kind::primitive => Primitive(node.str().parse().unwrap()),
            Kind::ptr => Ptr(node
                .children()
                .next()
                .unwrap()
                .visit::<TypeNode<'i>>(ctx)
                .into()),
            Kind::ident => Named(node.visit_ident(ctx)),

            _ => unexpected_kind(node),
        };

        Self {
            span,
            kind: ty,
            ty: Default::default(),
        }
    }
}

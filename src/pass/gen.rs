use crate::context::Ctx;
use crate::pass::ast::*;
use crate::pass::ty::Type;
use crate::util::Mangle;
use std::ops::Deref;

impl Program<'i> {
    pub fn gen(self, ctx: &mut Ctx<'i>) {
        for define in self.0 {
            define.gen(ctx);
            ctx.o.push('\n')
        }
        if ctx.o.ends_with('\n') {
            ctx.o.pop();
        }
    }
}

impl Define<'i> {
    pub fn gen(self, ctx: &mut Ctx<'i>) {
        use crate::pass::ast::DefineKind::*;
        match self.kind {
            Struct { name, body } => {
                let old_o = std::mem::take(&mut ctx.o);

                ctx.o.push_str("struct ");
                ctx.o.push_str(&name.mangle());

                ctx.struct_protos.push_str(&ctx.o);
                ctx.o.insert_str(0, &old_o);
                ctx.struct_protos.push_str(";\n");

                ctx.o.push_str(" {\n");
                for define in body {
                    define.gen(ctx);
                    ctx.o.push('\n')
                }
                ctx.o.push_str("};");
            }
            Func {
                ty_node,
                name,
                generic_placeholders,
                args,
                body,
            } => {
                // only gen non-generic func
                if generic_placeholders.is_empty() {
                    let old_o = std::mem::take(&mut ctx.o);

                    ty_node.gen(ctx);
                    ctx.o.push(' ');
                    ctx.o.push_str(
                        &name.mangle_func(
                            &args
                                .iter()
                                .map(|it| it.ty_node.ty.deref())
                                .collect::<Vec<_>>(),
                            &[],
                        ),
                    );
                    ctx.o.push('(');
                    for arg in args {
                        arg.gen(ctx);
                        ctx.o.push_str(", ")
                    }
                    if ctx.o.ends_with(", ") {
                        ctx.o.pop();
                        ctx.o.pop();
                    }
                    ctx.o.push(')');

                    ctx.func_protos.push_str(&ctx.o);
                    ctx.o.insert_str(0, &old_o);
                    ctx.func_protos.push_str(";\n");

                    ctx.o.push(' ');
                    body.gen(ctx);
                }
            }
            Var(var_define) => {
                var_define.gen(ctx);
                ctx.o.push(';')
            }

            CCode(c_code) => c_code.gen(ctx),
        }
    }
}

impl VarDefine<'i> {
    pub fn gen(self, ctx: &mut Ctx<'i>) {
        self.ty_node.gen(ctx);
        ctx.o.push(' ');
        ctx.o.push_str(&self.name.mangle());
        if let Some(value) = self.value {
            ctx.o.push_str(" = ");
            value.gen(ctx)
        }
    }
}

impl Statement<'i> {
    pub fn gen(self, ctx: &mut Ctx<'i>) {
        use crate::pass::ast::StatementKind::*;
        match self.kind {
            Return(value) => {
                ctx.o.push_str("return");
                if let Some(value) = value {
                    ctx.o.push(' ');
                    value.gen(ctx)
                }
                ctx.o.push(';');
            }
            Break => ctx.o.push_str("break;"),
            Continue => ctx.o.push_str("continue;"),
            If {
                cond,
                then,
                otherwise,
            } => {
                ctx.o.push_str("if (");
                cond.gen(ctx);
                ctx.o.push_str(") ");
                then.gen(ctx);
                if let Some(otherwise) = otherwise {
                    otherwise.gen(ctx);
                }
            }
            Until { cond, block } => {
                ctx.o.push_str("while (!(");
                cond.gen(ctx);
                ctx.o.push_str(")) ");
                block.gen(ctx);
            }
            For {
                init,
                cond,
                update,
                block,
            } => {
                ctx.o.push_str("for (");
                init.gen(ctx);
                ctx.o.push_str("; ");

                cond.gen(ctx);
                ctx.o.push_str("; ");
                update.gen(ctx);
                ctx.o.pop(); // for update statement's semicolon
                ctx.o.push_str(") ");
                block.gen(ctx);
            }
            ExprAssign { lvalue, rvalue } => {
                lvalue.gen(ctx);
                ctx.o.push_str(" = ");
                rvalue.gen(ctx);
                ctx.o.push(';');
            }
            VarDefine(var_define) => {
                var_define.gen(ctx);
                ctx.o.push(';');
            }
            Expr(expr) => {
                expr.gen(ctx);
                ctx.o.push(';');
            }
        }
    }
}

impl Block<'i> {
    pub fn gen(self, ctx: &mut Ctx<'i>) {
        ctx.o.push_str("{\n");
        for statement in self.0 {
            statement.gen(ctx);
            ctx.o.push('\n')
        }
        ctx.o.push('}');
    }
}

impl CCode<'i> {
    pub fn gen(self, ctx: &mut Ctx<'i>) {
        ctx.o.push_str("/*<{*/");
        for part in self.0 {
            match part {
                CCodePart::String(str) => ctx.o.push_str(str),
                CCodePart::Expr(expr) => expr.gen(ctx),
            }
        }
        ctx.o.push_str("/*}>*/");
    }
}

impl Expr<'i> {
    pub fn gen(self, ctx: &mut Ctx<'i>) {
        use crate::pass::ast;
        use crate::pass::ast::ExprKind::*;
        use crate::util::interned_str::Intern;
        match self.kind {
            Binary { left, op, right } => {
                ast::FuncCall {
                    span: self.span,
                    name: op,
                    generic_replacements: vec![],
                    args: vec![*left, *right],
                    ty: Default::default(),
                }
                .gen(ctx);
            }
            Unary { op, thing } => {
                ast::FuncCall {
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
                    ast::FuncCall {
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

impl FuncCall<'i> {
    pub fn gen(self, ctx: &mut Ctx<'i>) {
        ctx.o.push_str(
            &self.name.mangle_func(
                &self.args.iter().map(|it| it.ty.deref()).collect::<Vec<_>>(),
                &self
                    .generic_replacements
                    .iter()
                    .map(|it| it.ty.deref())
                    .collect::<Vec<_>>(),
            ),
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

impl Literal<'i> {
    pub fn gen(self, ctx: &mut Ctx<'i>) {
        use crate::pass::ast::Literal::*;
        ctx.o.push_str(&match self {
            Float(value) => value.to_string(),
            Int(value) => value.to_string(),
            Bool(value) => (value as u8).to_string(),
            Char(value) => format!("'{}'", value),
            Str(value) => format!("\"{}\"", value),
        })
    }
}

impl TypeNode<'i> {
    pub fn gen(self, ctx: &mut Ctx<'i>) {
        use crate::pass::ty::Type::*;
        match self.ty.deref() {
            Primitive(ty) => ctx.o.push_str(ty.c_type()),
            Struct(name) => {
                ctx.o.push_str("struct ");
                ctx.o.push_str(&name.mangle())
            }
            ty => panic!("tried to gen {}", ty),
        }
    }
}

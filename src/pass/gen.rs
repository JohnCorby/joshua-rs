use crate::context::Ctx;
use crate::pass::ast::*;
use crate::pass::ty::Type;
use crate::util::ctx_str::IntoCtx;
use crate::util::Mangle;
use std::ops::Deref;
use std::rc::Rc;

pub trait Gen<'i> {
    /// take fully initialized self and generate it into one of the ctx output sections
    fn gen(self, ctx: &mut Ctx<'i>);
}

impl Gen<'i> for Program<'i> {
    fn gen(self, ctx: &mut Ctx<'i>) {
        for define in Rc::try_unwrap(self.0).unwrap() {
            define.gen(ctx);
        }
    }
}

impl Gen<'i> for Define<'i> {
    fn gen(self, ctx: &mut Ctx<'i>) {
        use DefineKind::*;
        match self.kind {
            Struct {
                name,
                generic_placeholders,
                body,
            } => {
                // only gen non-generic func
                if generic_placeholders.is_empty() {
                    let structs = std::mem::take(&mut ctx.structs);
                    let old_o = std::mem::replace(&mut ctx.o, structs);

                    ctx.o.push_str("struct ");
                    ctx.o.push_str(&name.mangle());
                    ctx.o.push_str(" {\n");
                    for define in Rc::try_unwrap(body).unwrap() {
                        define.gen(ctx);
                    }
                    ctx.o.push_str("};\n");

                    let new_o = std::mem::replace(&mut ctx.o, old_o);
                    ctx.structs = new_o;
                } else {
                    todo!("generic struct define gen")
                }
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
                    ctx.o.push_str(&name.mangle_func(
                        &args.iter().map(|it| &*it.ty_node.ty).collect::<Vec<_>>(),
                        &[],
                    ));
                    ctx.o.push('(');
                    for arg in Rc::try_unwrap(args).unwrap() {
                        arg.gen(ctx);
                        ctx.o.push_str(", ")
                    }
                    if ctx.o.ends_with(", ") {
                        ctx.o.pop();
                        ctx.o.pop();
                    }
                    ctx.o.push(')');

                    ctx.func_declares.push_str(&ctx.o);
                    ctx.func_declares.push_str(";\n");

                    ctx.o.push(' ');
                    body.gen(ctx);

                    let new_o = std::mem::replace(&mut ctx.o, old_o);
                    ctx.func_defines.push_str(&new_o);
                }
            }
            Var(var_define) => {
                // fixme global variables stick around in ctx.o when they should go somewhere else, this will panic
                var_define.gen(ctx);
                ctx.o.push_str(";\n")
            }

            CCode(c_code) => {
                c_code.gen(ctx);
                ctx.o.push_str(";\n");
            }
        }
    }
}

impl Gen<'i> for VarDefine<'i> {
    fn gen(self, ctx: &mut Ctx<'i>) {
        self.ty_node.gen(ctx);
        ctx.o.push(' ');
        ctx.o.push_str(&self.name.mangle());
        if let Some(value) = self.value {
            ctx.o.push_str(" = ");
            value.gen(ctx)
        }
    }
}

impl Gen<'i> for Statement<'i> {
    fn gen(self, ctx: &mut Ctx<'i>) {
        use StatementKind::*;
        match self.kind {
            Return(value) => {
                ctx.o.push_str("return");
                if let Some(value) = value {
                    ctx.o.push(' ');
                    value.gen(ctx)
                }
                ctx.o.push_str(";\n");
            }
            Break => ctx.o.push_str("break;\n"),
            Continue => ctx.o.push_str("continue;\n"),
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
                Rc::try_unwrap(update).unwrap().gen(ctx);
                ctx.o.pop(); // for update statement's semicolon
                ctx.o.pop(); // for update statement's newline
                ctx.o.push_str(") ");
                block.gen(ctx);
            }
            ExprAssign { lvalue, rvalue } => {
                lvalue.gen(ctx);
                ctx.o.push_str(" = ");
                rvalue.gen(ctx);
                ctx.o.push_str(";\n");
            }
            VarDefine(var_define) => {
                var_define.gen(ctx);
                ctx.o.push_str(";\n");
            }
            Expr(expr) => {
                expr.gen(ctx);
                ctx.o.push_str(";\n");
            }
        }
    }
}

impl Gen<'i> for Block<'i> {
    fn gen(self, ctx: &mut Ctx<'i>) {
        ctx.o.push_str("{\n");
        for statement in Rc::try_unwrap(self.0).unwrap() {
            statement.gen(ctx);
        }
        ctx.o.push_str("}\n");
    }
}

impl Gen<'i> for CCode<'i> {
    fn gen(self, ctx: &mut Ctx<'i>) {
        ctx.o.push_str("/*<{*/");
        for part in Rc::try_unwrap(self.0).unwrap() {
            match part {
                CCodePart::String(str) => ctx.o.push_str(&str),
                CCodePart::Expr(expr) => expr.gen(ctx),
            }
        }
        ctx.o.push_str("/*}>*/");
    }
}

impl Gen<'i> for Expr<'i> {
    fn gen(self, ctx: &mut Ctx<'i>) {
        use ExprKind::*;
        match self.kind {
            Cast { thing, ty_node } => {
                // fixme hacky as shit
                if matches!(*thing.ty, Type::Literal(_) | Type::CCode) {
                    ctx.o.push('(');
                    ty_node.gen(ctx);
                    ctx.o.push_str(") ");
                    Rc::try_unwrap(thing).unwrap().gen(ctx);
                } else {
                    self::FuncCall {
                        span: self.span,
                        name: format!("as {}", ty_node.ty.code_name()).into_ctx(ctx),
                        generic_replacements: Default::default(),
                        args: vec![Rc::try_unwrap(thing).unwrap()].into(),
                        ty: Default::default(),
                    }
                    .gen(ctx);
                }
            }

            Field { receiver, var } => {
                Rc::try_unwrap(receiver).unwrap().gen(ctx);
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

impl Gen<'i> for FuncCall<'i> {
    fn gen(self, ctx: &mut Ctx<'i>) {
        ctx.o.push_str(
            &self.name.mangle_func(
                &self.args.iter().map(|it| &*it.ty).collect::<Vec<_>>(),
                &self
                    .generic_replacements
                    .iter()
                    .map(|it| &*it.ty)
                    .collect::<Vec<_>>(),
            ),
        );
        ctx.o.push('(');
        for arg in Rc::try_unwrap(self.args).unwrap() {
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

impl Gen<'i> for Literal<'i> {
    fn gen(self, ctx: &mut Ctx<'i>) {
        use Literal::*;
        ctx.o.push_str(&match self {
            Float(value) => value.to_string(),
            Int(value) => value.to_string(),
            Bool(value) => (value as u8).to_string(),
            Char(value) => format!("'{}'", value),
            StrZ(value) => format!("\"{}\"", value), // fixme maybe we dont want these being a ptr to a global and zero terminated? oh well
        })
    }
}

impl Gen<'i> for TypeNode<'i> {
    fn gen(self, ctx: &mut Ctx<'i>) {
        use Type::*;
        if let TypeKind::Ptr(inner) = self.kind {
            inner.deref().clone().gen(ctx);
            ctx.o.push('*');
            return;
        }
        match &*self.ty {
            Primitive(ty) => ctx.o.push_str(ty.c_type()),
            Struct(_) => {
                ctx.o.push_str("struct ");
                ctx.o.push_str(&self.ty.code_name().mangle())
            }
            ty => panic!("tried to gen {}", ty),
        }
    }
}

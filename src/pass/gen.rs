use crate::context::Ctx;
use crate::pass::ast::*;
use crate::pass::ty::Type;
use crate::util::ctx_str::IntoCtx;
use crate::util::{IterExt, RcExt, StrExt};
use std::ops::Deref;

pub trait Gen<'i> {
    /// take fully initialized self and generate it into one of the ctx output sections
    fn gen(self, ctx: &mut Ctx<'i>);
}

impl Gen<'i> for Program<'i> {
    fn gen(self, ctx: &mut Ctx<'i>) {
        for define in self.0.unwrap() {
            define.gen(ctx);
        }
    }
}

impl Gen<'i> for Define<'i> {
    fn gen(self, ctx: &mut Ctx<'i>) {
        use DefineKind::*;
        match self.kind {
            Struct {
                nesting_prefix,
                name,
                generic_placeholders,
                body,
            } => {
                // only gen non-generic func
                if generic_placeholders.is_empty() {
                    let old_o = std::mem::take(&mut ctx.o);

                    ctx.o.push_str("struct ");
                    ctx.o.push_str(&name.mangle(&nesting_prefix, &[], None));

                    ctx.struct_declares.push_str(&ctx.o);
                    ctx.struct_declares.push_str(";\n");

                    ctx.o.push_str(" {\n");
                    for define in body.unwrap() {
                        define.gen(ctx);
                    }
                    ctx.o.push_str("};\n");

                    let new_o = std::mem::replace(&mut ctx.o, old_o);
                    ctx.struct_defines.push_str(&new_o);
                } else {
                    todo!("generic struct define gen")
                }
            }
            Func {
                ty_node,
                nesting_prefix,
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
                    ctx.o.push_str(&name.mangle(
                        &nesting_prefix,
                        &[],
                        Some(&args.iter().map(|it| &*it.ty_node.ty).vec()),
                    ));
                    ctx.o.push('(');
                    for arg in args.unwrap() {
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
        ctx.o.push_str(&self.name.mangle(&[], &[], None));
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
                update.unwrap().gen(ctx);
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
            Define(define) => define.gen(ctx),
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
        for statement in self.0.unwrap() {
            statement.gen(ctx);
        }
        ctx.o.push_str("}\n");
    }
}

impl Gen<'i> for CCode<'i> {
    fn gen(self, ctx: &mut Ctx<'i>) {
        ctx.o.push_str("/*<{*/");
        for part in self.0.unwrap() {
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
            Cast {
                nesting_prefix,
                thing,
                ty_node,
            } => {
                // fixme hacky as shit
                if let Type::Literal(_) | Type::CCode = *thing.ty {
                    ctx.o.push('(');
                    ty_node.gen(ctx);
                    ctx.o.push_str(") ");
                    thing.unwrap().gen(ctx);
                } else {
                    self::FuncCall {
                        span: self.span,
                        nesting_prefix,
                        name: format!("as {}", ty_node.ty.encoded_name()).into_ctx(ctx),
                        generic_replacements: Default::default(),
                        args: vec![thing.unwrap()].into(),
                        ty: Default::default(),
                    }
                    .gen(ctx);
                }
            }

            MethodCall {
                receiver,
                func_call,
            } => {
                // let nesting_prefix = func_call.nesting_prefix.deref().deref().clone();
                // todo add prefix of structs and ptr's and darn this will suck
                // self::FuncCall {
                //     span: self.span,
                //     nesting_prefix: LateInit::from(vec![receiver.ty.code_name()].into()),
                //     name: format!("as {}", ty_node.ty.code_name()).into_ctx(ctx),
                //     generic_replacements: Default::default(),
                //     args: vec![thing.unwrap()].into(),
                //     ty: Default::default(),
                // }.gen();
                todo!("gen method call")
            }
            Field { receiver, var } => {
                let receiver_ty = receiver.ty.deref().clone();
                receiver.unwrap().gen(ctx);
                if let Type::Ptr(_) = receiver_ty {
                    ctx.o.push_str("->") // fixme this does a deref... do we want that? maybe put a & to make it a pointer again
                } else {
                    ctx.o.push('.')
                }
                ctx.o.push_str(&var.mangle(&[], &[], None));
            }

            Literal(literal) => literal.gen(ctx),
            FuncCall(func_call) => func_call.gen(ctx),
            Var(name) => ctx.o.push_str(&name.mangle(&[], &[], None)),

            CCode(c_code) => c_code.gen(ctx),
        }
    }
}

impl Gen<'i> for FuncCall<'i> {
    fn gen(self, ctx: &mut Ctx<'i>) {
        ctx.o.push_str(&self.name.mangle(
            &self.nesting_prefix,
            &self.generic_replacements.iter().map(|it| &*it.ty).vec(),
            Some(&self.args.iter().map(|it| &*it.ty).vec()),
        ));
        ctx.o.push('(');
        for arg in self.args.unwrap() {
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
            Struct { .. } => {
                ctx.o.push_str("struct ");
                ctx.o
                    .push_str(&self.ty.encoded_name().mangle(&[], &[], None))
            }
            ty => panic!("tried to gen {}", ty),
        }
    }
}

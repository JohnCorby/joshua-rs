//! take fully initialized self and generate it into one of the ctx output sections

use crate::context::Ctx;
use crate::pass::ast2::*;
use crate::util::ctx_str::IntoCtx;
use crate::util::{IterExt, RcExt, StrExt};
use std::ops::Deref;

impl Program<'i> {
    pub fn gen(self, ctx: &mut Ctx<'i>) {
        for define in self.0.into_inner() {
            define.gen(ctx);
        }
        debug_assert!(ctx.o.is_empty());
        ctx.o.clear();
        ctx.o.push_str("#pragma region struct declares\n");
        ctx.o.push_str(&ctx.struct_declares);
        ctx.o.push_str("#pragma endregion struct declares\n");
        ctx.o.push_str("#pragma region func declares\n");
        ctx.o.push_str(&ctx.func_declares);
        ctx.o.push_str("#pragma endregion func declares\n");
        ctx.o.push_str("#pragma region global vars\n");
        ctx.o.push_str(&ctx.global_vars);
        ctx.o.push_str("#pragma endregion global vars\n");
        ctx.o.push_str("#pragma region struct defines\n");
        ctx.o.push_str(&ctx.struct_defines);
        ctx.o.push_str("#pragma endregion struct defines\n");
        ctx.o.push_str("#pragma region func defines\n");
        ctx.o.push_str(&ctx.func_defines);
        ctx.o.push_str("#pragma endregion func defines");
    }
}

impl Define<'i> {
    pub fn gen(self, ctx: &mut Ctx<'i>) {
        use Define::*;
        match self {
            Struct {
                full_name,
                generic_replacements,
                body,
            } => {
                let old_o = std::mem::take(&mut ctx.o);

                ctx.o.push_str("struct ");
                ctx.o
                    .push_str(&full_name.mangle(&generic_replacements.iter().vec(), None));

                ctx.struct_declares.push_str(&ctx.o);
                ctx.struct_declares.push_str(";\n");

                ctx.o.push_str(" {\n");
                for define in body.into_inner() {
                    define.gen(ctx);
                }
                ctx.o.push_str("};\n");

                let new_o = std::mem::replace(&mut ctx.o, old_o);
                ctx.struct_defines.push_str(&new_o);
            }

            Func {
                ty,
                full_name,
                generic_replacements,
                args,
                body,
            } => {
                let old_o = std::mem::take(&mut ctx.o);

                ty.gen(ctx);
                ctx.o.push(' ');
                ctx.o.push_str(&full_name.mangle(
                    &generic_replacements.iter().vec(),
                    Some(&args.iter().map(|it| &it.ty).vec()),
                ));
                ctx.o.push('(');
                for arg in args.into_inner() {
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

            Var(var_define) => {
                // fixme global variables stick around in ctx.o when they should go somewhere else, this will panic
                var_define.gen(ctx);
                ctx.o.push_str(";\n")
            }

            CCode(c_code) => {
                c_code.gen(ctx);
                ctx.o.push_str(";\n");
            }

            NoGen => {}
        }
    }
}

impl VarDefine<'i> {
    pub fn gen(self, ctx: &mut Ctx<'i>) {
        self.ty.gen(ctx);
        ctx.o.push(' ');
        ctx.o.push_str(&self.name.mangle(&[], None));
        if let Some(value) = self.value {
            ctx.o.push_str(" = ");
            value.gen(ctx)
        }
    }
}

impl Statement<'i> {
    pub fn gen(self, ctx: &mut Ctx<'i>) {
        use Statement::*;
        match self {
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
                update.into_inner().gen(ctx);
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

impl Block<'i> {
    pub fn gen(self, ctx: &mut Ctx<'i>) {
        ctx.o.push_str("{\n");
        for statement in self.0.into_inner() {
            statement.gen(ctx);
        }
        ctx.o.push_str("}\n");
    }
}

impl CCode<'i> {
    pub fn gen(self, ctx: &mut Ctx<'i>) {
        ctx.o.push_str("/*<{*/");
        for part in self.0.into_inner() {
            match part {
                CCodePart::String(str) => ctx.o.push_str(&str),
                CCodePart::Expr(expr) => expr.gen(ctx),
            }
        }
        ctx.o.push_str("/*}>*/");
    }
}

impl Expr<'i> {
    pub fn gen(self, ctx: &mut Ctx<'i>) {
        use ExprKind::*;
        match self.kind {
            Cast {
                nesting_prefix,
                thing,
            } => {
                // fixme hacky as shit
                if let Type::Literal(_) | Type::CCode = thing.ty {
                    ctx.o.push('(');
                    self.ty.gen(ctx);
                    ctx.o.push_str(") ");
                    thing.into_inner().gen(ctx);
                } else {
                    self::Expr {
                        kind: self::ExprKind::FuncCall {
                            full_name: format!("{}as {}", nesting_prefix, self.ty.encoded_name())
                                .into_ctx(ctx),
                            generic_replacements: Default::default(),
                            args: vec![thing.into_inner()].into(),
                        },
                        ty: Default::default(),
                    }
                    .gen(ctx);
                }
            }

            Field { receiver, var } => {
                let receiver = receiver.into_inner();
                let receiver_ty = receiver.ty.clone();
                receiver.gen(ctx);
                if let Type::Ptr(_) = receiver_ty {
                    ctx.o.push_str("->") // fixme this does a deref... do we want that? maybe put a & to make it a pointer again
                } else {
                    ctx.o.push('.')
                }
                ctx.o.push_str(&var.mangle(&[], None));
            }

            Literal(literal) => literal.gen(ctx),
            FuncCall {
                full_name,
                generic_replacements,
                args,
            } => {
                ctx.o.push_str(&full_name.mangle(
                    &generic_replacements.iter().vec(),
                    Some(&args.iter().map(|it| &it.ty).vec()),
                ));
                ctx.o.push('(');
                for arg in args.into_inner() {
                    arg.gen(ctx);
                    ctx.o.push_str(", ")
                }
                if ctx.o.ends_with(", ") {
                    ctx.o.pop();
                    ctx.o.pop();
                }
                ctx.o.push(')');
            }
            Var(name) => ctx.o.push_str(&name.mangle(&[], None)),

            CCode(c_code) => c_code.gen(ctx),
        }
    }
}

impl Literal<'i> {
    pub fn gen(self, ctx: &mut Ctx<'i>) {
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

impl Type<'i> {
    pub fn gen(self, ctx: &mut Ctx<'i>) {
        use Type::*;
        match self {
            Primitive(ty) => ctx.o.push_str(ty.c_type()),
            Struct { .. } => {
                ctx.o.push_str("struct ");
                ctx.o.push_str(&self.encoded_name().mangle(&[], None))
            }
            Ptr(inner) => {
                inner.deref().clone().gen(ctx);
                ctx.o.push('*');
            }
            ty => panic!("tried to gen {}", ty),
        }
    }
}

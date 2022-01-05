//! take fully initialized self and generate it into one of the o output sections

use crate::context::{Intern, Output};
use crate::pass::ast2::*;
use crate::util::{IterExt, RcExt, StrExt};
use std::ops::Deref;

impl Program {
    pub fn gen(self, mut o: Output) -> String {
        for define in self.0.into_inner() {
            define.gen(&mut o);
        }
        debug_assert!(o.o.is_empty());
        // combine all the buffers
        o.o.clear();

        o.o.push_str("#pragma region prelude\n");
        o.o.push_str(&o.prelude);
        o.o.push_str("#pragma endregion prelude\n");

        o.o.push_str("#pragma region struct declares\n");
        o.o.push_str(&o.struct_declares);
        o.o.push_str("#pragma endregion struct declares\n");
        o.o.push_str("#pragma region func declares\n");
        o.o.push_str(&o.func_declares);
        o.o.push_str("#pragma endregion func declares\n");
        o.o.push_str("#pragma region global vars\n");
        o.o.push_str(&o.global_vars);
        o.o.push_str("#pragma endregion global vars\n");
        o.o.push_str("#pragma region struct defines\n");
        o.o.push_str(&o.struct_defines);
        o.o.push_str("#pragma endregion struct defines\n");
        o.o.push_str("#pragma region func defines\n");
        o.o.push_str(&o.func_defines);
        o.o.push_str("#pragma endregion func defines");
        o.o
    }
}

impl Define {
    pub fn gen(self, o: &mut Output) {
        use DefineKind::*;
        match self.kind {
            Struct {
                nesting_prefix,
                name,
                generic_replacements,
                body,
            } => {
                let old_o = std::mem::take(&mut o.o);

                o.o.push_str("struct ");
                o.o.push_str(
                    &name
                        .encode(nesting_prefix, None, &generic_replacements, None, true)
                        .mangle(),
                );

                o.struct_declares.push_str(&o.o);
                o.struct_declares.push_str(";\n");

                o.o.push_str(" {\n");
                for define in body.into_inner() {
                    define.gen(o);
                }
                o.o.push_str("};\n");

                let new_o = std::mem::replace(&mut o.o, old_o);
                o.struct_defines.push_str(&new_o);
            }

            Func {
                ty,
                nesting_prefix,
                receiver_ty,
                name,
                generic_replacements,
                args,
                body,
            } => {
                let old_o = std::mem::take(&mut o.o);

                ty.gen(o);
                o.o.push(' ');
                // special case for entry point
                if nesting_prefix.is_empty()
                    && receiver_ty.is_none()
                    && name == "main"
                    && generic_replacements.is_empty()
                    && args.is_empty()
                {
                    o.o.push_str("main")
                } else {
                    o.o.push_str(
                        &name
                            .encode(
                                nesting_prefix,
                                receiver_ty.as_ref(),
                                &generic_replacements,
                                Some(&args.iter().cloned().map(|it| it.ty).vec()),
                                true,
                            )
                            .mangle(),
                    )
                }
                o.o.push('(');
                for arg in args.into_inner() {
                    arg.gen(o);
                    o.o.push_str(", ")
                }
                if o.o.ends_with(", ") {
                    o.o.pop();
                    o.o.pop();
                }
                o.o.push(')');

                o.func_declares.push_str(&o.o);
                o.func_declares.push_str(";\n");

                o.o.push(' ');
                body.gen(o);

                let new_o = std::mem::replace(&mut o.o, old_o);
                o.func_defines.push_str(&new_o);
            }

            Var(var_define) => {
                if var_define.is_global {
                    // put global vars in the proper section
                    let old_o = std::mem::take(&mut o.o);

                    var_define.gen(o);
                    o.o.push_str(";\n");

                    let new_o = std::mem::replace(&mut o.o, old_o);
                    o.global_vars.push_str(&new_o);
                } else {
                    var_define.gen(o);
                    o.o.push_str(";\n");
                }
            }

            CCode(c_code) => {
                c_code.gen(o);
                o.o.push_str(";\n");
            }

            NoGen => {}
        }
    }
}

impl VarDefine {
    pub fn gen(self, o: &mut Output) {
        self.ty.gen(o);
        o.o.push(' ');
        o.o.push_str(&self.name.mangle());
        if let Some(value) = self.value {
            o.o.push_str(" = ");
            value.gen(o)
        }
    }
}

impl Statement {
    pub fn gen(self, o: &mut Output) {
        use StatementKind::*;
        match self.kind {
            Return(value) => {
                o.o.push_str("return");
                if let Some(value) = value {
                    o.o.push(' ');
                    value.gen(o)
                }
                o.o.push_str(";\n");
            }
            Break => o.o.push_str("break;\n"),
            Continue => o.o.push_str("continue;\n"),
            If {
                cond,
                then,
                otherwise,
            } => {
                o.o.push_str("if (");
                cond.gen(o);
                o.o.push_str(") ");
                then.gen(o);
                if let Some(otherwise) = otherwise {
                    otherwise.gen(o);
                }
            }
            Until { cond, block } => {
                o.o.push_str("while (!(");
                cond.gen(o);
                o.o.push_str(")) ");
                block.gen(o);
            }
            For {
                init,
                cond,
                update,
                block,
            } => {
                o.o.push_str("for (");
                init.gen(o);
                o.o.push_str("; ");

                cond.gen(o);
                o.o.push_str("; ");
                update.into_inner().gen(o);
                o.o.pop(); // for update statement's semicolon
                o.o.pop(); // for update statement's newline
                o.o.push_str(") ");
                block.gen(o);
            }
            ExprAssign { lvalue, rvalue } => {
                lvalue.gen(o);
                o.o.push_str(" = ");
                rvalue.gen(o);
                o.o.push_str(";\n");
            }
            Define(define) => define.gen(o),
            Expr(expr) => {
                expr.gen(o);
                o.o.push_str(";\n");
            }
        }
    }
}

impl Block {
    pub fn gen(self, o: &mut Output) {
        o.o.push_str("{\n");
        for statement in self.0.into_inner() {
            statement.gen(o);
        }
        o.o.push_str("}\n");
    }
}

impl CCode {
    pub fn gen(self, o: &mut Output) {
        o.o.push_str("/*<{*/");
        for part in self.0.into_inner() {
            match part {
                CCodePart::String(str) => o.o.push_str(str),
                CCodePart::Expr(expr) => expr.gen(o),
            }
        }
        o.o.push_str("/*}>*/");
    }
}

impl Expr {
    pub fn gen(self, o: &mut Output) {
        use ExprKind::*;
        match self.kind {
            Cast {
                nesting_prefix,
                thing,
            } => {
                let thing = thing.into_inner();
                // fixme hacky as shit
                if matches!(thing.ty.kind, TypeKind::Literal(_) | TypeKind::CCode) {
                    o.o.push('(');
                    self.ty.gen(o);
                    o.o.push_str(") ");
                    thing.gen(o);
                } else {
                    self::Expr {
                        span: self.span,
                        kind: self::ExprKind::FuncCall {
                            nesting_prefix,
                            receiver_ty: None,
                            name: format!("as {}", self.ty.encode(true)).intern(),
                            generic_replacements: Default::default(),
                            args: vec![thing].into(),
                        },
                        ty: thing.ty,
                    }
                    .gen(o);
                }
            }

            Field { receiver, var } => {
                let receiver = receiver.into_inner();
                let receiver_ty = receiver.ty.clone();
                receiver.gen(o);
                if matches!(receiver_ty.kind, TypeKind::Ptr(_)) {
                    o.o.push_str("->") // fixme this does a deref... do we want that? maybe put a & to make it a pointer again
                } else {
                    o.o.push('.')
                }
                o.o.push_str(&var.mangle());
            }

            Literal(literal) => literal.gen(o),
            FuncCall {
                nesting_prefix,
                receiver_ty,
                name,
                generic_replacements,
                args,
            } => {
                // special case for entry point
                if nesting_prefix.is_empty()
                    && receiver_ty.is_none()
                    && name == "main"
                    && generic_replacements.is_empty()
                    && args.is_empty()
                {
                    o.o.push_str("main")
                } else {
                    o.o.push_str(
                        &name
                            .encode(
                                nesting_prefix,
                                receiver_ty.as_ref(),
                                &generic_replacements,
                                Some(&args.iter().cloned().map(|it| it.ty).vec()),
                                true,
                            )
                            .mangle(),
                    )
                }
                o.o.push('(');
                for arg in args.into_inner() {
                    arg.gen(o);
                    o.o.push_str(", ")
                }
                if o.o.ends_with(", ") {
                    o.o.pop();
                    o.o.pop();
                }
                o.o.push(')');
            }
            Var(name) => o.o.push_str(&name.mangle()),

            CCode(c_code) => c_code.gen(o),
        }
    }
}

impl Literal {
    pub fn gen(self, o: &mut Output) {
        use Literal::*;
        o.o.push_str(&match self {
            Float(value) => value.to_string(),
            Int(value) => value.to_string(),
            Bool(value) => (value as u8).to_string(),
            Char(value) => format!("'{}'", value),
            StrZ(value) => format!("\"{}\"", value), // fixme maybe we dont want these being a ptr to a global and zero terminated? oh well
        })
    }
}

impl Type {
    pub fn gen(self, o: &mut Output) {
        use TypeKind::*;
        match self.kind {
            Primitive(ty) => o.o.push_str(ty.c_type()),
            Struct { .. } => {
                o.o.push_str("struct ");
                o.o.push_str(&self.encode(true).mangle())
            }
            Ptr(inner) => {
                inner.deref().clone().gen(o);
                o.o.push('*');
            }
            _ => panic!("tried to gen {}", self.encode(false)),
        }
    }
}

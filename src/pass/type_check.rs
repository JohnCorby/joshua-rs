//! type init, nesting prefix init, type check, symbol add, symbol check

use crate::context::{type_check_prelude, Intern, Output};
use crate::error::{err, Res};
use crate::pass::ast1::*;
use crate::pass::ast2;
use crate::pass::scope::{Scope, Scopes, Symbol};
use crate::pass::PrimitiveKind;
use crate::util::{IterExt, IterResExt, RcExt, StrExt};
use std::collections::HashMap;
use std::rc::Rc;

impl Program {
    pub fn type_check(self, o: &mut Output) -> Res<ast2::Program> {
        let mut scopes = Scopes(vec![Scope::new(false, false, None)]);
        type_check_prelude(&mut scopes, o);
        let defines = self
            .1
            .iter()
            .cloned()
            .map(|define| define.type_check(&mut scopes, o, Default::default()))
            .res_vec()?;
        debug_assert_eq!(scopes.0.len(), 1);
        Ok(ast2::Program(defines.into()))
    }
}

impl Define {
    /// non-empty `generic_replacements` means that this is a specialization
    pub fn type_check(
        self,
        scopes: &mut Scopes,
        o: &mut Output,
        generic_replacements: Rc<Vec<ast2::Type>>,
    ) -> Res<ast2::Define> {
        use Define::*;
        Ok(match self {
            Struct {
                span,
                name,
                generic_placeholders,
                body,
            } => {
                debug_assert!(generic_placeholders.is_empty() || generic_replacements.is_empty());

                if generic_placeholders.is_empty() {
                    let (var_defines, func_defines) =
                        body.iter()
                            .cloned()
                            .partition::<Vec<_>, _>(|define| match define {
                                Var(..) => true,
                                Func { .. } => false,
                                _ => panic!("struct body shouldn't have {:?}", define),
                            });

                    scopes.push(Scope::new(false, false, None));
                    let var_defines = var_defines
                        .into_iter()
                        .map(|x| x.type_check(scopes, o, Default::default()))
                        .res_vec()?;
                    scopes.pop();

                    // add symbol
                    let nesting_prefix = scopes.nesting_prefix();
                    scopes.add(
                        Symbol::Struct {
                            nesting_prefix,
                            name: name.1,
                            generic_replacements: generic_replacements.clone(),
                            field_types: var_defines
                                .iter()
                                .map(|x| {
                                    let ast2::Define::Var(var_define) = x else { unreachable!() };
                                    (var_define.name, var_define.ty.clone())
                                })
                                .collect::<HashMap<_, _>>()
                                .into(),
                        },
                        span,
                    )?;

                    let func_defines = func_defines
                        .into_iter()
                        .map(|mut x| {
                            // set func receiver ty to struct
                            let Func { receiver_ty, .. } = &mut x else { unreachable!() };
                            *receiver_ty = Some(Type::Named {
                                // fixme span bodge
                                span: name.0,
                                name,
                                generic_replacements: generic_replacements
                                    .iter()
                                    .cloned()
                                    .map(|x| x.into_ast1(name.0))
                                    .vec()
                                    .into(),
                            });

                            x.type_check(scopes, o, Default::default())
                        })
                        .res_vec()?;

                    let mut body = var_defines;
                    body.extend(func_defines);

                    ast2::Define::Struct {
                        nesting_prefix,
                        name: name.1,
                        generic_replacements,
                        body: body.into(),
                    }
                } else {
                    // add symbol
                    scopes.add(
                        Symbol::GenericStruct {
                            name: name.1,

                            span,
                            name_ast1: name,
                            generic_placeholders,
                            body,

                            scopes_index: scopes.0.len(),
                        },
                        span,
                    )?;

                    ast2::Define::NoGen
                }
            }

            Func {
                span,
                ty: ty_ast1,
                receiver_ty: receiver_ty_ast1,
                name,
                generic_placeholders,
                args,
                body,
            } => {
                debug_assert!(generic_placeholders.is_empty() || generic_replacements.is_empty());

                if !generic_placeholders.is_empty() {
                    // add placeholders
                    scopes.push(Scope::new(false, false, None));
                    for placeholder in generic_placeholders.iter() {
                        scopes.add(Symbol::GenericPlaceholder(placeholder.1), placeholder.0)?;
                    }
                }

                let ty = ty_ast1.clone().type_check(scopes, o)?;
                let receiver_ty = receiver_ty_ast1
                    .as_ref()
                    .cloned()
                    .map(|x| x.type_check(scopes, o))
                    .transpose()?;

                if generic_placeholders.is_empty() {
                    let nesting_prefix = scopes.nesting_prefix();

                    scopes.push(Scope::new(true, false, Some(ty.clone())));
                    let args: Rc<Vec<_>> = args
                        .iter()
                        .cloned()
                        .map(|x| x.type_check(scopes, o, true, false))
                        .res_vec()?
                        .into();

                    // add symbol
                    let scope = scopes.pop();
                    scopes.add(
                        Symbol::Func {
                            ty: ty.clone(),
                            nesting_prefix,
                            receiver_ty: receiver_ty.clone(),
                            name: name.1,
                            generic_replacements: generic_replacements.clone(),
                            arg_types: args.iter().cloned().map(|x| x.ty).vec().into(),
                        },
                        span,
                    )?;
                    scopes.push(scope);

                    let body = body.type_check(scopes, o)?;
                    scopes.check_return_called(span)?;
                    scopes.pop();

                    ast2::Define::Func {
                        ty,
                        nesting_prefix,
                        receiver_ty,
                        name: name.1,
                        generic_replacements,
                        args,
                        body,
                    }
                } else {
                    let arg_types = args
                        .iter()
                        .cloned()
                        .map(|x| x.type_check(scopes, o, true, false).map(|x| x.ty))
                        .res_vec()?
                        .into();
                    scopes.pop();

                    // add symbol
                    scopes.add(
                        Symbol::GenericFunc {
                            receiver_ty,
                            name: name.1,
                            arg_types,

                            ty,
                            span,
                            ty_ast1,
                            receiver_ty_ast1,
                            name_ast1: name,
                            generic_placeholders,
                            args,
                            body,

                            scopes_index: scopes.0.len(),
                        },
                        span,
                    )?;

                    ast2::Define::NoGen
                }
            }

            Var(var_define) => {
                debug_assert!(generic_replacements.is_empty());
                ast2::Define::Var(var_define.type_check(scopes, o, false, false)?)
            }
            CCode(c_code) => {
                debug_assert!(generic_replacements.is_empty());
                ast2::Define::CCode(c_code.type_check(scopes, o)?)
            }
        })
    }
}

impl VarDefine {
    pub fn type_check(
        self,
        scopes: &mut Scopes,
        o: &mut Output,

        is_func_arg: bool,
        is_for_init: bool,
    ) -> Res<ast2::VarDefine> {
        if matches!(self.ty, Type::Primitive(.., PrimitiveKind::Void)) {
            return err("vars can't have void type", self.span);
        }

        let is_global = scopes.0.len() == 1;
        if is_global && self.value.is_none() {
            return err("global vars must have initializer", self.span);
        }
        if is_func_arg && self.value.is_some() {
            return err("func args can't have initializer", self.span);
        }
        if is_for_init && self.value.is_none() {
            return err("for loop var must have initializer", self.span);
        }

        // special case for auto type, where lvalue is inferred from rvalue instead of the other way around
        let (ty, value) = if matches!(self.ty, Type::Auto(..)) {
            if let Some(value) = self.value {
                let value = value.type_check(scopes, o, None)?;
                let ty = value.ty();
                (ty, Some(value))
            } else {
                return err("cannot infer type", self.span);
            }
        } else {
            let ty = self.ty.type_check(scopes, o)?;
            if let Some(value) = self.value {
                let value = value.type_check(scopes, o, Some(&ty))?;

                // check matching
                value.ty().check(&ty, self.span)?;
                (ty, Some(value))
            } else {
                (ty, None)
            }
        };

        // add symbol
        scopes.add(Symbol::Var(ty.clone(), self.name.1), self.span)?;

        Ok(ast2::VarDefine {
            ty,
            name: self.name.1,
            value,

            is_global,
        })
    }
}

impl Statement {
    pub fn type_check(self, scopes: &mut Scopes, o: &mut Output) -> Res<ast2::Statement> {
        use Statement::*;
        Ok(match self {
            Return(span, value) => {
                let value = value
                    .map(|it| it.type_check(scopes, o, Some(&scopes.func_return_type().clone())))
                    .transpose()?;

                // check return type
                scopes.return_called();
                value
                    .as_ref()
                    .map(|x| x.ty())
                    .unwrap_or_default()
                    .check(scopes.func_return_type(), span)?;

                ast2::Statement::Return(value)
            }
            Break(span) => {
                if !scopes.in_loop() {
                    return err("break can't be used outside of loops", span);
                }
                ast2::Statement::Break
            }
            Continue(span) => {
                if !scopes.in_loop() {
                    return err("continue can't be used outside of loops", span);
                }
                ast2::Statement::Continue
            }
            If {
                span,
                cond,
                then,
                otherwise,
            } => {
                let cond =
                    cond.type_check(scopes, o, Some(&ast2::Type::Primitive(PrimitiveKind::Bool)))?;
                scopes.push(Scope::new(true, false, None));
                let then = then.type_check(scopes, o)?;
                scopes.pop();
                let otherwise = otherwise
                    .map(|otherwise| {
                        scopes.push(Scope::new(true, false, None));
                        let otherwise = otherwise.type_check(scopes, o);
                        scopes.pop();
                        otherwise
                    })
                    .transpose()?;

                // check condition
                cond.ty()
                    .check(&ast2::Type::Primitive(PrimitiveKind::Bool), span)?;

                ast2::Statement::If {
                    cond,
                    then,
                    otherwise,
                }
            }
            Until { span, cond, block } => {
                let cond =
                    cond.type_check(scopes, o, Some(&ast2::Type::Primitive(PrimitiveKind::Bool)))?;
                scopes.push(Scope::new(true, true, None));
                let block = block.type_check(scopes, o)?;
                scopes.pop();

                // check condition
                cond.ty()
                    .check(&ast2::Type::Primitive(PrimitiveKind::Bool), span)?;

                ast2::Statement::Until { cond, block }
            }
            For {
                span,
                init,
                cond,
                update,
                block,
            } => {
                scopes.push(Scope::new(true, true, None));
                let init = init.type_check(scopes, o, false, true)?;
                let cond =
                    cond.type_check(scopes, o, Some(&ast2::Type::Primitive(PrimitiveKind::Bool)))?;
                let update = update.cloned().type_check(scopes, o)?;
                let block = block.type_check(scopes, o)?;
                scopes.pop();

                // check condition
                cond.ty()
                    .check(&ast2::Type::Primitive(PrimitiveKind::Bool), span)?;

                ast2::Statement::For {
                    init,
                    cond,
                    update: update.into(),
                    block,
                }
            }
            ExprAssign {
                span,
                lvalue,
                rvalue,
            } => {
                let lvalue = lvalue.type_check(scopes, o, None)?;
                let rvalue = rvalue.type_check(scopes, o, Some(&lvalue.ty()))?;

                // check matching
                lvalue.check_assignable(span)?;
                rvalue.ty().check(&lvalue.ty(), span)?;

                ast2::Statement::ExprAssign { lvalue, rvalue }
            }
            Define(define) => {
                ast2::Statement::Define(define.type_check(scopes, o, Default::default())?)
            }
            Expr(expr) => ast2::Statement::Expr(expr.type_check(scopes, o, None)?),
        })
    }
}

impl Block {
    pub fn type_check(self, scopes: &mut Scopes, o: &mut Output) -> Res<ast2::Block> {
        self.1
            .iter()
            .cloned()
            .map(|statement| statement.type_check(scopes, o))
            .res_vec()
            .map(|statements| ast2::Block(statements.into()))
    }
}

impl CCode {
    pub fn type_check(self, scopes: &mut Scopes, o: &mut Output) -> Res<ast2::CCode> {
        self.1
            .iter()
            .cloned()
            .map(|part| {
                Ok(match part {
                    CCodePart::String(.., str) => ast2::CCodePart::String(str),
                    CCodePart::Expr(expr) => {
                        ast2::CCodePart::Expr(expr.type_check(scopes, o, None)?)
                    }
                })
            })
            .res_vec()
            .map(|parts| ast2::CCode(parts.into()))
    }
}

impl Expr {
    pub fn type_check(
        self,
        scopes: &mut Scopes,
        o: &mut Output,
        type_hint: Option<&ast2::Type>,
    ) -> Res<ast2::Expr> {
        use Expr::*;
        Ok(match self {
            Cast { span, thing, ty } => {
                let ty = ty.type_check(scopes, o)?;
                let thing = thing.cloned().type_check(scopes, o, Some(&ty))?;

                // symbol check
                // fixme hacky as shit
                let nesting_prefix =
                    if matches!(thing.ty(), ast2::Type::Literal(..) | ast2::Type::CCode) {
                        // casting will always work
                        ""
                    } else {
                        let symbol = scopes.find(
                            o,
                            &Symbol::new_func(
                                None,
                                format!("as {}", ty.encode(true)).intern(),
                                Default::default(),
                                vec![thing.ty()].into(),
                            ),
                            None,
                            span,
                        )?;
                        debug_assert_eq!(ty, symbol.ty());
                        let Symbol::Func { nesting_prefix, .. } = symbol else { unreachable!() };
                        nesting_prefix
                    };

                ast2::Expr::Cast {
                    nesting_prefix,
                    thing: thing.into(),
                    ty,
                }
            }
            MethodCall {
                span,
                receiver,
                mut func_call,
            } => {
                let receiver = receiver.cloned();
                func_call.args.modify(|x| x.insert(0, receiver.clone()));
                func_call.span = span;

                let receiver_span = receiver.span();
                let receiver = receiver.type_check(scopes, o, None)?;
                func_call.receiver_ty = Some(receiver.ty().into_ast1(receiver_span));

                func_call.type_check(scopes, o, type_hint)?
            }
            Field {
                span,
                receiver,
                name,
            } => {
                let receiver = receiver.cloned().type_check(scopes, o, None)?;

                // field check
                let symbol = match receiver.ty() {
                    ast2::Type::Struct {
                        name,
                        generic_replacements,
                        ..
                    } => Symbol::new_struct(name, generic_replacements),
                    ty => {
                        return err(
                            &format!("expected struct, but got {}", ty.encode(false)),
                            span,
                        )
                    }
                };
                let symbol = scopes.find(o, &symbol, None, span)?;
                let Symbol::Struct { field_types, .. } = &symbol else { unreachable!() };
                let ty = match field_types.get(name.1) {
                    Some(field_type) => field_type.clone(),
                    None => return err(&format!("no field named {} in {}", name.1, symbol), span),
                };

                ast2::Expr::Field {
                    receiver: receiver.into(),
                    name: name.1,
                    ty,
                }
            }
            Literal(.., literal) => ast2::Expr::Literal(literal),
            FuncCall(func_call) => func_call.type_check(scopes, o, type_hint)?,
            Var(name) => {
                // symbol check
                let ty = scopes.find(o, &Symbol::new_var(name.1), None, name.0)?.ty();
                ast2::Expr::Var(name.1, ty)
            }
            CCode(c_code) => ast2::Expr::CCode(c_code.type_check(scopes, o)?),
        })
    }
}

impl FuncCall {
    pub fn type_check(
        self,
        scopes: &mut Scopes,
        o: &mut Output,
        type_hint: Option<&ast2::Type>,
    ) -> Res<ast2::Expr> {
        let receiver_ty = self
            .receiver_ty
            .map(|it| it.type_check(scopes, o))
            .transpose()?;

        let generic_replacements: Rc<Vec<_>> = self
            .generic_replacements
            .iter()
            .cloned()
            .map(|it| it.type_check(scopes, o))
            .res_vec()?
            .into();
        let args: Rc<Vec<_>> = self
            .args
            .iter()
            .cloned()
            .map(|it| it.type_check(scopes, o, None))
            .res_vec()?
            .into();

        // symbol check
        let symbol = scopes.find(
            o,
            &Symbol::new_func(
                receiver_ty,
                self.name.1,
                generic_replacements,
                args.iter().map(|it| it.ty()).vec().into(),
            ),
            type_hint,
            self.span,
        )?;
        let Symbol::Func {
            ty,
            nesting_prefix,
            receiver_ty,
            generic_replacements,
            ..
        } = symbol else { unreachable!() };
        Ok(ast2::Expr::FuncCall {
            nesting_prefix,
            receiver_ty,
            name: self.name.1,
            generic_replacements,
            args,
            ty,
        })
    }
}

impl Type {
    pub fn type_check(self, scopes: &mut Scopes, o: &mut Output) -> Res<ast2::Type> {
        use Type::*;
        Ok(match self {
            Primitive(.., kind) => ast2::Type::Primitive(kind),
            Ptr(.., inner) => ast2::Type::Ptr(inner.cloned().type_check(scopes, o)?.into()),
            Named {
                span,
                name,
                generic_replacements,
            } => {
                let generic_replacements: Rc<Vec<_>> = generic_replacements
                    .iter()
                    .cloned()
                    .map(|x| x.type_check(scopes, o))
                    .res_vec()?
                    .into();

                // symbol check
                // fixme make this actually display the proper error message instead of always saying "could not find struct or generic placeholder"
                scopes
                    .find(
                        o,
                        &Symbol::new_struct(name.1, generic_replacements.clone()),
                        None,
                        span,
                    )
                    .or_else(|_| {
                        scopes.find(o, &Symbol::new_generic_placeholder(name.1), None, name.0)
                    })
                    .or_else(|_| {
                        err(
                            &format!(
                                "could not find struct or generic placeholder {}",
                                name.1.encode("", None, &generic_replacements, None, false)
                            ),
                            span,
                        )
                    })?
                    .ty()
            }
            Auto(..) => ast2::Type::Auto,
        })
    }
}

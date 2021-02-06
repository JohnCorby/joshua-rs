use crate::cached::CachedString;
use crate::error::{unexpected_rule, MyResult};
use crate::parse::{Node, Rule};
use crate::pass::{Gen, Visit};
use crate::scope::Scope;
use crate::span::Span;
use crate::with::WithSpan;
use std::str::FromStr;

#[derive(Debug, Copy, Clone)]
pub enum Type {
    Primitive(PrimitiveType),
    Literal(LiteralType),
    Named(CachedString),
}
impl Default for Type {
    fn default() -> Self {
        Self::Named(CachedString::from(String::new()))
    }
}
impl Type {
    pub fn check(self, expected: Type) -> MyResult<()> {
        let actual = self;
        if expected == actual {
            Ok(())
        } else {
            Err(format!(
                "expected {}, but got {}",
                expected.to_string(),
                actual.to_string()
            )
            .into())
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, strum::EnumString, strum::ToString)]
#[strum(serialize_all = "snake_case")]
pub enum PrimitiveType {
    I8,
    U8,
    I16,
    U16,
    I32,
    U32,
    I64,
    U64,
    F32,
    F64,
    Bool,
    Char,
    Void,
}
#[derive(Debug, Copy, Clone, PartialEq, strum::ToString)]
pub enum LiteralType {
    Float,
    Int,
}

impl PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
        use Type::*;
        match (self, other) {
            (Primitive(ty1), Primitive(ty2)) => ty1 == ty2,
            (Named(name1), Named(name2)) => name1 == name2,
            (Literal(ty1), Literal(ty2)) => ty1 == ty2,
            (_, _) => false,
        }
    }
}
impl ToString for Type {
    fn to_string(&self) -> String {
        match self {
            Type::Primitive(ty) => format!("primitive type {}", ty.to_string()),
            Type::Named(name) => format!("named type `{}`", name.to_string()),
            Type::Literal(ty) => format!("literal type {}", ty.to_string()),
        }
    }
}

impl Visit for Type {
    fn visit_impl(node: Node) -> Self {
        let node = node.into_inner_checked(Rule::ty).next().unwrap();
        match node.rule() {
            Rule::primitive => Self::Primitive(PrimitiveType::from_str(node.as_str()).unwrap()),
            Rule::ident => Self::Named(node.as_str().into()),

            rule => unexpected_rule(rule),
        }
    }
}

impl Gen for WithSpan<Type> {
    fn span(&self) -> Span {
        self.1
    }

    fn gen_impl(self) -> MyResult<String> {
        Ok(match self.0 {
            Type::Primitive(ty) => {
                use PrimitiveType::*;
                match ty {
                    I8 => "signed char",
                    U8 => "unsigned char",
                    I16 => "signed short",
                    U16 => "unsigned short",
                    I32 => "signed int",
                    U32 => "unsigned int",
                    I64 => "signed long long",
                    U64 => "unsigned long long",
                    F32 => "float",
                    F64 => "double",
                    Bool => "unsigned char",
                    Char => "unsigned char",
                    Void => "void",
                }
                .into()
            }
            Type::Named(name) => {
                Scope::current().get_type(name)?;
                name.to_string()
            }
            ty => panic!("tried to gen {}", ty.to_string()),
        })
    }
}

/// todo make type checking a separate pass instead of shoving it after gen and doing a lot of dumb cloning
pub trait HasType {
    fn ty(&self) -> Type;
}
impl HasType for PrimitiveType {
    fn ty(&self) -> Type {
        Type::Primitive(*self)
    }
}
impl HasType for LiteralType {
    fn ty(&self) -> Type {
        Type::Literal(*self)
    }
}

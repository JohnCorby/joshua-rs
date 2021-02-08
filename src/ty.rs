use crate::cached::CachedString;
use crate::error::{err, unexpected_rule, MyResult};
use crate::parse::{Node, Rule};
use crate::pass::{Gen, Visit};
use crate::scope::Scope;
use crate::span::Span;
use crate::with::WithSpan;
use std::fmt::{Display, Formatter};
use std::hash::{Hash, Hasher};
use std::str::FromStr;

#[derive(Debug, Copy, Clone)]
pub enum Type {
    Primitive(PrimitiveType),
    Named(CachedString),
    Literal(LiteralType),
    CCode,
}
impl Default for Type {
    fn default() -> Self {
        Self::Primitive(PrimitiveType::Void)
    }
}
impl Type {
    pub fn check(self, expected: Type) -> MyResult<()> {
        let actual = self;
        if expected == actual {
            Ok(())
        } else {
            err(format!("expected {}, but got {}", expected, actual))
        }
    }
}

#[derive(Debug, Copy, Clone, Hash, PartialEq, strum::EnumString, strum::Display)]
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
#[derive(Debug, Copy, Clone, Hash, PartialEq, strum::Display)]
pub enum LiteralType {
    Float,
    Int,
}

/// note: eq contains cases that hash doesnt cover, check both when comparing
impl Hash for Type {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Type::Primitive(ty) => ty.hash(state),
            Type::Named(name) => name.hash(state),
            Type::Literal(ty) => ty.hash(state),
            Type::CCode => ().hash(state),
        }
    }
}
impl PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
        use Type::*;
        match (self, other) {
            (Primitive(ty1), Primitive(ty2)) => ty1 == ty2,
            (Named(name1), Named(name2)) => name1 == name2,
            (Literal(ty1), Literal(ty2)) => ty1 == ty2,

            // c code can be any type lol
            (CCode, _) | (_, CCode) => true,

            _ => false,
        }
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            Type::Primitive(ty) => write!(f, "primitive type {}", ty),
            Type::Named(name) => write!(f, "named type `{}`", name),
            Type::Literal(ty) => write!(f, "literal type {}", ty),
            Type::CCode => write!(f, "c code type"),
        }
    }
}

impl Visit for Type {
    fn visit_impl(node: Node) -> Self {
        let node = node.into_inner_checked(Rule::ty).next().unwrap();
        match node.rule() {
            Rule::primitive => Self::Primitive(PrimitiveType::from_str(node.as_str()).unwrap()),
            Rule::ident => Self::Named(node.as_str().into()),

            _ => unexpected_rule(node),
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
            ty => panic!("tried to gen {}", ty),
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

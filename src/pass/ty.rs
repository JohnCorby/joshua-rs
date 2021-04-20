use crate::error::{err, Res};
use crate::span::Span;
use crate::util::interned_str::InternedStr;
use std::fmt::{Display, Formatter};
use std::rc::Rc;

/// note: cloning is okay and cheap because it's Rc::clone
#[derive(Debug, Clone, Hash, PartialEq)]
pub enum Type<'i> {
    Primitive(PrimitiveType),
    Literal(LiteralType),
    Struct(InternedStr<'i>),
    GenericPlaceholder(InternedStr<'i>),
    Ptr(Rc<Type<'i>>),
}
impl Type<'i> {
    pub fn check(&self, expected: &Self, span: Option<Span<'i>>) -> Res<'i, ()> {
        let actual = self;
        if expected == actual {
            Ok(())
        } else {
            err(&format!("expected {}, but got {}", expected, actual), span)
        }
    }

    pub fn name(&self) -> String {
        use Type::*;
        match self {
            Primitive(ty) => ty.to_string(),
            Struct(name) => name.to_string(),
            GenericPlaceholder(name) => name.to_string(),
            Literal(ty) => ty.to_string(),
            Ptr(ty) => format!("ptr<{}>", ty.name()),
        }
    }
}
impl Default for Type<'_> {
    fn default() -> Self {
        Self::Primitive(PrimitiveType::Void)
    }
}

impl Display for Type<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        use Type::*;
        match self {
            Primitive(ty) => write!(f, "primitive type {}", ty),
            Struct(name) => write!(f, "struct type `{}`", name),
            GenericPlaceholder(name) => write!(f, "generic placeholder type `{}`", name),
            Literal(ty) => write!(f, "literal type {}", ty),
            Ptr(ty) => write!(f, "pointer type to {}", ty),
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
impl PrimitiveType {
    pub fn ty(&self) -> Type<'_> {
        Type::Primitive(*self)
    }

    pub fn c_type(&self) -> &str {
        use PrimitiveType::*;
        match self {
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
    }
}

#[derive(Debug, Copy, Clone, Hash, PartialEq, strum::Display)]
#[strum(serialize_all = "snake_case")]
pub enum LiteralType {
    Float,
    Int,
    CCode,
}
impl LiteralType {
    pub fn ty(&self) -> Type<'_> {
        Type::Literal(*self)
    }
}

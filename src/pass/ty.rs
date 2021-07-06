//! helper stuff for `ast2::Type`

use crate::error::{err, Res};
use crate::pass::ast2::Type;
use crate::span::Span;
use crate::util::{IterExt, StrExt};
use std::fmt::{Display, Formatter};

impl Type<'i> {
    pub fn check(&self, expected: &Self, span: Span<'i>) -> Res<'i> {
        let actual = self;
        if expected == actual {
            Ok(())
        } else {
            err(&format!("expected {}, but got {}", expected, actual), span)
        }
    }
}
impl Default for Type<'_> {
    fn default() -> Self {
        Self::Primitive(PrimitiveType::Void)
    }
}

impl Display for Type<'_> {
    /// also used in code gen
    ///
    /// NOTE: structs include nesting prefix
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        use Type::*;
        match self {
            Primitive(ty) => ty.fmt(f),
            Struct {
                nesting_prefix,
                name,
                generic_replacements,
            } => write!(
                f,
                "{}{}",
                nesting_prefix,
                name.encode(&generic_replacements.iter().vec(), None)
            ),
            Ptr(ty) => write!(f, "ptr<{}>", ty),
            GenericPlaceholder(name) => f.write_str(name),
            _ => panic!("type {:?} shouldn't be displayed", self),
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
    Void,
}
impl PrimitiveType {
    pub const fn ty(&self) -> Type<'static> {
        Type::Primitive(*self)
    }

    pub const fn c_type(&self) -> &str {
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
            Void => "void",
        }
    }
}

#[derive(Debug, Copy, Clone, Hash, PartialEq)]
pub enum LiteralType {
    Float,
    Int,
}
impl LiteralType {
    pub const fn ty(&self) -> Type<'static> {
        Type::Literal(*self)
    }
}

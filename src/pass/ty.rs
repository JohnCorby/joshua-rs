use crate::error::{err, Res};
use crate::span::Span;
use crate::util::ctx_str::CtxStr;
use crate::util::{IterExt, StrExt};
use std::fmt::{Display, Formatter};
use std::rc::Rc;

#[derive(Debug, Clone, Hash, PartialEq)]
pub enum Type<'i> {
    Primitive(PrimitiveType),
    /// fixme merge these into generics when we get type inference
    Literal(LiteralType),
    CCode,
    Struct {
        name: CtxStr<'i>,
        generic_replacements: Rc<Vec<Type<'i>>>,
    },
    /// replaced with concrete type on specialization
    GenericPlaceholder(CtxStr<'i>),
    Ptr(Rc<Type<'i>>),
    /// for inferring with var define and probably other stuff later
    Auto,
    /// for when we use generics and we don't know what types are yet (fields, func return types, etc)
    GenericUnknown,
}
impl Type<'i> {
    pub fn check(&self, expected: &Self, span: Option<Span<'i>>) -> Res<'i> {
        // very lol
        if matches!(self, Type::GenericUnknown | Type::GenericPlaceholder(_))
            || matches!(expected, Type::GenericUnknown | Type::GenericPlaceholder(_))
        {
            return Ok(());
        }
        let actual = self;
        if expected == actual {
            Ok(())
        } else {
            err(&format!("expected {}, but got {}", expected, actual), span)
        }
    }

    /// name used in funcs
    pub fn encoded_name(&self) -> String {
        use Type::*;
        match self {
            Primitive(ty) => ty.to_string(),
            Struct {
                name,
                generic_replacements,
            } => name.encode(&[], &generic_replacements.iter().vec(), None),
            Ptr(ty) => format!("ptr<{}>", ty.encoded_name()),
            // _ => format!("{{{:?}}}", self),
            _ => panic!("internal type {:?} should not be used in func name", self),
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
            Struct {
                name,
                generic_replacements,
            } => f.write_str(&name.to_display(
                "struct type",
                &generic_replacements.iter().vec(),
                None,
            )),
            Ptr(ty) => write!(f, "pointer to {}", ty),
            // _ => write!(f, "internal type {:?}", self),
            _ => panic!("internal type {:?} should not be displayed", self),
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

#[derive(Debug, Copy, Clone, Hash, PartialEq, strum::Display)]
pub enum LiteralType {
    Float,
    Int,
}
impl LiteralType {
    pub const fn ty(&self) -> Type<'static> {
        Type::Literal(*self)
    }
}

//! helper stuff for `ast2::Type`

use crate::error::{err, Res};
use crate::pass::ast2::{Type, TypeKind};
use crate::span::Span;
use crate::util::StrExt;

impl Type {
    pub fn check(&self, expected: &Self, span: Span) -> Res {
        let actual = self;
        if expected == actual {
            Ok(())
        } else {
            err(
                &format!(
                    "expected {}, but got {}",
                    expected.encode(false),
                    actual.encode(false)
                ),
                span,
            )
        }
    }
}
impl Default for Type {
    fn default() -> Self {
        Self {
            span: Default::default(),
            kind: TypeKind::Primitive(PrimitiveType::Void),
        }
    }
}

impl Type {
    /// used for codegen and display
    pub fn encode(&self, include_nesting_prefixes: bool) -> String {
        use TypeKind::*;
        match &self.kind {
            Primitive(ty) => ty.to_string(),
            Struct {
                nesting_prefix,
                name,
                generic_replacements,
            } => name.encode(
                nesting_prefix,
                None,
                generic_replacements,
                None,
                include_nesting_prefixes,
            ),
            Ptr(ty) => format!("ptr<{}>", ty.encode(include_nesting_prefixes)),
            GenericPlaceholder(name) => name.to_string(),
            _ => panic!("type {:?} shouldn't be displayed or encoded", self),
        }
    }
}

#[derive(Debug, Copy, Clone, Hash, PartialEq, Default, EnumString, Display)]
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
    #[default]
    Void,
}
impl PrimitiveType {
    pub const fn ty(&self, span: Span) -> Type {
        Type {
            span,
            kind: TypeKind::Primitive(*self),
        }
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
    pub const fn ty(&self, span: Span) -> Type {
        Type {
            span,
            kind: TypeKind::Literal(*self),
        }
    }
}

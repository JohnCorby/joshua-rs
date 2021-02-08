use crate::cached::CachedString;
use crate::error::{err, unexpected_kind, MyResult};
use crate::parse::{Kind, Node};
use crate::pass::{Gen, Visit};
use crate::scope::Scope;
use crate::span::Span;
use std::fmt::{Display, Formatter};
use std::hash::{Hash, Hasher};
use std::str::FromStr;

#[derive(Debug, Copy, Clone)]
pub struct Type {
    span: Span,
    kind: Type,
}
#[derive(Debug, Copy, Clone)]
pub enum TypeKind {
    Primitive(PrimitiveType),
    Struct(CachedString),
    Literal(LiteralType),
    CCode,
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
impl PrimitiveType {
    pub fn ty(&self) -> Type {
        Self::Primitive(*self)
    }
}

#[derive(Debug, Copy, Clone, Hash, PartialEq, strum::Display)]
pub enum LiteralType {
    Float,
    Int,
}
impl LiteralType {
    pub fn ty(&self) -> Type {
        Self::Literal(*self)
    }
}

/// note: eq contains cases that hash doesnt cover, check both when comparing
impl Hash for Type {
    fn hash<H: Hasher>(&self, state: &mut H) {
        use TypeKind::*;
        match &self.kind {
            Primitive(ty) => ty.hash(state),
            Struct(name) => name.hash(state),
            Literal(ty) => ty.hash(state),
            CCode => ().hash(state),
        }
    }
}
impl PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
        use TypeKind::*;
        match (self.kind, other.kind) {
            (Primitive(ty1), Primitive(ty2)) => ty1 == ty2,
            (Struct(name1), Struct(name2)) => name1 == name2,
            (Literal(ty1), Literal(ty2)) => ty1 == ty2,

            // c code can be any type lol
            (CCode, _) | (_, CCode) => true,

            _ => false,
        }
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        use TypeKind::*;
        match self {
            Primitive(ty) => write!(f, "primitive type {}", ty),
            Struct(name) => write!(f, "named type `{}`", name),
            Literal(ty) => write!(f, "literal type {}", ty),
            CCode => write!(f, "c code type"),
        }
    }
}

impl Visit for Type {
    fn visit_impl(node: Node) -> Self {
        let node = node.children_checked(Kind::ty).next().unwrap();
        let kind = match node.kind() {
            Kind::primitive => Self::Primitive(PrimitiveType::from_str(node.as_str()).unwrap()),
            Kind::ident => Self::Struct(node.as_str().into()),

            _ => unexpected_kind(node),
        };

        Self {
            span: node.span(),
            kind,
        }
    }
}

impl Gen for Type {
    fn span(&self) -> Span {
        self.span
    }

    fn gen_impl(self) -> MyResult<String> {
        use TypeKind::*;
        Ok(match self.kind {
            Primitive(ty) => {
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
            Struct(name) => {
                Scope::current().get_struct(name)?;
                name.to_string()
            }
            ty => panic!("tried to gen {}", ty),
        })
    }
}

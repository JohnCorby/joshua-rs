use crate::cached::CachedString;
use crate::error::{err, unexpected_kind, Res};
use crate::expr::VisitIdent;
use crate::parse::{Kind, Node};
use crate::scope::Scope;
use crate::span::Span;
use crate::util::Visit;
use std::fmt::{Display, Formatter};
use std::hash::{Hash, Hasher};
use std::lazy::OnceCell;

#[derive(Debug, Clone)]
pub struct Type {
    span: Span,
    kind: TypeKind,
    ty: OnceCell<TypeKind>,
}

impl Visit for Type {
    fn visit(node: Node) -> Self {
        let node = node.children_checked(Kind::ty).next().unwrap();
        let kind = match node.kind() {
            Kind::primitive => TypeKind::Primitive(node.as_str().parse().unwrap()),
            Kind::ident => TypeKind::Struct(node.visit_ident()),

            _ => unexpected_kind(node),
        };

        Self {
            span: node.span(),
            kind,
            ty: Default::default(),
        }
    }
}

impl Type {
    pub fn init_ty(&self) -> Res<TypeKind> {
        self.ty
            .get_or_try_init(|| {
                if let TypeKind::Struct(name) = self.kind {
                    Scope::current().get_struct(name, self.span)?;
                }
                Ok(self.kind)
            })
            .copied()
    }

    pub fn gen(self, c_code: &mut String) -> Res<()> {
        use TypeKind::*;
        match self.kind {
            Primitive(ty) => c_code.push_str(ty.c_type()),
            Struct(name) => {
                Scope::current().get_struct(name, self.span)?;
                c_code.push_str(&name.to_string())
            }
            kind => panic!("tried to gen {}", kind),
        }
        Ok(())
    }
}

#[derive(Debug, Copy, Clone)]
pub enum TypeKind {
    Primitive(PrimitiveType),
    Struct(CachedString),
    Literal(LiteralType),
    CCode,
}
impl TypeKind {
    pub fn check(&self, expected: &Self, span: impl Into<Option<Span>>) -> Res<()> {
        let actual = self;
        if expected == actual {
            Ok(())
        } else {
            err(
                format!("expected {}, but got {}", expected, actual),
                span.into(),
            )
        }
    }

    pub fn name(&self) -> String {
        use TypeKind::*;
        match self {
            Primitive(ty) => ty.to_string(),
            Struct(name) => name.to_string(),
            Literal(ty) => ty.to_string(),
            _ => unreachable!("{} doesnt have a name", self),
        }
    }
}
impl Default for TypeKind {
    fn default() -> Self {
        Self::Primitive(PrimitiveType::Void)
    }
}

/// note: eq contains cases that hash doesnt cover, check both when comparing
impl Hash for TypeKind {
    fn hash<H: Hasher>(&self, state: &mut H) {
        use TypeKind::*;
        match self {
            Primitive(ty) => ty.hash(state),
            Struct(name) => name.hash(state),
            Literal(ty) => ty.hash(state),
            CCode => ().hash(state),
        }
    }
}
impl PartialEq for TypeKind {
    fn eq(&self, other: &Self) -> bool {
        use TypeKind::*;
        match (self, other) {
            (Primitive(ty1), Primitive(ty2)) => ty1 == ty2,
            (Struct(name1), Struct(name2)) => name1 == name2,
            (Literal(ty1), Literal(ty2)) => ty1 == ty2,

            // c code can be any type lol
            (CCode, _) | (_, CCode) => true,

            _ => false,
        }
    }
}

impl Display for TypeKind {
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
    pub fn ty(&self) -> TypeKind {
        TypeKind::Primitive(*self)
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
pub enum LiteralType {
    Float,
    Int,
}
impl LiteralType {
    pub fn ty(&self) -> TypeKind {
        TypeKind::Literal(*self)
    }
}

use crate::context::Ctx;
use crate::error::{err, unexpected_kind, Res};
use crate::interned_string::InternedStr;
use crate::parse::{Kind, Node};
use crate::span::Span;
use crate::util::Visit;
use std::fmt::{Display, Formatter};
use std::hash::{Hash, Hasher};
use std::lazy::OnceCell;

#[derive(Debug, Clone)]
pub struct TypeNode<'i> {
    span: Span<'i>,
    _ty: Type<'i>,
    ty: OnceCell<Type<'i>>,
}

impl<'i> Visit<'i> for TypeNode<'i> {
    fn visit(node: Node<'i>, ctx: &mut Ctx<'i>) -> Self {
        let node = node.children_checked(Kind::ty).next().unwrap();
        let ty = match node.kind() {
            Kind::primitive => Type::Primitive(node.str().parse().unwrap()),
            Kind::ident => Type::Struct(node.visit_ident(ctx)),

            _ => unexpected_kind(node),
        };

        Self {
            span: node.span(),
            _ty: ty,
            ty: Default::default(),
        }
    }
}

impl<'i> TypeNode<'i> {
    pub fn init_ty(&self, ctx: &Ctx<'i>) -> Res<'i, Type<'i>> {
        self.ty
            .get_or_try_init(|| {
                if let Type::Struct(name) = self._ty {
                    ctx.scopes.get_struct(name, self.span)?;
                }
                Ok(self._ty)
            })
            .copied()
    }

    pub fn gen(self, ctx: &mut Ctx<'i>) -> Res<'i, ()> {
        use Type::*;
        match self._ty {
            Primitive(ty) => ctx.o.push_str(ty.c_type()),
            Struct(name) => {
                ctx.scopes.get_struct(name, self.span)?;
                ctx.o.push_str(&name)
            }
            ty => panic!("tried to gen {}", ty),
        }
        Ok(())
    }
}

#[derive(Debug, Copy, Clone)]
pub enum Type<'i> {
    Primitive(PrimitiveType),
    Struct(InternedStr<&'i str>),
    Literal(LiteralType),
    CCode,
}
impl<'i> Type<'i> {
    pub fn check(self, expected: Self, span: impl Into<Option<Span<'i>>>) -> Res<'i, ()> {
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

    pub fn name(self) -> String {
        use Type::*;
        match self {
            Primitive(ty) => ty.to_string(),
            Struct(name) => name.to_string(),
            Literal(ty) => ty.to_string(),
            _ => unreachable!("{} doesnt have a name", self),
        }
    }
}
impl Default for Type<'_> {
    fn default() -> Self {
        Self::Primitive(PrimitiveType::Void)
    }
}

/// note: eq contains cases that hash doesnt cover, check both when comparing
impl Hash for Type<'_> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        use Type::*;
        match self {
            Primitive(ty) => ty.hash(state),
            Struct(name) => name.hash(state),
            Literal(ty) => ty.hash(state),
            CCode => ().hash(state),
        }
    }
}
impl PartialEq for Type<'_> {
    fn eq(&self, other: &Self) -> bool {
        use Type::*;
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

impl Display for Type<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        use Type::*;
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
pub enum LiteralType {
    Float,
    Int,
}
impl LiteralType {
    pub fn ty(&self) -> Type<'_> {
        Type::Literal(*self)
    }
}

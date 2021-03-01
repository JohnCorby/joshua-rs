use crate::context::Ctx;
use crate::error::{err, unexpected_kind, Res};
use crate::parse::{Kind, Node};
use crate::scope::Symbol;
use crate::span::Span;
use crate::util::interned_str::InternedStr;
use crate::util::late_init::LateInit;
use crate::util::{Mangle, Visit};
use std::fmt::{Display, Formatter};
// use std::hash::{Hash, Hasher};
use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct TypeNode<'i> {
    pub span: Span<'i>,
    pub kind: TypeKind<'i>,
    pub ty: LateInit<Type<'i>>,
}

impl<'i> Visit<'i> for TypeNode<'i> {
    fn visit(node: Node<'i>, ctx: &mut Ctx<'i>) -> Self {
        let node = node.children_checked(Kind::ty).next().unwrap();
        let span = node.span();
        use TypeKind::*;
        let ty = match node.kind() {
            Kind::primitive => Primitive(node.str().parse().unwrap()),
            Kind::ptr => Ptr(node
                .children()
                .next()
                .unwrap()
                .visit::<TypeNode<'i>>(ctx)
                .into()),
            Kind::ident => Named(node.visit_ident(ctx)),

            _ => unexpected_kind(node),
        };

        Self {
            span,
            kind: ty,
            ty: Default::default(),
        }
    }
}

impl<'i> TypeNode<'i> {
    pub fn gen(self, ctx: &mut Ctx<'i>) {
        use Type::*;
        match *self.ty {
            Primitive(ty) => ctx.o.push_str(ty.c_type()),
            Struct(name) => {
                ctx.o.push_str("struct ");
                ctx.o.push_str(&name.mangle())
            }
            ty => panic!("tried to gen {}", ty),
        }
    }
}

#[derive(Debug, Clone)]
pub enum TypeKind<'i> {
    Primitive(PrimitiveType),
    Ptr(Rc<TypeNode<'i>>),
    Named(InternedStr<'i>),
}

#[derive(Debug, Copy, Clone, Hash, PartialEq)]
pub enum Type<'i> {
    Primitive(PrimitiveType),
    Literal(LiteralType),
    Struct(InternedStr<'i>),
    GenericPlaceholder(InternedStr<'i>),
    // Ptr(Rc<Type<'i>>),
}
impl<'i> Type<'i> {
    pub fn check(self, expected: Self, span: Option<Span<'i>>) -> Res<'i, ()> {
        let actual = self;
        if expected == actual {
            Ok(())
        } else {
            err(&format!("expected {}, but got {}", expected, actual), span)
        }
    }

    pub fn name(self) -> String {
        use Type::*;
        match self {
            Primitive(ty) => ty.to_string(),
            Struct(name) => format!("s({})", name),
            GenericPlaceholder(name) => format!("g({})", name),
            Literal(ty) => ty.to_string(),
        }
    }

    pub fn symbol<'c>(&self, ctx: &'c mut Ctx<'i>) -> &'c Symbol<'i> {
        use Type::*;
        match self {
            Struct(name) => ctx
                .scopes
                .find(&Symbol::new_struct_type(*name), None)
                .unwrap(),
            GenericPlaceholder(name) => ctx
                .scopes
                .find(&Symbol::new_generic_placeholder_type(*name), None)
                .unwrap(),
            ty => panic!("{} doesn't have symbol counterpart", ty),
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

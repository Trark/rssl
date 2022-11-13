use crate::primitive_types::*;
use crate::ScopedIdentifier;
use rssl_text::Located;

#[derive(PartialEq, Clone)]
pub struct Type(pub TypeLayout, pub TypeModifier);

#[derive(PartialEq, Clone)]
pub enum TypeLayout {
    Void,
    Scalar(ScalarType),
    Vector(ScalarType, u32),
    Matrix(ScalarType, u32, u32),
    Custom(Box<ScopedIdentifier>, Vec<Located<Type>>),
}

/// Template parameters
#[derive(PartialEq, Debug, Clone)]
pub struct TemplateParamList(pub Vec<Located<String>>);

impl Type {
    pub const fn void() -> Type {
        Type::from_layout(TypeLayout::Void)
    }
    pub const fn from_layout(layout: TypeLayout) -> Type {
        Type(layout, TypeModifier::new())
    }
    pub const fn from_scalar(scalar: ScalarType) -> Type {
        Type::from_layout(TypeLayout::from_scalar(scalar))
    }

    pub const fn uint() -> Type {
        Type::from_layout(TypeLayout::uint())
    }
    pub const fn int() -> Type {
        Type::from_layout(TypeLayout::int())
    }
    pub const fn long() -> Type {
        Type::from_layout(TypeLayout::long())
    }
    pub const fn float() -> Type {
        Type::from_layout(TypeLayout::float())
    }
    pub const fn floatn(x: u32) -> Type {
        Type::from_layout(TypeLayout::floatn(x))
    }
    pub const fn double() -> Type {
        Type::from_layout(TypeLayout::double())
    }
    pub const fn float4x4() -> Type {
        Type::from_layout(TypeLayout::float4x4())
    }
    pub fn custom(name: Located<&str>) -> Type {
        Type::from_layout(TypeLayout::custom(name))
    }
    pub fn custom_templated(name: Located<&str>, types: Vec<Located<Type>>) -> Type {
        Type::from_layout(TypeLayout::custom_templated(name, types))
    }
}

impl TypeLayout {
    pub const fn from_scalar(scalar: ScalarType) -> TypeLayout {
        TypeLayout::Scalar(scalar)
    }
    pub const fn from_vector(scalar: ScalarType, x: u32) -> TypeLayout {
        TypeLayout::Vector(scalar, x)
    }

    pub const fn uint() -> TypeLayout {
        TypeLayout::from_scalar(ScalarType::UInt)
    }
    pub const fn int() -> TypeLayout {
        TypeLayout::from_scalar(ScalarType::Int)
    }
    pub const fn long() -> TypeLayout {
        TypeLayout::from_scalar(ScalarType::Int)
    }
    pub const fn float() -> TypeLayout {
        TypeLayout::from_scalar(ScalarType::Float)
    }
    pub const fn floatn(x: u32) -> TypeLayout {
        TypeLayout::from_vector(ScalarType::Float, x)
    }
    pub const fn double() -> TypeLayout {
        TypeLayout::from_scalar(ScalarType::Double)
    }
    pub const fn float4x4() -> TypeLayout {
        TypeLayout::Matrix(ScalarType::Float, 4, 4)
    }
    pub fn custom(name: Located<&str>) -> TypeLayout {
        TypeLayout::Custom(Box::new(name.into()), Vec::new())
    }
    pub fn custom_templated(name: Located<&str>, types: Vec<Located<Type>>) -> TypeLayout {
        TypeLayout::Custom(Box::new(name.into()), types)
    }
}

impl std::fmt::Debug for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{:?}{:?}", self.1, self.0)
    }
}

impl std::fmt::Debug for TypeLayout {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            TypeLayout::Void => write!(f, "void"),
            TypeLayout::Scalar(st) => write!(f, "{:?}", st),
            TypeLayout::Vector(st, x) => write!(f, "{:?}{}", st, x),
            TypeLayout::Matrix(st, x, y) => write!(f, "{:?}{}x{}", st, x, y),
            TypeLayout::Custom(s, args) => {
                write!(f, "{}", s)?;
                if let Some((last, non_last)) = args.split_last() {
                    write!(f, "<")?;
                    for arg in non_last {
                        write!(f, "{:?}, ", arg)?;
                    }
                    write!(f, "{:?}", last)?;
                    write!(f, ">")?;
                }
                Ok(())
            }
        }
    }
}

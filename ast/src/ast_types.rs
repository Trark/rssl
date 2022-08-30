use crate::primitive_types::*;
use rssl_text::Located;

#[derive(PartialEq, Clone)]
pub struct Type(pub TypeLayout, pub TypeModifier);

#[derive(PartialEq, Clone)]
pub enum TypeLayout {
    Void,
    Scalar(ScalarType),
    Vector(ScalarType, u32),
    Matrix(ScalarType, u32, u32),
    Custom(String, Vec<Located<Type>>),
}

/// A type that can be used in structured buffers
/// These are the both struct defined types, the format data types
#[derive(PartialEq, Debug, Clone)]
pub struct StructuredType(pub StructuredLayout, pub TypeModifier);

#[derive(PartialEq, Debug, Clone)]
pub enum StructuredLayout {
    Scalar(ScalarType),
    Vector(ScalarType, u32),
    Matrix(ScalarType, u32, u32),
    Custom(String), // Struct + User defined types
}

/// Template parameters
#[derive(PartialEq, Debug, Clone)]
pub struct TemplateParamList(pub Vec<Located<String>>);

impl From<DataType> for Type {
    fn from(ty: DataType) -> Type {
        let DataType(layout, modifier) = ty;
        Type(layout.into(), modifier)
    }
}

impl From<StructuredType> for Type {
    fn from(ty: StructuredType) -> Type {
        let StructuredType(layout, modifier) = ty;
        Type(layout.into(), modifier)
    }
}

impl From<DataLayout> for TypeLayout {
    fn from(data: DataLayout) -> TypeLayout {
        match data {
            DataLayout::Scalar(scalar) => TypeLayout::Scalar(scalar),
            DataLayout::Vector(scalar, x) => TypeLayout::Vector(scalar, x),
            DataLayout::Matrix(scalar, x, y) => TypeLayout::Matrix(scalar, x, y),
        }
    }
}

impl From<StructuredLayout> for TypeLayout {
    fn from(structured: StructuredLayout) -> TypeLayout {
        match structured {
            StructuredLayout::Scalar(scalar) => TypeLayout::Scalar(scalar),
            StructuredLayout::Vector(scalar, x) => TypeLayout::Vector(scalar, x),
            StructuredLayout::Matrix(scalar, x, y) => TypeLayout::Matrix(scalar, x, y),
            StructuredLayout::Custom(name) => TypeLayout::Custom(name, Vec::new()),
        }
    }
}

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
    pub fn custom(name: &str) -> Type {
        Type::from_layout(TypeLayout::custom(name))
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
    pub fn custom(name: &str) -> TypeLayout {
        TypeLayout::Custom(name.to_string(), Vec::new())
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

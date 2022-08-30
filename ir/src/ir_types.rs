use crate::*;

/// The full type when paired with modifiers
#[derive(PartialEq, Clone)]
pub struct Type(pub TypeLayout, pub TypeModifier);

#[derive(PartialEq, Clone)]
pub enum TypeLayout {
    Void,
    Scalar(ScalarType),
    Vector(ScalarType, u32),
    Matrix(ScalarType, u32, u32),
    Struct(StructId),
    SamplerState,
    Object(ObjectType),
    Array(Box<TypeLayout>, u64),
    TemplateParam(TemplateTypeId),
}

/// A type that can be used in structured buffers
/// These are the both all the data types and user defined structs
#[derive(PartialEq, Clone)]
pub struct StructuredType(pub StructuredLayout, pub TypeModifier);

/// Layout for StructuredType
#[derive(PartialEq, Clone)]
pub enum StructuredLayout {
    Scalar(ScalarType),
    Vector(ScalarType, u32),
    Matrix(ScalarType, u32, u32),
    Struct(StructId),
}

/// Id to a user defined struct
#[derive(PartialEq, Eq, Hash, PartialOrd, Ord, Debug, Clone, Copy)]
pub struct StructId(pub u32);

/// Id to a template type argument
#[derive(PartialEq, Eq, Hash, PartialOrd, Ord, Debug, Clone, Copy)]
pub struct TemplateTypeId(pub u32);

#[derive(PartialEq, Clone)]
pub enum ObjectType {
    Buffer(DataType),
    RWBuffer(DataType),

    ByteAddressBuffer,
    RWByteAddressBuffer,

    StructuredBuffer(StructuredType),
    RWStructuredBuffer(StructuredType),
    AppendStructuredBuffer(StructuredType),
    ConsumeStructuredBuffer(StructuredType),

    Texture1D(DataType),
    Texture1DArray(DataType),
    Texture2D(DataType),
    Texture2DArray(DataType),
    Texture2DMS(DataType),
    Texture2DMSArray(DataType),
    Texture3D(DataType),
    TextureCube(DataType),
    TextureCubeArray(DataType),
    RWTexture1D(DataType),
    RWTexture1DArray(DataType),
    RWTexture2D(DataType),
    RWTexture2DArray(DataType),
    RWTexture3D(DataType),

    ConstantBuffer(StructuredType),

    InputPatch,
    OutputPatch,
}

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
            StructuredLayout::Struct(id) => TypeLayout::Struct(id),
        }
    }
}

impl From<Type> for Option<DataType> {
    fn from(ty: Type) -> Option<DataType> {
        let Type(tyl, ty_mod) = ty;
        Option::<DataLayout>::from(tyl).map(|dtyl| DataType(dtyl, ty_mod))
    }
}

impl From<TypeLayout> for Option<DataLayout> {
    fn from(ty: TypeLayout) -> Option<DataLayout> {
        match ty {
            TypeLayout::Scalar(scalar) => Some(DataLayout::Scalar(scalar)),
            TypeLayout::Vector(scalar, x) => Some(DataLayout::Vector(scalar, x)),
            TypeLayout::Matrix(scalar, x, y) => Some(DataLayout::Matrix(scalar, x, y)),
            _ => None,
        }
    }
}

impl Type {
    pub fn void() -> Type {
        Type(TypeLayout::Void, TypeModifier::new())
    }
    pub fn from_layout(layout_type: TypeLayout) -> Type {
        Type(layout_type, TypeModifier::new())
    }
    pub fn from_scalar(scalar: ScalarType) -> Type {
        Type(TypeLayout::from_scalar(scalar), TypeModifier::new())
    }
    pub fn from_vector(scalar: ScalarType, x: u32) -> Type {
        Type(TypeLayout::from_vector(scalar, x), TypeModifier::new())
    }
    pub fn from_matrix(scalar: ScalarType, x: u32, y: u32) -> Type {
        Type(TypeLayout::from_matrix(scalar, x, y), TypeModifier::new())
    }
    pub fn from_data(DataType(tyl, tym): DataType) -> Type {
        Type(TypeLayout::from_data(tyl), tym)
    }
    pub fn from_struct(id: StructId) -> Type {
        Type(TypeLayout::from_struct(id), TypeModifier::new())
    }
    pub fn from_structured(StructuredType(tyl, tym): StructuredType) -> Type {
        Type(TypeLayout::from_structured(tyl), tym)
    }
    pub fn from_object(ty: ObjectType) -> Type {
        Type(TypeLayout::from_object(ty), TypeModifier::new())
    }

    pub fn bool() -> Type {
        Type::from_scalar(ScalarType::Bool)
    }
    pub fn booln(dim: u32) -> Type {
        Type::from_vector(ScalarType::Bool, dim)
    }
    pub fn uint() -> Type {
        Type::from_scalar(ScalarType::UInt)
    }
    pub fn uintn(dim: u32) -> Type {
        Type::from_vector(ScalarType::UInt, dim)
    }
    pub fn int() -> Type {
        Type::from_scalar(ScalarType::Int)
    }
    pub fn intn(dim: u32) -> Type {
        Type::from_vector(ScalarType::Int, dim)
    }
    pub fn float() -> Type {
        Type::from_scalar(ScalarType::Float)
    }
    pub fn floatn(dim: u32) -> Type {
        Type::from_vector(ScalarType::Float, dim)
    }
    pub fn double() -> Type {
        Type::from_scalar(ScalarType::Double)
    }
    pub fn doublen(dim: u32) -> Type {
        Type::from_vector(ScalarType::Double, dim)
    }

    pub fn long() -> Type {
        Type::from_scalar(ScalarType::Int)
    }
    pub fn float4x4() -> Type {
        Type::from_matrix(ScalarType::Float, 4, 4)
    }

    pub fn transform_scalar(self, to_scalar: ScalarType) -> Type {
        let Type(tyl, ty_mod) = self;
        Type(tyl.transform_scalar(to_scalar), ty_mod)
    }

    pub fn is_array(&self) -> bool {
        self.0.is_array()
    }

    pub fn is_void(&self) -> bool {
        self.0 == TypeLayout::Void
    }

    pub fn is_const(&self) -> bool {
        self.1.is_const
    }
}

impl TypeLayout {
    pub const fn from_scalar(scalar: ScalarType) -> TypeLayout {
        TypeLayout::Scalar(scalar)
    }
    pub const fn from_vector(scalar: ScalarType, x: u32) -> TypeLayout {
        TypeLayout::Vector(scalar, x)
    }
    pub const fn from_matrix(scalar: ScalarType, x: u32, y: u32) -> TypeLayout {
        TypeLayout::Matrix(scalar, x, y)
    }
    pub fn from_data(ty: DataLayout) -> TypeLayout {
        TypeLayout::from(ty)
    }
    pub const fn from_struct(id: StructId) -> TypeLayout {
        TypeLayout::Struct(id)
    }
    pub fn from_structured(ty: StructuredLayout) -> TypeLayout {
        TypeLayout::from(ty)
    }
    pub const fn from_object(ty: ObjectType) -> TypeLayout {
        TypeLayout::Object(ty)
    }
    pub const fn to_scalar(&self) -> Option<ScalarType> {
        match *self {
            TypeLayout::Scalar(scalar)
            | TypeLayout::Vector(scalar, _)
            | TypeLayout::Matrix(scalar, _, _) => Some(scalar),
            _ => None,
        }
    }
    pub const fn to_x(&self) -> Option<u32> {
        match *self {
            TypeLayout::Vector(_, ref x) => Some(*x),
            TypeLayout::Matrix(_, ref x, _) => Some(*x),
            _ => None,
        }
    }
    pub const fn to_y(&self) -> Option<u32> {
        match *self {
            TypeLayout::Matrix(_, _, ref y) => Some(*y),
            _ => None,
        }
    }
    pub fn max_dim(r1: Option<u32>, r2: Option<u32>) -> Option<u32> {
        use std::cmp::max;
        match (r1, r2) {
            (Some(x1), Some(x2)) => Some(max(x1, x2)),
            (Some(x1), None) => Some(x1),
            (None, Some(x2)) => Some(x2),
            (None, None) => None,
        }
    }
    pub const fn get_num_elements(&self) -> u32 {
        match (self.to_x(), self.to_y()) {
            (Some(x1), Some(x2)) => x1 * x2,
            (Some(x1), None) => x1,
            (None, Some(x2)) => x2,
            (None, None) => 1,
        }
    }
    pub const fn from_numeric(
        scalar: ScalarType,
        x_opt: Option<u32>,
        y_opt: Option<u32>,
    ) -> TypeLayout {
        match (x_opt, y_opt) {
            (Some(x), Some(y)) => TypeLayout::Matrix(scalar, x, y),
            (Some(x), None) => TypeLayout::Vector(scalar, x),
            (None, None) => TypeLayout::Scalar(scalar),
            _ => panic!("invalid numeric type"),
        }
    }

    /// Replaces the scalar type inside a numeric type with the given scalar type
    pub fn transform_scalar(self, to_scalar: ScalarType) -> TypeLayout {
        match self {
            TypeLayout::Scalar(_) => TypeLayout::Scalar(to_scalar),
            TypeLayout::Vector(_, x) => TypeLayout::Vector(to_scalar, x),
            TypeLayout::Matrix(_, x, y) => TypeLayout::Matrix(to_scalar, x, y),
            _ => panic!("non-numeric type in TypeLayout::transform_scalar"),
        }
    }

    pub fn is_array(&self) -> bool {
        matches!(self, &TypeLayout::Array(_, _))
    }
}

impl StructuredType {
    pub const fn as_const(self) -> StructuredType {
        let StructuredType(layout, mut modifier) = self;
        modifier.is_const = true;
        StructuredType(layout, modifier)
    }
}

impl std::fmt::Debug for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{:?}{:?}", self.1, self.0)
    }
}

impl std::fmt::Debug for TypeLayout {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match *self {
            TypeLayout::Void => write!(f, "void"),
            TypeLayout::Scalar(ref st) => write!(f, "{:?}", st),
            TypeLayout::Vector(ref st, ref x) => write!(f, "{:?}{}", st, x),
            TypeLayout::Matrix(ref st, ref x, ref y) => write!(f, "{:?}{}x{}", st, x, y),
            TypeLayout::Struct(ref sid) => write!(f, "struct<{}>", sid.0),
            TypeLayout::SamplerState => write!(f, "SamplerState"),
            TypeLayout::Object(ref ot) => write!(f, "{:?}", ot),
            TypeLayout::Array(ref ty, ref len) => write!(f, "{:?}[{}]", ty, len),
            TypeLayout::TemplateParam(ref id) => write!(f, "typename<{}>", id.0),
        }
    }
}

impl std::fmt::Debug for StructuredType {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{:?}{:?}", self.1, self.0)
    }
}

impl std::fmt::Debug for StructuredLayout {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match *self {
            StructuredLayout::Scalar(ref st) => write!(f, "{:?}", st),
            StructuredLayout::Vector(ref st, ref x) => write!(f, "{:?}{}", st, x),
            StructuredLayout::Matrix(ref st, ref x, ref y) => write!(f, "{:?}{}x{}", st, x, y),
            StructuredLayout::Struct(ref sid) => write!(f, "struct<{}>", sid.0),
        }
    }
}

impl std::fmt::Debug for ObjectType {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        use self::ObjectType::*;
        match *self {
            Buffer(ref dt) => write!(f, "Buffer<{:?}>", dt),
            RWBuffer(ref dt) => write!(f, "RWBuffer<{:?}>", dt),
            ByteAddressBuffer => write!(f, "ByteAddressBuffer"),
            RWByteAddressBuffer => write!(f, "RWByteAddressBuffer"),
            StructuredBuffer(ref st) => write!(f, "StructuredBuffer<{:?}>", st),
            RWStructuredBuffer(ref st) => write!(f, "RWStructuredBuffer<{:?}>", st),
            AppendStructuredBuffer(ref st) => write!(f, "AppendStructuredBuffer<{:?}>", st),
            ConsumeStructuredBuffer(ref st) => write!(f, "ConsumeStructuredBuffer<{:?}>", st),
            Texture1D(ref dt) => write!(f, "Texture1D<{:?}>", dt),
            Texture1DArray(ref dt) => write!(f, "Texture1DArray<{:?}>", dt),
            Texture2D(ref dt) => write!(f, "Texture2D<{:?}>", dt),
            Texture2DArray(ref dt) => write!(f, "Texture2DArray<{:?}>", dt),
            Texture2DMS(ref dt) => write!(f, "Texture2DMS<{:?}>", dt),
            Texture2DMSArray(ref dt) => write!(f, "Texture2DMSArray<{:?}>", dt),
            Texture3D(ref dt) => write!(f, "Texture3D<{:?}>", dt),
            TextureCube(ref dt) => write!(f, "TextureCube<{:?}>", dt),
            TextureCubeArray(ref dt) => write!(f, "TextureCubeArray<{:?}>", dt),
            RWTexture1D(ref dt) => write!(f, "RWTexture1D<{:?}>", dt),
            RWTexture1DArray(ref dt) => write!(f, "RWTexture1DArray<{:?}>", dt),
            RWTexture2D(ref dt) => write!(f, "RWTexture2D<{:?}>", dt),
            RWTexture2DArray(ref dt) => write!(f, "RWTexture2DArray<{:?}>", dt),
            RWTexture3D(ref dt) => write!(f, "RWTexture3D<{:?}>", dt),
            ConstantBuffer(ref st) => write!(f, "ConstantBuffer<{:?}>", st),
            InputPatch => write!(f, "InputPatch"),
            OutputPatch => write!(f, "OutputPatch"),
        }
    }
}

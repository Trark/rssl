use crate::*;
use rssl_text::Located;

/// Id to a type definition
#[derive(PartialEq, Eq, Hash, PartialOrd, Ord, Debug, Clone, Copy)]
pub struct TypeId(pub u32);

/// Id to an intrinsic object definition
#[derive(PartialEq, Eq, Hash, PartialOrd, Ord, Debug, Clone, Copy)]
pub struct ObjectId(pub u32);

/// Container of all registered types
#[derive(PartialEq, Eq, Clone, Default, Debug)]
pub struct TypeRegistry {
    /// Full type tree information for each type id
    layouts: Vec<TypeLayout>,

    /// Direct type information for each type id
    layers: Vec<TypeLayer>,

    /// Layout information for a registered object type
    object_layouts: Vec<ObjectType>,

    /// Functions for a registered object type
    object_functions: Vec<Vec<FunctionId>>,
}

/// The description of a type represented by a type id.
/// This is a single layer of the definition which links to the next type id for complex types.
#[derive(PartialEq, Eq, Hash, Copy, Clone)]
pub enum TypeLayer {
    Void,
    Scalar(ScalarType),
    Vector(ScalarType, u32),
    Matrix(ScalarType, u32, u32),
    Struct(StructId),
    StructTemplate(StructTemplateId),
    Object(ObjectType),
    Array(TypeId, u64),
    TemplateParam(TemplateTypeId),
    Modifier(TypeModifier, TypeId),
}

/// A type description
#[derive(PartialEq, Eq, Hash, Clone)]
pub enum TypeLayout {
    Void,
    Scalar(ScalarType),
    Vector(ScalarType, u32),
    Matrix(ScalarType, u32, u32),
    Struct(StructId),
    StructTemplate(StructTemplateId),
    Object(ObjectType),
    Array(Box<TypeLayout>, u64),
    TemplateParam(TemplateTypeId),
    Modifier(TypeModifier, Box<TypeLayout>),
}

impl TypeRegistry {
    /// Get or create the type id from a type layout
    pub fn register_type(&mut self, type_layout: TypeLayout) -> TypeId {
        let layer = match type_layout {
            TypeLayout::Void => TypeLayer::Void,
            TypeLayout::Scalar(st) => TypeLayer::Scalar(st),
            TypeLayout::Vector(st, x) => TypeLayer::Vector(st, x),
            TypeLayout::Matrix(st, x, y) => TypeLayer::Matrix(st, x, y),
            TypeLayout::Struct(id) => TypeLayer::Struct(id),
            TypeLayout::StructTemplate(id) => TypeLayer::StructTemplate(id),
            // TODO: Deal with recursive StructuredType/DataType
            TypeLayout::Object(ot) => TypeLayer::Object(ot),
            TypeLayout::Array(inner, len) => {
                TypeLayer::Array(self.register_type((*inner).clone()), len)
            }
            TypeLayout::TemplateParam(id) => TypeLayer::TemplateParam(id),
            TypeLayout::Modifier(modifier, inner) => {
                assert!(!matches!(*inner, TypeLayout::Modifier(_, _)));
                TypeLayer::Modifier(modifier, self.register_type((*inner).clone()))
            }
        };

        self.register_type_layer(layer)
    }

    /// Get or create the type id from a type layer
    pub fn register_type_layer(&mut self, layer: TypeLayer) -> TypeId {
        // Search for an existing registration of the type
        for (i, existing) in self.layers.iter().enumerate() {
            if layer == *existing {
                return TypeId(i as u32);
            }
        }

        let full_layout = match &layer {
            TypeLayer::Void => TypeLayout::Void,
            TypeLayer::Scalar(st) => TypeLayout::Scalar(*st),
            TypeLayer::Vector(st, x) => TypeLayout::Vector(*st, *x),
            TypeLayer::Matrix(st, x, y) => TypeLayout::Matrix(*st, *x, *y),
            TypeLayer::Struct(id) => TypeLayout::Struct(*id),
            TypeLayer::StructTemplate(id) => TypeLayout::StructTemplate(*id),
            // TODO: Deal with recursive StructuredType/DataType
            TypeLayer::Object(ot) => TypeLayout::Object(*ot),
            TypeLayer::Array(inner, len) => {
                TypeLayout::Array(Box::new(self.get_type_layout(*inner).clone()), *len)
            }
            TypeLayer::TemplateParam(id) => TypeLayout::TemplateParam(*id),
            TypeLayer::Modifier(modifier, inner) => {
                let layout = self.get_type_layout(*inner).clone();
                assert!(!matches!(layout, TypeLayout::Modifier(_, _)));
                TypeLayout::Modifier(*modifier, Box::new(layout))
            }
        };

        // Make a new entry
        let id = TypeId(self.layouts.len() as u32);
        self.layouts.push(full_layout);
        self.layers.push(layer);
        id
    }

    /// Get the type layout for a type id
    pub fn get_type_layout(&self, id: TypeId) -> &TypeLayout {
        &self.layouts[id.0 as usize]
    }

    /// Get the top type layer for a type id
    pub fn get_type_layer(&self, id: TypeId) -> TypeLayer {
        self.layers[id.0 as usize]
    }

    /// Get the object type layout for an object id
    pub fn get_object_layout(&self, id: ObjectId) -> ObjectType {
        self.object_layouts[id.0 as usize]
    }

    /// Get the intrinsic functions for an object id
    pub fn get_object_functions(&self, id: ObjectId) -> &Vec<FunctionId> {
        &self.object_functions[id.0 as usize]
    }

    /// Get the base type and type modifier from a type
    pub fn extract_modifier(&self, id: TypeId) -> (TypeId, TypeModifier) {
        match self.layers[id.0 as usize] {
            TypeLayer::Modifier(modifier, inner) => (inner, modifier),
            _ => (id, TypeModifier::default()),
        }
    }

    /// Get the base type without a type modifier from a type
    pub fn remove_modifier(&self, id: TypeId) -> TypeId {
        self.extract_modifier(id).0
    }

    /// Add a modifier onto a type
    ///
    /// This expects there to not already be a modifier on the type
    pub fn combine_modifier(&mut self, id: TypeId, modifier: TypeModifier) -> TypeId {
        assert!(!matches!(
            self.layers[id.0 as usize],
            TypeLayer::Modifier(_, _)
        ));
        if modifier == TypeModifier::default() {
            id
        } else {
            self.register_type_layer(TypeLayer::Modifier(modifier, id))
        }
    }

    /// Add the const modifier to a type
    pub fn make_const(&mut self, id: TypeId) -> TypeId {
        let (base, mut modifier) = self.extract_modifier(id);
        if modifier.is_const {
            id
        } else {
            modifier.is_const = true;
            self.register_type_layer(TypeLayer::Modifier(modifier, base))
        }
    }
}

impl Module {
    /// Get or create the object id from an object type
    pub fn register_object(&mut self, object_type: ObjectType) -> ObjectId {
        // Search for an existing registration of the object
        for (i, existing) in self.type_registry.object_layouts.iter().enumerate() {
            if object_type == *existing {
                return ObjectId(i as u32);
            }
        }

        // Make a new entry
        let id = ObjectId(self.type_registry.object_layouts.len() as u32);
        self.type_registry.object_layouts.push(object_type);

        // Gather the intrinsic functions
        let mut functions = Vec::new();
        for def in intrinsic_data::get_methods(self, object_type) {
            // Register the function
            let func_id = self.function_registry.register_function(
                FunctionNameDefinition {
                    name: Located::none(def.name.clone()),
                    full_name: ScopedName::unscoped(def.name),
                },
                def.signature,
            );

            // Set the intrinsic that is represented by this function registration
            self.function_registry
                .set_intrinsic_data(func_id, def.intrinsic);

            // Add it to the list of functions for this type
            functions.push(func_id);
        }

        // Set the list of functions for this type
        self.type_registry.object_functions.push(functions);

        id
    }
}

/// Basic scalar types
#[derive(PartialEq, Eq, Hash, Copy, Clone)]
pub enum ScalarType {
    Bool,
    UntypedInt,
    Int,
    UInt,
    Half,
    Float,
    Double,
}

/// The dimensions of a scalar, vector or matrix data type
#[derive(PartialEq, Eq, Debug, Copy, Clone)]
pub enum NumericDimension {
    Scalar,
    Vector(u32),
    Matrix(u32, u32),
}

/// A type that can be used in structured buffers
/// These are the both all the data types and user defined structs
#[derive(PartialEq, Eq, Hash, Copy, Clone)]
pub struct StructuredType(pub StructuredLayout, pub TypeModifier);

/// Layout for StructuredType
#[derive(PartialEq, Eq, Hash, Copy, Clone)]
pub enum StructuredLayout {
    Scalar(ScalarType),
    Vector(ScalarType, u32),
    Matrix(ScalarType, u32, u32),
    Struct(StructId),
}

/// A type that can be used in data buffers (Buffer / RWBuffer / etc)
#[derive(PartialEq, Eq, Hash, Copy, Clone)]
pub struct DataType(pub DataLayout, pub TypeModifier);

/// The memory layout of a DataType
#[derive(PartialEq, Eq, Hash, Copy, Clone)]
pub enum DataLayout {
    Scalar(ScalarType),
    Vector(ScalarType, u32),
    Matrix(ScalarType, u32, u32),
}

/// Id to a user defined struct
#[derive(PartialEq, Eq, Hash, PartialOrd, Ord, Debug, Clone, Copy)]
pub struct StructId(pub u32);

/// Id to a user defined struct template
#[derive(PartialEq, Eq, Hash, PartialOrd, Ord, Debug, Clone, Copy)]
pub struct StructTemplateId(pub u32);

/// Id to a template type argument
#[derive(PartialEq, Eq, Hash, PartialOrd, Ord, Debug, Clone, Copy)]
pub struct TemplateTypeId(pub u32);

/// Number of template arguments to a template
#[derive(PartialEq, Eq, Hash, PartialOrd, Ord, Debug, Clone, Copy)]
pub struct TemplateParamCount(pub u32);

/// A built in object type
#[derive(PartialEq, Eq, Hash, Copy, Clone)]
pub enum ObjectType {
    Buffer(TypeId),
    RWBuffer(TypeId),

    ByteAddressBuffer,
    RWByteAddressBuffer,

    BufferAddress,
    RWBufferAddress,

    StructuredBuffer(TypeId),
    RWStructuredBuffer(TypeId),

    Texture2D(TypeId),
    RWTexture2D(TypeId),

    ConstantBuffer(TypeId),

    SamplerState,
}

/// A constant value
#[derive(PartialEq, Eq, Hash, Clone, Debug)]
pub enum Constant {
    Bool(bool),
    UntypedInt(u64),
    Int(u64),
    UInt(u64),
}

/// Either a type or a constant
#[derive(PartialEq, Eq, Hash, Clone, Debug)]
pub enum TypeOrConstant {
    /// Value is a type
    Type(TypeId),

    /// Value is a constant
    Constant(Constant),
}

impl TypeId {
    /// Turn the type into an [ExpressionType] as an rvalue
    pub fn to_rvalue(self) -> ExpressionType {
        ExpressionType(self, ValueType::Rvalue)
    }

    /// Turn the type into an [ExpressionType] as an lvalue
    pub fn to_lvalue(self) -> ExpressionType {
        ExpressionType(self, ValueType::Lvalue)
    }
}

impl From<DataType> for TypeLayout {
    fn from(ty: DataType) -> TypeLayout {
        let DataType(layout, modifier) = ty;
        TypeLayout::from(layout).combine_modifier(modifier)
    }
}

impl From<StructuredType> for TypeLayout {
    fn from(ty: StructuredType) -> TypeLayout {
        let StructuredType(layout, modifier) = ty;
        TypeLayout::from(layout).combine_modifier(modifier)
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

impl From<TypeLayout> for Option<DataType> {
    fn from(ty: TypeLayout) -> Option<DataType> {
        let (tyl, ty_mod) = ty.extract_modifier();
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

impl TypeLayout {
    pub const fn void() -> Self {
        TypeLayout::Void
    }

    pub const fn from_scalar(scalar: ScalarType) -> Self {
        TypeLayout::Scalar(scalar)
    }

    pub const fn from_vector(scalar: ScalarType, x: u32) -> Self {
        TypeLayout::Vector(scalar, x)
    }

    pub const fn from_matrix(scalar: ScalarType, x: u32, y: u32) -> Self {
        TypeLayout::Matrix(scalar, x, y)
    }

    pub const fn from_struct(id: StructId) -> Self {
        TypeLayout::Struct(id)
    }

    pub const fn from_object(ty: ObjectType) -> Self {
        TypeLayout::Object(ty)
    }

    pub const fn bool() -> Self {
        Self::from_scalar(ScalarType::Bool)
    }

    pub const fn booln(dim: u32) -> Self {
        Self::from_vector(ScalarType::Bool, dim)
    }

    pub const fn uint() -> Self {
        Self::from_scalar(ScalarType::UInt)
    }

    pub const fn uintn(dim: u32) -> Self {
        Self::from_vector(ScalarType::UInt, dim)
    }

    pub const fn int() -> Self {
        Self::from_scalar(ScalarType::Int)
    }

    pub const fn intn(dim: u32) -> Self {
        Self::from_vector(ScalarType::Int, dim)
    }

    pub const fn float() -> Self {
        Self::from_scalar(ScalarType::Float)
    }

    pub const fn floatn(dim: u32) -> Self {
        Self::from_vector(ScalarType::Float, dim)
    }

    pub const fn double() -> Self {
        Self::from_scalar(ScalarType::Double)
    }

    pub const fn doublen(dim: u32) -> Self {
        Self::from_vector(ScalarType::Double, dim)
    }

    pub const fn long() -> Self {
        Self::from_scalar(ScalarType::Int)
    }

    pub const fn float4x4() -> Self {
        Self::from_matrix(ScalarType::Float, 4, 4)
    }

    pub fn to_scalar(&self) -> Option<ScalarType> {
        assert!(!self.has_modifiers());
        match *self {
            TypeLayout::Scalar(scalar)
            | TypeLayout::Vector(scalar, _)
            | TypeLayout::Matrix(scalar, _, _) => Some(scalar),
            _ => None,
        }
    }

    pub fn to_x(&self) -> Option<u32> {
        assert!(!self.has_modifiers());
        match *self {
            TypeLayout::Vector(_, ref x) => Some(*x),
            TypeLayout::Matrix(_, ref x, _) => Some(*x),
            _ => None,
        }
    }

    pub fn to_y(&self) -> Option<u32> {
        assert!(!self.has_modifiers());
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

    /// Get the most significant type from two data types
    pub fn most_significant_data_type(left: &Self, right: &Self) -> Self {
        let (left_ty, left_mod) = left.clone().extract_modifier();
        let (right_ty, _) = right.clone().extract_modifier();

        // Get the more important input type, that serves as the base to
        // calculate the output type from
        let nd = match Self::most_significant_dimension(&left_ty, &right_ty) {
            Some(nd) => nd,
            None => panic!("non-arithmetic numeric type in binary operation"),
        };

        let st = left_ty.to_scalar().unwrap();
        assert_eq!(st, right.to_scalar().unwrap());
        Self::from(DataLayout::new(st, nd)).combine_modifier(left_mod)
    }

    /// Attempt to get the most significant dimension of two data types
    pub fn most_significant_dimension(lhs: &Self, rhs: &Self) -> Option<NumericDimension> {
        assert!(!lhs.has_modifiers());
        assert!(!rhs.has_modifiers());
        use std::cmp::max;
        use std::cmp::min;
        use TypeLayout::*;
        match (lhs, rhs) {
            (&Scalar(_), &Scalar(_)) => Some(NumericDimension::Scalar),
            (&Scalar(_), &Vector(_, ref x)) => Some(NumericDimension::Vector(*x)),
            (&Vector(_, ref x), &Scalar(_)) => Some(NumericDimension::Vector(*x)),
            (&Vector(_, ref x1), &Vector(_, ref x2)) if *x1 == 1 || *x2 == 1 => {
                Some(NumericDimension::Vector(max(*x1, *x2)))
            }
            (&Vector(_, ref x1), &Vector(_, ref x2)) => {
                let x = min(*x1, *x2);
                Some(NumericDimension::Vector(x))
            }
            (&Matrix(_, ref x1, ref y1), &Matrix(_, ref x2, ref y2)) => {
                let x = min(*x1, *x2);
                let y = min(*y1, *y2);
                Some(NumericDimension::Matrix(x, y))
            }
            _ => None,
        }
    }

    pub fn get_num_elements(&self) -> u32 {
        assert!(!self.has_modifiers());
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
        let (tyl, ty_mod) = self.extract_modifier();
        let tyl = match tyl {
            TypeLayout::Scalar(_) => TypeLayout::Scalar(to_scalar),
            TypeLayout::Vector(_, x) => TypeLayout::Vector(to_scalar, x),
            TypeLayout::Matrix(_, x, y) => TypeLayout::Matrix(to_scalar, x, y),
            _ => panic!("non-numeric type in TypeLayout::transform_scalar"),
        };
        tyl.combine_modifier(ty_mod)
    }

    /// Returns `true` if the type has modifiers
    pub fn has_modifiers(&self) -> bool {
        matches!(self, TypeLayout::Modifier(_, _))
    }

    /// Returns `true` if the type is the void type
    pub fn is_void(&self) -> bool {
        if let TypeLayout::Modifier(_, inner) = self {
            **inner == TypeLayout::Void
        } else {
            *self == TypeLayout::Void
        }
    }

    /// Returns `true` if the type has a const modifier
    pub fn is_const(&self) -> bool {
        if let TypeLayout::Modifier(modifier, _) = self {
            modifier.is_const
        } else {
            false
        }
    }

    /// Returns `true` if the type is an array
    pub fn is_array(&self) -> bool {
        assert!(!self.has_modifiers());
        matches!(self, &TypeLayout::Array(_, _))
    }

    /// Returns `true` if the type is an object
    pub fn is_object(&self) -> bool {
        assert!(!self.has_modifiers());
        matches!(self, &TypeLayout::Object(_))
    }

    /// Returns `true` if the type is a buffer address
    pub fn is_buffer_address(&self) -> bool {
        let ty = if let TypeLayout::Modifier(_, inner) = self {
            &**inner
        } else {
            self
        };

        matches!(
            ty,
            &TypeLayout::Object(ObjectType::BufferAddress)
                | &TypeLayout::Object(ObjectType::RWBufferAddress)
        )
    }

    /// Split the modifier out of the type
    pub fn extract_modifier(self) -> (Self, TypeModifier) {
        match self {
            TypeLayout::Modifier(modifier, tyl) => (*tyl, modifier),
            ty => (ty, TypeModifier::default()),
        }
    }

    /// Remove the modifier from the type
    pub fn remove_modifier(self) -> Self {
        self.extract_modifier().0
    }

    /// Recombine the modifier back onto the type
    ///
    /// This expects there to not already be a modifier
    pub fn combine_modifier(self, modifier: TypeModifier) -> Self {
        assert!(!self.has_modifiers());
        if modifier == TypeModifier::default() {
            self
        } else {
            TypeLayout::Modifier(modifier, Box::new(self))
        }
    }

    /// Add the const modifier
    pub fn make_const(mut self) -> Self {
        if let TypeLayout::Modifier(modifier, _) = &mut self {
            modifier.is_const = true;
            self
        } else {
            TypeLayout::Modifier(TypeModifier::const_only(), Box::new(self))
        }
    }

    /// Remove the const modifier
    pub fn remove_const(self) -> Self {
        if let TypeLayout::Modifier(mut modifier, inner) = self {
            modifier.is_const = false;
            if modifier == TypeModifier::default() {
                *inner
            } else {
                TypeLayout::Modifier(modifier, inner)
            }
        } else {
            self
        }
    }
}

impl StructuredType {
    pub const fn as_const(self) -> StructuredType {
        let StructuredType(layout, mut modifier) = self;
        modifier.is_const = true;
        StructuredType(layout, modifier)
    }
}

impl DataType {
    pub const fn as_const(self) -> DataType {
        let DataType(layout, mut modifier) = self;
        modifier.is_const = true;
        DataType(layout, modifier)
    }
}

impl DataLayout {
    /// Construct a data layout from a scalar type part and the dimension part
    pub const fn new(scalar: ScalarType, dim: NumericDimension) -> DataLayout {
        match dim {
            NumericDimension::Scalar => DataLayout::Scalar(scalar),
            NumericDimension::Vector(x) => DataLayout::Vector(scalar, x),
            NumericDimension::Matrix(x, y) => DataLayout::Matrix(scalar, x, y),
        }
    }

    /// Extract scalar type part
    pub const fn to_scalar(&self) -> ScalarType {
        match *self {
            DataLayout::Scalar(scalar)
            | DataLayout::Vector(scalar, _)
            | DataLayout::Matrix(scalar, _, _) => scalar,
        }
    }

    /// Replace scalar type part with another type
    pub const fn transform_scalar(self, to_scalar: ScalarType) -> DataLayout {
        match self {
            DataLayout::Scalar(_) => DataLayout::Scalar(to_scalar),
            DataLayout::Vector(_, x) => DataLayout::Vector(to_scalar, x),
            DataLayout::Matrix(_, x, y) => DataLayout::Matrix(to_scalar, x, y),
        }
    }
}

impl std::fmt::Debug for TypeLayer {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match *self {
            TypeLayer::Void => write!(f, "void"),
            TypeLayer::Scalar(ref st) => write!(f, "{:?}", st),
            TypeLayer::Vector(ref st, ref x) => write!(f, "{:?}{}", st, x),
            TypeLayer::Matrix(ref st, ref x, ref y) => write!(f, "{:?}{}x{}", st, x, y),
            TypeLayer::Struct(ref sid) => write!(f, "struct<{}>", sid.0),
            TypeLayer::StructTemplate(ref sid) => write!(f, "struct_template<{}>", sid.0),
            TypeLayer::Object(ref ot) => write!(f, "{:?}", ot),
            TypeLayer::Array(ref ty, ref len) => write!(f, "type<{:?}>[{}]", ty, len),
            TypeLayer::TemplateParam(ref id) => write!(f, "typename<{}>", id.0),
            TypeLayer::Modifier(ref modifier, ref ty) => write!(f, "{:?}type<{:?}>", modifier, ty),
        }
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
            TypeLayout::StructTemplate(ref sid) => write!(f, "struct_template<{}>", sid.0),
            TypeLayout::Object(ref ot) => write!(f, "{:?}", ot),
            TypeLayout::Array(ref ty, ref len) => write!(f, "{:?}[{}]", ty, len),
            TypeLayout::TemplateParam(ref id) => write!(f, "typename<{}>", id.0),
            TypeLayout::Modifier(ref modifier, ref ty) => write!(f, "{:?}{:?}", modifier, ty),
        }
    }
}

impl std::fmt::Debug for ScalarType {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            ScalarType::Bool => write!(f, "bool"),
            ScalarType::UntypedInt => write!(f, "integer"),
            ScalarType::Int => write!(f, "int"),
            ScalarType::UInt => write!(f, "uint"),
            ScalarType::Half => write!(f, "half"),
            ScalarType::Float => write!(f, "float"),
            ScalarType::Double => write!(f, "double"),
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

impl std::fmt::Debug for DataType {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{:?}{:?}", self.1, self.0)
    }
}

impl std::fmt::Debug for DataLayout {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match *self {
            DataLayout::Scalar(st) => write!(f, "{:?}", st),
            DataLayout::Vector(st, x) => write!(f, "{:?}{}", st, x),
            DataLayout::Matrix(st, x, y) => write!(f, "{:?}{}x{}", st, x, y),
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
            BufferAddress => write!(f, "BufferAddress"),
            RWBufferAddress => write!(f, "RWBufferAddress"),
            StructuredBuffer(ref st) => write!(f, "StructuredBuffer<{:?}>", st),
            RWStructuredBuffer(ref st) => write!(f, "RWStructuredBuffer<{:?}>", st),
            Texture2D(ref dt) => write!(f, "Texture2D<{:?}>", dt),
            RWTexture2D(ref dt) => write!(f, "RWTexture2D<{:?}>", dt),
            ConstantBuffer(ref st) => write!(f, "ConstantBuffer<{:?}>", st),
            SamplerState => write!(f, "SamplerState"),
        }
    }
}

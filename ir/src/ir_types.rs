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
            // TODO: Deal with recursive types in object type args
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
            // TODO: Deal with recursive types in object type args
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
    SamplerComparisonState,
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

impl TypeLayer {
    /// Parse numeric type from a string
    pub const fn from_numeric_str(typename: &str) -> Option<TypeLayer> {
        const fn digit(input: &[u8]) -> Option<(&[u8], u32)> {
            match input {
                [b'1', rest @ ..] => Some((rest, 1)),
                [b'2', rest @ ..] => Some((rest, 2)),
                [b'3', rest @ ..] => Some((rest, 3)),
                [b'4', rest @ ..] => Some((rest, 4)),
                _ => None,
            }
        }

        const fn parse_str(input: &[u8]) -> Option<(&[u8], TypeLayer)> {
            let (rest, ty) = match ScalarType::parse_str(input) {
                Some(ok) => ok,
                None => return None,
            };
            if rest.is_empty() {
                return Some((&[], TypeLayer::Scalar(ty)));
            }

            let (rest, x) = match digit(rest) {
                Some(ok) => ok,
                None => return None,
            };
            if rest.is_empty() {
                return Some((&[], TypeLayer::Vector(ty, x)));
            }

            let rest = match rest {
                [b'x', rest @ ..] => rest,
                _ => return None,
            };

            let (rest, y) = match digit(rest) {
                Some(ok) => ok,
                None => return None,
            };
            if rest.is_empty() {
                return Some((&[], TypeLayer::Matrix(ty, x, y)));
            }

            None
        }

        let type_name_bytes = typename.as_bytes();

        match parse_str(type_name_bytes) {
            Some((rest, ty)) => {
                if !rest.is_empty() {
                    panic!("from_numeric_str expects to finish the string`");
                }
                Some(ty)
            }
            None => None,
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
        Self::from_numeric_dimensions(st, nd).combine_modifier(left_mod)
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

    /// Get the total number of scalar elements in the type - or 1 for non-numeric types
    pub fn get_num_elements(&self) -> u32 {
        assert!(!self.has_modifiers());
        match (self.to_x(), self.to_y()) {
            (Some(x1), Some(x2)) => x1 * x2,
            (Some(x1), None) => x1,
            (None, Some(x2)) => x2,
            (None, None) => 1,
        }
    }

    /// Construct a type layout from a scalar type part and the dimension part
    pub const fn from_numeric_dimensions(scalar: ScalarType, dim: NumericDimension) -> Self {
        match dim {
            NumericDimension::Scalar => TypeLayout::Scalar(scalar),
            NumericDimension::Vector(x) => TypeLayout::Vector(scalar, x),
            NumericDimension::Matrix(x, y) => TypeLayout::Matrix(scalar, x, y),
        }
    }

    /// Construct a type layout from a scalar type part and the two optional dimension sizes
    pub const fn from_numeric_parts(
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

    /// Turn a [TypeLayer] for a numeric type into a [TypeLayout]
    pub const fn from_numeric_layer(layer: TypeLayer) -> Option<Self> {
        match layer {
            TypeLayer::Scalar(s) => Some(TypeLayout::Scalar(s)),
            TypeLayer::Vector(s, x) => Some(TypeLayout::Vector(s, x)),
            TypeLayer::Matrix(s, x, y) => Some(TypeLayout::Matrix(s, x, y)),
            _ => None,
        }
    }

    /// Turn a [TypeLayer] for a numeric type into a [TypeLayout]
    pub const fn from_numeric_layer_or_fail(layer: TypeLayer) -> Self {
        match layer {
            TypeLayer::Scalar(s) => TypeLayout::Scalar(s),
            TypeLayer::Vector(s, x) => TypeLayout::Vector(s, x),
            TypeLayer::Matrix(s, x, y) => TypeLayout::Matrix(s, x, y),
            _ => panic!(),
        }
    }
}

impl ScalarType {
    /// Parse scalar type as part of a string
    const fn parse_str(input: &[u8]) -> Option<(&[u8], ScalarType)> {
        match input {
            [b'b', b'o', b'o', b'l', rest @ ..] => Some((rest, ScalarType::Bool)),
            [b'i', b'n', b't', rest @ ..] => Some((rest, ScalarType::Int)),
            [b'u', b'i', b'n', b't', rest @ ..] => Some((rest, ScalarType::UInt)),
            [b'd', b'w', b'o', b'r', b'd', rest @ ..] => Some((rest, ScalarType::UInt)),
            [b'h', b'a', b'l', b'f', rest @ ..] => Some((rest, ScalarType::Half)),
            [b'f', b'l', b'o', b'a', b't', rest @ ..] => Some((rest, ScalarType::Float)),
            [b'd', b'o', b'u', b'b', b'l', b'e', rest @ ..] => Some((rest, ScalarType::Double)),
            _ => None,
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
            SamplerComparisonState => write!(f, "SamplerComparisonState"),
        }
    }
}

#[test]
fn test_parse_numeric_str() {
    assert_eq!(
        TypeLayer::from_numeric_str("float"),
        Some(TypeLayer::Scalar(ScalarType::Float))
    );
    assert_eq!(
        TypeLayer::from_numeric_str("uint3"),
        Some(TypeLayer::Vector(ScalarType::UInt, 3))
    );
    assert_eq!(
        TypeLayer::from_numeric_str("bool2x3"),
        Some(TypeLayer::Matrix(ScalarType::Bool, 2, 3))
    );

    assert_eq!(TypeLayer::from_numeric_str(""), None);
    assert_eq!(TypeLayer::from_numeric_str("float5"), None);
    assert_eq!(TypeLayer::from_numeric_str("float2x"), None);
}

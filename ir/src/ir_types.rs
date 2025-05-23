use crate::*;
use rssl_text::Located;
use std::cell::RefCell;

/// Id to a type definition
#[derive(PartialEq, Eq, Hash, PartialOrd, Ord, Debug, Clone, Copy)]
pub struct TypeId(pub u32);

/// Id to an intrinsic object definition
#[derive(PartialEq, Eq, Hash, PartialOrd, Ord, Debug, Clone, Copy)]
pub struct ObjectId(pub u32);

/// Container of all registered types
#[derive(PartialEq, Clone, Debug)]
pub struct TypeRegistry {
    /// Direct type information for each type id
    layers: RefCell<Vec<TypeLayer>>,

    /// Layout information for a registered object type
    object_layouts: Vec<ObjectType>,

    /// Functions for a registered object type
    object_functions: Vec<Vec<FunctionId>>,

    /// Template type argument definitions
    template_types: Vec<TemplateParamType>,
}

/// The description of a type represented by a type id.
/// This is a single layer of the definition which links to the next type id for complex types.
#[derive(PartialEq, Eq, Hash, Copy, Clone)]
pub enum TypeLayer {
    Void,
    Scalar(ScalarType),
    Vector(TypeId, u32),
    Matrix(TypeId, u32, u32),
    Struct(StructId),
    StructTemplate(StructTemplateId),
    Enum(EnumId),
    Object(ObjectType),
    Array(TypeId, Option<u64>),
    TemplateParam(TemplateTypeId),
    Modifier(TypeModifier, TypeId),
}

/// Template type parameter state
#[derive(PartialEq, Debug, Clone)]
pub struct TemplateParamType {
    /// Name for the template parameter
    pub name: Option<Located<String>>,

    /// Id of the type that represents this template type
    pub type_id: TypeId,

    /// Index into list of template arguments the parameter is contained in
    pub positional_index: u32,
}

impl TypeRegistry {
    /// Get or create the type id from a type layer
    pub fn register_type(&self, layer: TypeLayer) -> TypeId {
        // Search for an existing registration of the type
        if let Some(ty) = self.try_find_type(layer) {
            return ty;
        }

        match layer {
            TypeLayer::Vector(inner, _) => {
                if !matches!(
                    self.get_type_layer(inner),
                    TypeLayer::Scalar(_) | TypeLayer::TemplateParam(_)
                ) {
                    panic!("{:?} inside vector", self.get_type_layer(inner));
                }
            }
            TypeLayer::Matrix(inner, _, _) => {
                if !matches!(
                    self.get_type_layer(inner),
                    TypeLayer::Scalar(_) | TypeLayer::TemplateParam(_)
                ) {
                    panic!("{:?} inside matrix", self.get_type_layer(inner));
                }
            }
            TypeLayer::Modifier(modifier, inner) => {
                assert!(!self.get_type_layer(inner).is_modifier());
                assert!(modifier != TypeModifier::default());
            }
            _ => {}
        }

        // Make a new entry
        let mut layers = self.layers.borrow_mut();
        let id = TypeId(layers.len() as u32);
        layers.push(layer);
        id
    }

    /// Find an existing type id from a type layer
    pub fn try_find_type(&self, layer: TypeLayer) -> Option<TypeId> {
        // Search for an existing registration of the type
        for (i, existing) in self.layers.borrow().iter().enumerate() {
            if layer == *existing {
                return Some(TypeId(i as u32));
            }
        }
        None
    }

    /// Get or create the type id from a numeric type
    pub fn register_numeric_type(&self, numeric: NumericType) -> TypeId {
        let scalar_id = self.register_type(TypeLayer::Scalar(numeric.scalar));
        match numeric.dimension {
            NumericDimension::Scalar => scalar_id,
            NumericDimension::Vector(x) => self.register_type(TypeLayer::Vector(scalar_id, x)),
            NumericDimension::Matrix(x, y) => {
                self.register_type(TypeLayer::Matrix(scalar_id, x, y))
            }
        }
    }

    /// Register a new template type parameter with the module
    pub fn register_template_type(
        &mut self,
        name: Option<Located<String>>,
        positional_index: u32,
    ) -> TemplateTypeId {
        let id = TemplateTypeId(self.template_types.len() as u32);

        // Make a type id for the new template type id
        let type_id = self.register_type(TypeLayer::TemplateParam(id));

        self.template_types.push(TemplateParamType {
            name,
            type_id,
            positional_index,
        });

        id
    }

    /// Get the total number of registered types
    pub fn get_type_count(&self) -> u32 {
        self.layers.borrow().len() as u32
    }

    /// Iterate the set of valid type ids
    ///
    /// This does not take a reference to the registry so the caller may add new types when this is in progress
    pub fn iter(&self) -> TypeIdIterator {
        TypeIdIterator {
            current: 0,
            end: self.get_type_count(),
        }
    }

    /// Get the top type layer for a type id
    #[inline]
    pub fn get_type_layer(&self, id: TypeId) -> TypeLayer {
        self.layers.borrow()[id.0 as usize]
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
        match self.get_type_layer(id) {
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
    pub fn combine_modifier(&self, id: TypeId, modifier: TypeModifier) -> TypeId {
        assert!(!matches!(
            self.get_type_layer(id),
            TypeLayer::Modifier(_, _)
        ));
        if modifier == TypeModifier::default() {
            id
        } else {
            self.register_type(TypeLayer::Modifier(modifier, id))
        }
    }

    /// Add the const modifier to a type
    pub fn make_const(&self, id: TypeId) -> TypeId {
        let (base, mut modifier) = self.extract_modifier(id);
        if modifier.is_const {
            id
        } else {
            modifier.is_const = true;
            self.register_type(TypeLayer::Modifier(modifier, base))
        }
    }

    /// Get the scalar type from a numeric type
    pub fn extract_scalar(&self, id: TypeId) -> Option<ScalarType> {
        match self.get_type_layer(id) {
            TypeLayer::Scalar(scalar) => Some(scalar),
            TypeLayer::Vector(inner, _) => self.extract_scalar(inner),
            TypeLayer::Matrix(inner, _, _) => self.extract_scalar(inner),
            TypeLayer::Modifier(_, _) => {
                panic!("extract_scalar expects unmodified type")
            }
            _ => None,
        }
    }

    /// Get the id after removing vector / matrix modifiers
    pub fn get_non_vector_id(&self, id: TypeId) -> TypeId {
        match self.get_type_layer(id) {
            TypeLayer::Vector(inner, _) => self.get_non_vector_id(inner),
            TypeLayer::Matrix(inner, _, _) => self.get_non_vector_id(inner),
            _ => id,
        }
    }

    /// Get the layer after removing vector / matrix modifiers
    pub fn get_non_vector_layer(&self, id: TypeId) -> TypeLayer {
        let non_vector_id = self.get_non_vector_id(id);
        self.get_type_layer(non_vector_id)
    }

    /// Get the id after removing array modifiers
    pub fn get_non_array_id(&self, id: TypeId) -> TypeId {
        match self.get_type_layer(id) {
            TypeLayer::Array(inner, _) => self.get_non_array_id(inner),
            _ => id,
        }
    }

    /// Get the layer after removing array modifiers
    pub fn get_non_array_layer(&self, id: TypeId) -> TypeLayer {
        let non_array_id = self.get_non_array_id(id);
        self.get_type_layer(non_array_id)
    }

    /// Replaces the scalar type inside a numeric type with the given scalar type
    pub fn transform_scalar(&self, id: TypeId, to_scalar: ScalarType) -> TypeId {
        let (base_id, modifer) = self.extract_modifier(id);
        let scalar_id = self.register_type(TypeLayer::Scalar(to_scalar));
        let new_id = match self.get_type_layer(base_id) {
            TypeLayer::Scalar(_) => scalar_id,
            TypeLayer::Vector(_, x) => self.register_type(TypeLayer::Vector(scalar_id, x)),
            TypeLayer::Matrix(_, x, y) => self.register_type(TypeLayer::Matrix(scalar_id, x, y)),
            TypeLayer::Enum(_) => scalar_id,
            _ => panic!("non-numeric type in transform_scalar"),
        };
        self.combine_modifier(new_id, modifer)
    }

    /// Returns `true` if the type is the void type
    pub fn is_void(&self, id: TypeId) -> bool {
        let unmodified_id = self.remove_modifier(id);
        let tyl = self.get_type_layer(unmodified_id);
        tyl == TypeLayer::Void
    }

    /// Returns `true` if the type is a buffer address
    pub fn is_buffer_address(&self, id: TypeId) -> bool {
        let unmodified_id = self.remove_modifier(id);
        let tyl = self.get_type_layer(unmodified_id);

        matches!(
            tyl,
            TypeLayer::Object(ObjectType::BufferAddress)
                | TypeLayer::Object(ObjectType::RWBufferAddress)
        )
    }

    /// Returns `true` if the type is const
    pub fn is_const(&self, id: TypeId) -> bool {
        matches!(self.get_non_array_layer(id), TypeLayer::Modifier(m, _) if m.is_const)
    }

    /// Get the stored data for a template type parameter from an id
    pub fn get_template_type(&self, id: TemplateTypeId) -> &TemplateParamType {
        &self.template_types[id.0 as usize]
    }
}

impl Module {
    /// Get or create the object id from an object type
    pub fn register_object(&mut self, object_type: ObjectType) -> ObjectId {
        // Search for an existing registration of the object
        if let Some(object) = self.try_find_object(object_type) {
            return object;
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
                    namespace: None,
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

    /// Get the object id from an existing object type
    pub fn try_find_object(&self, object_type: ObjectType) -> Option<ObjectId> {
        // Search for an existing registration of the object
        for (i, existing) in self.type_registry.object_layouts.iter().enumerate() {
            if object_type == *existing {
                return Some(ObjectId(i as u32));
            }
        }
        None
    }
}

/// Basic scalar types
#[derive(PartialEq, Eq, Hash, Copy, Clone)]
pub enum ScalarType {
    Bool,
    IntLiteral,
    Int32,
    UInt32,
    FloatLiteral,
    Float16,
    Float32,
    Float64,
}

/// The dimensions of a scalar, vector or matrix data type
#[derive(PartialEq, Eq, Debug, Copy, Clone)]
pub enum NumericDimension {
    Scalar,
    Vector(u32),
    Matrix(u32, u32),
}

/// Representation for a built in scalar, vector or matrix data type
#[derive(PartialEq, Eq, Debug, Copy, Clone)]
pub struct NumericType {
    pub scalar: ScalarType,
    pub dimension: NumericDimension,
}

/// Id to a user defined struct
#[derive(PartialEq, Eq, Hash, PartialOrd, Ord, Debug, Clone, Copy)]
pub struct StructId(pub u32);

/// Id to a user defined struct template
#[derive(PartialEq, Eq, Hash, PartialOrd, Ord, Debug, Clone, Copy)]
pub struct StructTemplateId(pub u32);

/// Id to a user defined enum
#[derive(PartialEq, Eq, Hash, PartialOrd, Ord, Debug, Clone, Copy)]
pub struct EnumId(pub u32);

/// Id to a template type argument
#[derive(PartialEq, Eq, Hash, PartialOrd, Ord, Debug, Clone, Copy)]
pub struct TemplateTypeId(pub u32);

/// Id to a template type or value argument
#[derive(PartialEq, Eq, Hash, PartialOrd, Ord, Debug, Clone, Copy)]
pub enum TemplateParam {
    Type(TemplateTypeId),
    Value(TemplateValueId),
}

/// Modifiers that can apply to any type
#[derive(PartialEq, Eq, Hash, Copy, Clone)]
pub struct TypeModifier {
    // If the type has the const modifier
    pub is_const: bool,

    // If the type has the volatile modifier
    pub volatile: bool,

    // If the type has row_major modifier.
    // While not valid with column_major or with non-matrix types during type checking - this is valid in the syntax tree
    pub row_major: bool,

    // If the type has column_major modifier.
    // While not valid with row_major or with non-matrix types during type checking - this is valid in the syntax tree
    pub column_major: bool,

    // If the type has unorm modifier
    pub unorm: bool,

    // If the type has snorm modifier
    pub snorm: bool,
}

/// Storage type for global variables
#[derive(PartialEq, Eq, Copy, Clone, Default)]
pub enum GlobalStorage {
    /// Input from outside the shader (default)
    #[default]
    Extern,

    /// Statically allocated thread-local variable in global scope
    Static,

    /// Shared between every thread in the work group
    GroupShared,
}

/// Storage type for local variables
#[derive(PartialEq, Eq, Debug, Copy, Clone, Default)]
pub enum LocalStorage {
    /// Statically allocated thread-local variable
    #[default]
    Local,

    /// Statically allocated thread-local variable that persists between function calls
    /// Essentially the same as global static but name-scoped into a function
    Static,
}

/// Binding type for parameters
#[derive(PartialEq, Eq, Copy, Clone, Default)]
pub enum InputModifier {
    /// Function input
    #[default]
    In,

    /// Function output (must be written)
    Out,

    /// Function input and output
    InOut,
}

/// Modifier applied to a variable interpolated between stages
#[derive(PartialEq, Eq, Copy, Clone, Debug)]
pub enum InterpolationModifier {
    /// Interpolate at pixel center
    CenterPerspective,

    /// Interpolate at approximate center of coverage
    CentroidPerspective,

    /// Interpolate at sample location
    SamplePerspective,

    /// Interpolate at pixel center without perspective-correction
    CenterNoPerspective,

    /// Interpolate at approximate center of coverage without perspective-correction
    CentroidNoPerspective,

    /// Interpolate at sample location without perspective-correction
    SampleNoPerspective,

    /// Do not interpolate
    Flat,

    /// Geometry shader point primitive type
    Point,

    /// Geometry shader line primitive type
    Line,

    /// Geometry shader triangle primitive type
    Triangle,

    /// Geometry shader lineadj primitive type
    LineAdj,

    /// Geometry shader triangleadj primitive type
    TriangleAdj,

    /// Mesh shader vertices output type - not exactly an interpolation modifier but appears in the same place
    Vertices,

    /// Mesh shader primitives output type - not exactly an interpolation modifier but appears in the same place
    Primitives,

    /// Mesh shader indices output type - not exactly an interpolation modifier but appears in the same place
    Indices,

    /// Mesh shader payload input type - not exactly an interpolation modifier but appears in the same place
    Payload,
}

/// Value for outputtopology attribute of a mesh shader
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub enum OutputTopology {
    Point,
    Line,
    Triangle,
}

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
    Texture2DMips(TypeId),
    Texture2DMipsSlice(TypeId),

    Texture2DArray(TypeId),
    Texture2DArrayMips(TypeId),
    Texture2DArrayMipsSlice(TypeId),

    RWTexture2D(TypeId),

    RWTexture2DArray(TypeId),

    TextureCube(TypeId),
    TextureCubeArray(TypeId),

    Texture3D(TypeId),
    Texture3DMips(TypeId),
    Texture3DMipsSlice(TypeId),

    RWTexture3D(TypeId),

    ConstantBuffer(TypeId),

    SamplerState,
    SamplerComparisonState,

    TriangleStream(TypeId),

    RaytracingAccelerationStructure,
    RayQuery(u32),
    RayDesc,
}

/// A constant value
#[derive(PartialEq, Clone, Debug)]
pub enum Constant {
    /// Boolean value
    Bool(bool),

    /// Int literal before it receives a proper int type
    IntLiteral(i128),

    /// 32-bit signed integer
    Int32(i32),

    /// 32-bit unsigned integer
    UInt32(u32),

    /// 64-bit signed integer
    Int64(i64),

    /// 64-bit unsigned integer
    UInt64(u64),

    /// Float literal before it receives a proper float type
    FloatLiteral(f64),

    /// 16-bit floating point value
    Float16(f32),

    /// 32-bit floating point value
    Float32(f32),

    /// 64-bit floating point value
    Float64(f64),

    /// A string value
    String(String),

    /// Enum value - with underlying constant value
    Enum(EnumId, Box<Constant>),
}

/// A constant value with unique equality
#[derive(PartialEq, Eq, Hash, Clone, Debug)]
pub enum RestrictedConstant {
    /// Boolean value
    Bool(bool),

    /// Int literal before it receives a proper int type
    IntLiteral(i128),

    /// 32-bit signed integer
    Int32(i32),

    /// 32-bit unsigned integer
    UInt32(u32),

    /// 64-bit signed integer
    Int64(i64),

    /// 64-bit unsigned integer
    UInt64(u64),

    /// Enum value - with underlying constant value
    Enum(EnumId, Box<RestrictedConstant>),
}

/// Either a type or a constant
#[derive(PartialEq, Eq, Hash, Clone, Debug)]
pub enum TypeOrConstant {
    /// Value is a type
    Type(TypeId),

    /// Value is a constant
    Constant(RestrictedConstant),
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
    /// Returns `true` if the layer is a modifier layer
    pub fn is_modifier(&self) -> bool {
        matches!(self, TypeLayer::Modifier(_, _))
    }

    /// Returns `true` if the type is an object
    pub fn is_object(&self) -> bool {
        assert!(!self.is_modifier());
        matches!(self, &TypeLayer::Object(_))
    }

    /// Construct a [NumericDimension] from a layer
    pub fn to_dimensions(self) -> NumericDimension {
        assert!(!self.is_modifier());
        match self {
            TypeLayer::Vector(_, x) => NumericDimension::Vector(x),
            TypeLayer::Matrix(_, x, y) => NumericDimension::Matrix(x, y),
            // Treat all enum and non-numeric types as scalar
            _ => NumericDimension::Scalar,
        }
    }

    /// Get the size of the first dimension for a vector or matrix
    pub fn to_x(&self) -> Option<u32> {
        assert!(!self.is_modifier());
        match *self {
            TypeLayer::Vector(_, ref x) => Some(*x),
            TypeLayer::Matrix(_, ref x, _) => Some(*x),
            _ => None,
        }
    }

    /// Get the size of the second dimension for a matrix
    pub fn to_y(&self) -> Option<u32> {
        assert!(!self.is_modifier());
        match *self {
            TypeLayer::Matrix(_, _, ref y) => Some(*y),
            _ => None,
        }
    }

    /// Get the total number of scalar elements in the type - or 1 for non-numeric types
    pub fn get_num_elements(&self) -> u32 {
        match (self.to_x(), self.to_y()) {
            (Some(x1), Some(x2)) => x1 * x2,
            (Some(x1), None) => x1,
            (None, Some(x2)) => x2,
            (None, None) => 1,
        }
    }

    /// Attempt to get the most significant dimension of two data types
    pub fn most_significant_dimension(lhs: Self, rhs: Self) -> Option<NumericDimension> {
        assert!(!lhs.is_modifier());
        assert!(!rhs.is_modifier());
        use std::cmp::max;
        use std::cmp::min;
        use TypeLayer::*;
        match (lhs, rhs) {
            (Scalar(_), Scalar(_)) => Some(NumericDimension::Scalar),
            (Scalar(_), Vector(_, ref x)) => Some(NumericDimension::Vector(*x)),
            (Vector(_, ref x), Scalar(_)) => Some(NumericDimension::Vector(*x)),
            (Vector(_, ref x1), Vector(_, ref x2)) if *x1 == 1 || *x2 == 1 => {
                Some(NumericDimension::Vector(max(*x1, *x2)))
            }
            (Vector(_, ref x1), Vector(_, ref x2)) => {
                let x = min(*x1, *x2);
                Some(NumericDimension::Vector(x))
            }
            (Matrix(_, ref x1, ref y1), Matrix(_, ref x2, ref y2)) => {
                let x = min(*x1, *x2);
                let y = min(*y1, *y2);
                Some(NumericDimension::Matrix(x, y))
            }
            _ => None,
        }
    }
}

impl ScalarType {
    /// Get the size in bytes of the primitive scalar
    pub const fn get_size(self) -> Option<u32> {
        match self {
            ScalarType::Bool => Some(4),
            ScalarType::IntLiteral => None,
            ScalarType::Int32 => Some(4),
            ScalarType::UInt32 => Some(4),
            ScalarType::FloatLiteral => None,
            ScalarType::Float16 => Some(2),
            ScalarType::Float32 => Some(4),
            ScalarType::Float64 => Some(8),
        }
    }

    /// Parse scalar type as part of a string
    const fn parse_str(input: &[u8]) -> Option<(&[u8], ScalarType)> {
        match input {
            [b'b', b'o', b'o', b'l', rest @ ..] => Some((rest, ScalarType::Bool)),
            [b'i', b'n', b't', rest @ ..] => Some((rest, ScalarType::Int32)),
            [b'u', b'i', b'n', b't', rest @ ..] => Some((rest, ScalarType::UInt32)),
            [b'd', b'w', b'o', b'r', b'd', rest @ ..] => Some((rest, ScalarType::UInt32)),
            [b'h', b'a', b'l', b'f', rest @ ..] => Some((rest, ScalarType::Float16)),
            [b'f', b'l', b'o', b'a', b't', rest @ ..] => Some((rest, ScalarType::Float32)),
            [b'd', b'o', b'u', b'b', b'l', b'e', rest @ ..] => Some((rest, ScalarType::Float64)),
            _ => None,
        }
    }
}

impl NumericDimension {
    /// Construct a NumericDimension from optional dimension values
    pub fn from_parts(x_opt: Option<u32>, y_opt: Option<u32>) -> Self {
        match (x_opt, y_opt) {
            (Some(x), Some(y)) => NumericDimension::Matrix(x, y),
            (Some(x), None) => NumericDimension::Vector(x),
            (None, None) => NumericDimension::Scalar,
            _ => panic!("invalid numeric dimension parts"),
        }
    }

    /// Get the maximum of two dimension values
    pub fn max_dim(r1: Option<u32>, r2: Option<u32>) -> Option<u32> {
        use std::cmp::max;
        match (r1, r2) {
            (Some(x1), Some(x2)) => Some(max(x1, x2)),
            (Some(x1), None) => Some(x1),
            (None, Some(x2)) => Some(x2),
            (None, None) => None,
        }
    }
}

impl NumericType {
    /// Parse numeric type from a string
    pub const fn from_str(typename: &str) -> Option<NumericType> {
        const fn digit(input: &[u8]) -> Option<(&[u8], u32)> {
            match input {
                [b'1', rest @ ..] => Some((rest, 1)),
                [b'2', rest @ ..] => Some((rest, 2)),
                [b'3', rest @ ..] => Some((rest, 3)),
                [b'4', rest @ ..] => Some((rest, 4)),
                _ => None,
            }
        }

        const fn parse_str(input: &[u8]) -> Option<(&[u8], NumericType)> {
            let (rest, ty) = match ScalarType::parse_str(input) {
                Some(ok) => ok,
                None => return None,
            };
            if rest.is_empty() {
                return Some((
                    &[],
                    NumericType {
                        scalar: ty,
                        dimension: NumericDimension::Scalar,
                    },
                ));
            }

            let (rest, x) = match digit(rest) {
                Some(ok) => ok,
                None => return None,
            };
            if rest.is_empty() {
                return Some((
                    &[],
                    NumericType {
                        scalar: ty,
                        dimension: NumericDimension::Vector(x),
                    },
                ));
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
                return Some((
                    &[],
                    NumericType {
                        scalar: ty,
                        dimension: NumericDimension::Matrix(x, y),
                    },
                ));
            }

            None
        }

        let type_name_bytes = typename.as_bytes();

        match parse_str(type_name_bytes) {
            Some((rest, ty)) => {
                if !rest.is_empty() {
                    panic!("NumericType::from_str expects to finish the string`");
                }
                Some(ty)
            }
            None => None,
        }
    }
}

impl TypeModifier {
    /// Create the default type modifier
    pub const fn new() -> TypeModifier {
        TypeModifier {
            is_const: false,
            volatile: false,
            row_major: false,
            column_major: false,
            unorm: false,
            snorm: false,
        }
    }

    /// Test if we do not have any modifiers
    pub fn is_empty(&self) -> bool {
        *self == Default::default()
    }

    /// Create a modifier which is const
    pub fn const_only() -> TypeModifier {
        TypeModifier {
            is_const: true,
            ..TypeModifier::default()
        }
    }

    /// Combine two modifier sets
    pub fn combine(self, other: TypeModifier) -> Self {
        TypeModifier {
            is_const: self.is_const || other.is_const,
            volatile: self.volatile || other.volatile,
            row_major: self.row_major || other.row_major,
            column_major: self.column_major || other.column_major,
            unorm: self.unorm || other.unorm,
            snorm: self.snorm || other.snorm,
        }
    }
}

impl Default for TypeRegistry {
    fn default() -> Self {
        TypeRegistry {
            layers: RefCell::new(Vec::with_capacity(256)),
            object_layouts: Vec::with_capacity(64),
            object_functions: Vec::with_capacity(64),
            template_types: Vec::with_capacity(256),
        }
    }
}

impl Default for TypeModifier {
    fn default() -> TypeModifier {
        TypeModifier::new()
    }
}

impl ObjectType {
    pub fn get_register_type(&self) -> RegisterType {
        match self {
            ObjectType::Buffer(_)
            | ObjectType::ByteAddressBuffer
            | ObjectType::BufferAddress
            | ObjectType::StructuredBuffer(_)
            | ObjectType::Texture2D(_)
            | ObjectType::Texture2DArray(_)
            | ObjectType::TextureCube(_)
            | ObjectType::TextureCubeArray(_)
            | ObjectType::Texture3D(_)
            | ObjectType::RaytracingAccelerationStructure => RegisterType::T,

            ObjectType::RWBuffer(_)
            | ObjectType::RWByteAddressBuffer
            | ObjectType::RWBufferAddress
            | ObjectType::RWStructuredBuffer(_)
            | ObjectType::RWTexture2D(_)
            | ObjectType::RWTexture2DArray(_)
            | ObjectType::RWTexture3D(_) => RegisterType::U,

            ObjectType::ConstantBuffer(_) => RegisterType::B,

            ObjectType::SamplerState | ObjectType::SamplerComparisonState => RegisterType::S,

            ObjectType::Texture2DMips(_)
            | ObjectType::Texture2DMipsSlice(_)
            | ObjectType::Texture2DArrayMips(_)
            | ObjectType::Texture2DArrayMipsSlice(_)
            | ObjectType::Texture3DMips(_)
            | ObjectType::Texture3DMipsSlice(_)
            | ObjectType::TriangleStream(_)
            | ObjectType::RayQuery(_)
            | ObjectType::RayDesc => {
                panic!("get_register_type called on non-root object types")
            }
        }
    }
}

impl Constant {
    /// Cast a constant to a u64 value
    pub fn to_uint64(&self) -> Option<u64> {
        match self {
            Constant::Bool(v) => Some(u64::from(*v)),
            Constant::IntLiteral(v) if *v <= u64::MAX as i128 => Some(*v as u64),
            Constant::Int32(v) if *v >= 0 => Some(*v as u64),
            Constant::UInt32(v) => Some(*v as u64),
            Constant::Int64(v) if *v >= 0 => Some(*v as u64),
            Constant::UInt64(v) => Some(*v),
            _ => None,
        }
    }

    /// Cast a constant to a f32 value
    pub fn to_f32(&self) -> Option<f32> {
        match self {
            Constant::Bool(v) => Some(f32::from(*v)),
            Constant::IntLiteral(v) if *v <= f32::MAX as i128 => Some(*v as f32),
            Constant::Int32(v) if *v >= 0 => Some(*v as f32),
            Constant::UInt32(v) => Some(*v as f32),
            Constant::Int64(v) if *v >= 0 => Some(*v as f32),
            Constant::UInt64(v) => Some(*v as f32),
            Constant::Float16(v) => Some(*v),
            Constant::Float32(v) => Some(*v),
            Constant::Float64(v) => Some(*v as f32),
            _ => None,
        }
    }

    /// Find the type of a constant
    pub fn get_type(&self, module: &Module) -> ExpressionType {
        let tyl = match self {
            Constant::Bool(_) => TypeLayer::Scalar(ScalarType::Bool),
            Constant::IntLiteral(_) => TypeLayer::Scalar(ScalarType::IntLiteral),
            Constant::Int32(_) => TypeLayer::Scalar(ScalarType::Int32),
            Constant::UInt32(_) => TypeLayer::Scalar(ScalarType::UInt32),
            Constant::Int64(_) => unimplemented!(),
            Constant::UInt64(_) => unimplemented!(),
            Constant::FloatLiteral(_) => TypeLayer::Scalar(ScalarType::FloatLiteral),
            Constant::Float16(_) => TypeLayer::Scalar(ScalarType::Float16),
            Constant::Float32(_) => TypeLayer::Scalar(ScalarType::Float32),
            Constant::Float64(_) => TypeLayer::Scalar(ScalarType::Float64),
            Constant::String(_) => panic!("strings not supported"),
            Constant::Enum(_, _) => panic!("enum not expected"),
        };
        module.type_registry.register_type(tyl).to_rvalue()
    }
}

impl RestrictedConstant {
    /// Convert to an unrestricted constant
    pub fn unrestrict(self) -> Constant {
        match self {
            RestrictedConstant::Bool(v) => Constant::Bool(v),
            RestrictedConstant::IntLiteral(v) => Constant::IntLiteral(v),
            RestrictedConstant::Int32(v) => Constant::Int32(v),
            RestrictedConstant::UInt32(v) => Constant::UInt32(v),
            RestrictedConstant::Int64(v) => Constant::Int64(v),
            RestrictedConstant::UInt64(v) => Constant::UInt64(v),
            RestrictedConstant::Enum(ty, v) => Constant::Enum(ty, Box::new(v.unrestrict())),
        }
    }

    /// Cast a constant to a u64 value
    pub fn to_uint64(&self) -> Option<u64> {
        self.clone().unrestrict().to_uint64()
    }
}

impl TypeOrConstant {
    pub fn as_constant(&self) -> Option<&RestrictedConstant> {
        match self {
            TypeOrConstant::Type(_) => None,
            TypeOrConstant::Constant(val) => Some(val),
        }
    }
}

pub struct TypeIdIterator {
    current: u32,
    end: u32,
}

impl Iterator for TypeIdIterator {
    type Item = TypeId;

    fn next(&mut self) -> Option<Self::Item> {
        let result = if self.current < self.end {
            Some(TypeId(self.current))
        } else {
            None
        };
        self.current += 1;
        result
    }
}

impl std::fmt::Debug for TypeLayer {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match *self {
            TypeLayer::Void => write!(f, "void"),
            TypeLayer::Scalar(ref st) => write!(f, "{st:?}"),
            TypeLayer::Vector(ref st, ref x) => write!(f, "vector<{st:?}, {x}>"),
            TypeLayer::Matrix(ref st, ref x, ref y) => write!(f, "matrix<{st:?}, {x}, {y}>"),
            TypeLayer::Struct(ref sid) => write!(f, "struct<{}>", sid.0),
            TypeLayer::StructTemplate(ref sid) => write!(f, "struct_template<{}>", sid.0),
            TypeLayer::Enum(ref id) => write!(f, "enum<{}>", id.0),
            TypeLayer::Object(ref ot) => write!(f, "{ot:?}"),
            TypeLayer::Array(ref ty, Some(ref len)) => write!(f, "type<{ty:?}>[{len}]"),
            TypeLayer::Array(ref ty, None) => write!(f, "type<{ty:?}>[]"),
            TypeLayer::TemplateParam(ref id) => write!(f, "typename<{}>", id.0),
            TypeLayer::Modifier(ref modifier, ref ty) => write!(f, "{modifier:?}type<{ty:?}>"),
        }
    }
}

impl std::fmt::Debug for ScalarType {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            ScalarType::Bool => write!(f, "bool"),
            ScalarType::IntLiteral => write!(f, "literal int"),
            ScalarType::Int32 => write!(f, "int"),
            ScalarType::UInt32 => write!(f, "uint"),
            ScalarType::FloatLiteral => write!(f, "literal float"),
            ScalarType::Float16 => write!(f, "half"),
            ScalarType::Float32 => write!(f, "float"),
            ScalarType::Float64 => write!(f, "double"),
        }
    }
}

impl std::fmt::Debug for GlobalStorage {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            GlobalStorage::Extern => write!(f, "extern"),
            GlobalStorage::Static => write!(f, "static"),
            GlobalStorage::GroupShared => write!(f, "groupshared"),
        }
    }
}

impl std::fmt::Debug for InputModifier {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            InputModifier::In => write!(f, "in"),
            InputModifier::Out => write!(f, "out"),
            InputModifier::InOut => write!(f, "inout"),
        }
    }
}

impl std::fmt::Debug for TypeModifier {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        if self.row_major {
            write!(f, "row_major ")?;
        }
        if self.column_major {
            write!(f, "column_major ")?;
        }
        if self.unorm {
            write!(f, "unorm ")?;
        }
        if self.snorm {
            write!(f, "snorm ")?;
        }
        if self.is_const {
            write!(f, "const ")?;
        }
        if self.volatile {
            write!(f, "volatile ")?;
        }
        Ok(())
    }
}

impl std::fmt::Debug for ObjectType {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        use self::ObjectType::*;
        match *self {
            Buffer(ref dt) => write!(f, "Buffer<{dt:?}>"),
            RWBuffer(ref dt) => write!(f, "RWBuffer<{dt:?}>"),
            ByteAddressBuffer => write!(f, "ByteAddressBuffer"),
            RWByteAddressBuffer => write!(f, "RWByteAddressBuffer"),
            BufferAddress => write!(f, "BufferAddress"),
            RWBufferAddress => write!(f, "RWBufferAddress"),
            StructuredBuffer(ref st) => write!(f, "StructuredBuffer<{st:?}>"),
            RWStructuredBuffer(ref st) => write!(f, "RWStructuredBuffer<{st:?}>"),
            Texture2D(ref dt) => write!(f, "Texture2D<{dt:?}>"),
            Texture2DMips(ref dt) => write!(f, "Texture2D<{dt:?}>::Mips"),
            Texture2DMipsSlice(ref dt) => write!(f, "Texture2D<{dt:?}>::MipsSlice"),
            Texture2DArray(ref dt) => write!(f, "Texture2DArray<{dt:?}>"),
            Texture2DArrayMips(ref dt) => write!(f, "Texture2DArray<{dt:?}>::Mips"),
            Texture2DArrayMipsSlice(ref dt) => write!(f, "Texture2DArray<{dt:?}>::MipsSlice"),
            RWTexture2D(ref dt) => write!(f, "RWTexture2D<{dt:?}>"),
            RWTexture2DArray(ref dt) => write!(f, "RWTexture2DArray<{dt:?}>"),
            TextureCube(ref dt) => write!(f, "TextureCube<{dt:?}>"),
            TextureCubeArray(ref dt) => write!(f, "TextureCubeArray<{dt:?}>"),
            Texture3D(ref dt) => write!(f, "Texture3D<{dt:?}>"),
            Texture3DMips(ref dt) => write!(f, "Texture3D<{dt:?}>::Mips"),
            Texture3DMipsSlice(ref dt) => write!(f, "Texture3D<{dt:?}>::MipsSlice"),
            RWTexture3D(ref dt) => write!(f, "RWTexture3D<{dt:?}>"),
            ConstantBuffer(ref st) => write!(f, "ConstantBuffer<{st:?}>"),
            SamplerState => write!(f, "SamplerState"),
            SamplerComparisonState => write!(f, "SamplerComparisonState"),
            TriangleStream(ref st) => write!(f, "TriangleStream<{st:?}>"),
            RaytracingAccelerationStructure => write!(f, "RaytracingAccelerationStructure"),
            RayQuery(ref v) => write!(f, "RayQuery<{v}>"),
            RayDesc => write!(f, "RayDesc"),
        }
    }
}

#[test]
fn test_parse_numeric_str() {
    assert_eq!(
        NumericType::from_str("float"),
        Some(NumericType {
            scalar: ScalarType::Float32,
            dimension: NumericDimension::Scalar
        })
    );
    assert_eq!(
        NumericType::from_str("uint3"),
        Some(NumericType {
            scalar: ScalarType::UInt32,
            dimension: NumericDimension::Vector(3)
        })
    );
    assert_eq!(
        NumericType::from_str("bool2x3"),
        Some(NumericType {
            scalar: ScalarType::Bool,
            dimension: NumericDimension::Matrix(2, 3)
        })
    );

    assert_eq!(NumericType::from_str(""), None);
    assert_eq!(NumericType::from_str("float5"), None);
    assert_eq!(NumericType::from_str("float2x"), None);
}

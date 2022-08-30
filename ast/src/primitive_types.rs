/// Basic scalar types
#[derive(PartialEq, Copy, Clone)]
pub enum ScalarType {
    Bool,
    UntypedInt,
    Int,
    UInt,
    Half,
    Float,
    Double,
}

#[derive(PartialEq, Debug, Copy, Clone)]
pub enum NumericDimension {
    Scalar,
    Vector(u32),
    Matrix(u32, u32),
}

/// Modifiers that can apply to any type
#[derive(PartialEq, Copy, Clone)]
pub struct TypeModifier {
    pub is_const: bool,
    pub row_order: RowOrder,
    pub precise: bool,
    pub volatile: bool,
}

/// Matrix ordering
#[derive(PartialEq, Debug, Copy, Clone)]
pub enum RowOrder {
    Row,
    Column,
}

/// Storage type for global variables
#[derive(PartialEq, Clone)]
pub enum GlobalStorage {
    // Input from outside the shader (default)
    Extern,

    /// Statically allocated thread-local variable in global scope
    Static,

    /// Shared between every thread in the work group
    GroupShared,
}

/// Storage type for local variables
#[derive(PartialEq, Debug, Clone)]
pub enum LocalStorage {
    /// Statically allocated thread-local variable
    Local,

    /// Statically allocated thread-local variable that persists between function calls
    /// Essentially the same as global static but name-scoped into a function
    Static,
}

/// Binding type for parameters
#[derive(PartialEq, Clone)]
pub enum InputModifier {
    /// Function input
    In,
    /// Function output (must be written)
    Out,
    /// Function input and output
    InOut,
}

/// Modifier applied to a variable interpolated between stages
#[derive(PartialEq, Clone)]
pub enum InterpolationModifier {
    NoInterpolation,
    Linear,
    Centroid,
    NoPerspective,
    Sample,
}

#[derive(PartialEq, Debug, Clone)]
pub enum Semantic {
    DispatchThreadId,
    GroupId,
    GroupIndex,
    GroupThreadId,
    VertexId,
    InstanceId,
    PrimitiveId,
    Position,
    Target(u8),
    Depth,
    DepthGreaterEqual,
    DepthLessEqual,
    User(String),
}

impl TypeModifier {
    /// Create the default type modifier set
    pub const fn new() -> TypeModifier {
        TypeModifier {
            is_const: false,
            row_order: RowOrder::Column,
            precise: false,
            volatile: false,
        }
    }

    /// Test if we do not have any modifiers
    pub fn is_empty(&self) -> bool {
        !self.is_const && self.row_order == RowOrder::Column && !self.precise && !self.volatile
    }

    /// Create a modifier which is const
    pub fn const_only() -> TypeModifier {
        TypeModifier {
            is_const: true,
            ..TypeModifier::default()
        }
    }

    /// Remove all modifiers except for precise
    pub fn keep_precise(&self) -> TypeModifier {
        TypeModifier {
            precise: self.precise,
            ..TypeModifier::default()
        }
    }
}

impl Default for TypeModifier {
    fn default() -> TypeModifier {
        TypeModifier::new()
    }
}

impl Default for GlobalStorage {
    fn default() -> GlobalStorage {
        GlobalStorage::Extern
    }
}

impl Default for InputModifier {
    fn default() -> InputModifier {
        InputModifier::In
    }
}

impl Default for LocalStorage {
    fn default() -> LocalStorage {
        LocalStorage::Local
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

impl std::fmt::Debug for InterpolationModifier {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            InterpolationModifier::NoInterpolation => write!(f, "nointerpolation"),
            InterpolationModifier::Linear => write!(f, "linear"),
            InterpolationModifier::Centroid => write!(f, "centroid"),
            InterpolationModifier::NoPerspective => write!(f, "noperspective"),
            InterpolationModifier::Sample => write!(f, "sample"),
        }
    }
}

impl std::fmt::Debug for TypeModifier {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        if self.precise {
            write!(f, "precise ")?;
        }
        if self.row_order == RowOrder::Row {
            write!(f, "row_major ")?;
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

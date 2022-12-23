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
}

/// Storage type for global variables
#[derive(PartialEq, Eq, Copy, Clone)]
pub enum GlobalStorage {
    // Input from outside the shader (default)
    Extern,

    /// Statically allocated thread-local variable in global scope
    Static,

    /// Shared between every thread in the work group
    GroupShared,
}

/// Storage type for local variables
#[derive(PartialEq, Eq, Debug, Copy, Clone)]
pub enum LocalStorage {
    /// Statically allocated thread-local variable
    Local,

    /// Statically allocated thread-local variable that persists between function calls
    /// Essentially the same as global static but name-scoped into a function
    Static,
}

/// Binding type for parameters
#[derive(PartialEq, Eq, Copy, Clone)]
pub enum InputModifier {
    /// Function input
    In,
    /// Function output (must be written)
    Out,
    /// Function input and output
    InOut,
}

/// Modifier applied to a variable interpolated between stages
#[derive(PartialEq, Eq, Clone)]
pub enum InterpolationModifier {
    NoInterpolation,
    Linear,
    Centroid,
    NoPerspective,
    Sample,
}

/// Semantic identifier for linking inputs / outputs
#[derive(PartialEq, Eq, Debug, Clone)]
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
            volatile: false,
            row_major: false,
            column_major: false,
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

    pub fn combine(self, other: TypeModifier) -> Self {
        TypeModifier {
            is_const: self.is_const || other.is_const,
            volatile: self.volatile || other.volatile,
            row_major: self.row_major || other.row_major,
            column_major: self.column_major || other.column_major,
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
        if self.row_major {
            write!(f, "row_major ")?;
        }
        if self.column_major {
            write!(f, "column_major ")?;
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

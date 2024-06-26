use crate::ast_declarations::Declarator;
use crate::Expression;
use crate::ExpressionOrType;
use crate::ScopedIdentifier;
use rssl_text::{Locate, Located, SourceLocation};

/// The type part of a declaration
///
/// As AST does not have full type information this may trigger ambiguous parse branches in the tree.
#[derive(PartialEq, Clone)]
pub struct Type {
    pub layout: TypeLayout,
    pub modifiers: TypeModifierSet,
    pub location: SourceLocation,
}

/// A type-id that includes both a base type and optionally modifiers from an abstract declarator
///
/// As AST does not have full type information this may trigger ambiguous parse branches in the tree.
#[derive(PartialEq, Clone)]
pub struct TypeId {
    /// The type specifier part of the type
    pub base: Type,

    /// The modifiers from the declarator part. The base must be [Declarator::Empty] and not [Declarator::Identifier].
    pub abstract_declarator: Declarator,
}

/// A type name reference without modifiers
#[derive(PartialEq, Clone)]
pub struct TypeLayout(pub ScopedIdentifier, pub Box<[ExpressionOrType]>);

/// Modifiers that can apply to type-like objects in the syntax tree
#[derive(PartialEq, Eq, Clone)]
pub struct TypeModifierSet {
    pub modifiers: Vec<Located<TypeModifier>>,
}

/// Single modifier that can apply to type-like objects in the syntax tree
///
/// These are not just modifiers for the type - these are also storage classes for the variable
/// The syntax tree does not care about which combinations are valid - these are resolved later during type checking
#[derive(PartialEq, Eq, Copy, Clone)]
pub enum TypeModifier {
    /// const modifier for any type
    Const,

    /// volatile modifier for any type - which does not do anything
    Volatile,

    /// row_major modifier for matrix type
    RowMajor,

    /// column_major modifier for matrix type
    ColumnMajor,

    /// unorm type modifier for floating point type
    Unorm,

    /// snorm type modifier for floating point type
    Snorm,

    /// in modifier to a function parameter
    In,

    /// out modifier to a function parameter
    Out,

    /// inout modifier to a function parameter
    InOut,

    /// Global variable: Input from outside the shader (default)
    Extern,

    /// Global or local variable: Statically allocated lane-local variable in global scope
    Static,

    /// Global variable: Shared between every lane in the work group
    GroupShared,

    /// Global or local variable: Computations leading to this value must be precise/invariant
    Precise,

    /// nointerpolation interpolation modifier
    NoInterpolation,

    /// linear interpolation modifier
    Linear,

    /// centroid interpolation modifier
    Centroid,

    /// noperspective interpolation modifier
    NoPerspective,

    /// sample interpolation modifier
    Sample,

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

    /// Mesh shader vertices output
    Vertices,

    /// Mesh shader primitives output
    Primitives,

    /// Mesh shader indices output
    Indices,

    /// Mesh shader input from task shader payload
    Payload,

    /// Metal shading language address space modifier
    AddressSpace(AddressSpace),
}

/// Address space qualifiers for pointer types
#[derive(PartialEq, Eq, Copy, Clone)]
pub enum AddressSpace {
    // Metal shading language device address space
    Device,

    // Metal shading language constant address space
    Constant,

    // Metal shading language thread address space
    Thread,

    // Metal shading language threadgroup address space
    ThreadGroup,

    // Metal shading language threadgroup_imageblock address space
    ThreadGroupImageBlock,

    // Metal shading language ray_data address space
    RayData,

    // Metal shading language object_data address space
    ObjectData,
}

/// Semantic identifier for linking inputs / outputs
#[derive(PartialEq, Eq, Hash, Debug, Clone)]
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

/// Template parameters
#[derive(PartialEq, Debug, Clone)]
pub struct TemplateParamList(pub Vec<TemplateParam>);

/// Template parameter - either a type or a value
#[derive(PartialEq, Debug, Clone)]
pub enum TemplateParam {
    Type(TemplateTypeParam),
    Value(TemplateValueParam),
}

/// Type template parameter
#[derive(PartialEq, Debug, Clone)]
pub struct TemplateTypeParam {
    pub name: Option<Located<String>>,
    pub default: Option<Type>,
}

/// Value template parameter
#[derive(PartialEq, Debug, Clone)]
pub struct TemplateValueParam {
    pub value_type: Type,
    pub name: Option<Located<String>>,
    pub default: Option<Located<Expression>>,
}

/// A typedef definition
///
/// Currently these are unique nodes but the syntax is meant to support these as a modifier on normal declarations
#[derive(PartialEq, Debug, Clone)]
pub struct Typedef {
    /// The type information for the typedef
    pub source: Type,

    /// Name and type part assigned to the name
    /// We do not support multiple declarations on the same line with typedef
    pub declarator: Declarator,
}

impl Type {
    /// Make a type from a single name
    pub fn trivial(name: &str) -> Self {
        Type::from_layout(TypeLayout::trivial(name))
    }

    /// Make a type from a layout and without modifiers
    pub fn from_layout(layout: TypeLayout) -> Self {
        let location = layout.0.identifiers[0].location;
        Type {
            layout,
            modifiers: TypeModifierSet::new(),
            location,
        }
    }

    /// Make a type from a single located name and template arguments
    pub fn with_template_types(name: Located<&str>, types: &[ExpressionOrType]) -> Self {
        Type::from_layout(TypeLayout::with_template_types(name, types))
    }

    /// Apply a set of modifiers to a type
    pub fn with_modifiers(mut self, modifiers: &[Located<TypeModifier>]) -> Self {
        assert!(self.modifiers.modifiers.is_empty());
        self.modifiers = TypeModifierSet::from(modifiers);
        self
    }
}

impl TypeLayout {
    /// Make a type layout from a single name
    pub fn trivial(name: &str) -> Self {
        TypeLayout(ScopedIdentifier::trivial(name), Default::default())
    }

    /// Make a type layout froom a single located name and template arguments
    pub fn with_template_types(name: Located<&str>, types: &[ExpressionOrType]) -> Self {
        TypeLayout(name.into(), types.to_vec().into_boxed_slice())
    }
}

impl TypeModifierSet {
    /// Create the default type modifier set
    pub const fn new() -> Self {
        TypeModifierSet {
            modifiers: Vec::new(),
        }
    }

    /// Create the default type modifier set
    pub fn from(modifiers: &[Located<TypeModifier>]) -> Self {
        TypeModifierSet {
            modifiers: Vec::from(modifiers),
        }
    }

    /// Combine two sets of modifiers
    pub fn combine(mut self, other: TypeModifierSet) -> Self {
        self.modifiers.extend(other.modifiers);
        self
    }

    /// Add a modifier to the end of the set
    pub fn append(&mut self, modifier: Located<TypeModifier>) {
        self.modifiers.push(modifier);
    }

    /// Add a modifier to the start of the set
    pub fn prepend(&mut self, modifier: Located<TypeModifier>) {
        self.modifiers.insert(0, modifier);
    }
}

impl Default for TypeModifierSet {
    fn default() -> TypeModifierSet {
        TypeModifierSet::new()
    }
}

impl Locate for Type {
    fn get_location(&self) -> SourceLocation {
        self.location
    }
}

impl Locate for TypeId {
    fn get_location(&self) -> SourceLocation {
        self.base.get_location()
    }
}

impl From<TypeLayout> for Type {
    fn from(layout: TypeLayout) -> Self {
        Type::from_layout(layout)
    }
}

impl From<&str> for Type {
    fn from(name: &str) -> Self {
        Located::none(name).into()
    }
}

impl From<Located<&str>> for Type {
    fn from(name: Located<&str>) -> Self {
        let location = name.location;
        Type {
            layout: name.into(),
            modifiers: Default::default(),
            location,
        }
    }
}

impl From<ScopedIdentifier> for Type {
    fn from(name: ScopedIdentifier) -> Self {
        let location = name.identifiers[0].location;
        Type {
            layout: name.into(),
            modifiers: Default::default(),
            location,
        }
    }
}

impl From<Type> for TypeId {
    fn from(base: Type) -> Self {
        TypeId {
            base,
            abstract_declarator: Declarator::Empty,
        }
    }
}

impl From<TypeLayout> for TypeId {
    fn from(layout: TypeLayout) -> Self {
        TypeId::from(Type::from(layout))
    }
}

impl From<&str> for TypeId {
    fn from(name: &str) -> Self {
        TypeId::from(Type::from(name))
    }
}

impl From<Located<&str>> for TypeId {
    fn from(name: Located<&str>) -> Self {
        TypeId::from(Type::from(name))
    }
}

impl From<ScopedIdentifier> for TypeId {
    fn from(name: ScopedIdentifier) -> Self {
        TypeId::from(Type::from(name))
    }
}

impl From<&str> for TypeLayout {
    fn from(name: &str) -> Self {
        Located::none(name).into()
    }
}

impl From<Located<&str>> for TypeLayout {
    fn from(name: Located<&str>) -> Self {
        TypeLayout(name.into(), Default::default())
    }
}

impl From<ScopedIdentifier> for TypeLayout {
    fn from(name: ScopedIdentifier) -> Self {
        TypeLayout(name, Default::default())
    }
}

impl std::fmt::Debug for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(
            f,
            "{:?}{:?} @ {:?}",
            self.modifiers,
            self.layout,
            self.location.get_raw()
        )
    }
}

impl std::fmt::Debug for TypeId {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        if self.abstract_declarator == Declarator::Empty {
            write!(f, "{:?}", self.base)
        } else {
            write!(f, "{:?} ~ {:?}", self.base, self.abstract_declarator)
        }
    }
}

impl std::fmt::Display for TypeLayout {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.0)?;
        if let Some((last, non_last)) = self.1.split_last() {
            write!(f, "<")?;
            for arg in non_last {
                write!(f, "{arg:?}, ")?;
            }
            write!(f, "{last:?}")?;
            write!(f, ">")?;
        }
        Ok(())
    }
}

impl std::fmt::Debug for TypeLayout {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{:?}", self.0)?;
        if let Some((last, non_last)) = self.1.split_last() {
            write!(f, "<")?;
            for arg in non_last {
                write!(f, "{arg:?}, ")?;
            }
            write!(f, "{last:?}")?;
            write!(f, ">")?;
        }
        Ok(())
    }
}

impl std::fmt::Debug for TypeModifierSet {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        for modifier in &self.modifiers {
            write!(f, "{modifier:?} ")?;
        }
        Ok(())
    }
}

impl std::fmt::Debug for TypeModifier {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            TypeModifier::Const => write!(f, "const"),
            TypeModifier::Volatile => write!(f, "volatile"),
            TypeModifier::RowMajor => write!(f, "row_major"),
            TypeModifier::ColumnMajor => write!(f, "column_major"),
            TypeModifier::Unorm => write!(f, "unorm"),
            TypeModifier::Snorm => write!(f, "snorm"),
            TypeModifier::In => write!(f, "in"),
            TypeModifier::Out => write!(f, "out"),
            TypeModifier::InOut => write!(f, "inout"),
            TypeModifier::Extern => write!(f, "extern"),
            TypeModifier::Static => write!(f, "static"),
            TypeModifier::GroupShared => write!(f, "groupshared"),
            TypeModifier::Precise => write!(f, "precise"),
            TypeModifier::NoInterpolation => write!(f, "nointerpolation"),
            TypeModifier::Linear => write!(f, "linear"),
            TypeModifier::Centroid => write!(f, "centroid"),
            TypeModifier::NoPerspective => write!(f, "noperspective"),
            TypeModifier::Sample => write!(f, "sample"),
            TypeModifier::Point => write!(f, "point"),
            TypeModifier::Line => write!(f, "line"),
            TypeModifier::Triangle => write!(f, "triangle"),
            TypeModifier::LineAdj => write!(f, "lineadj"),
            TypeModifier::TriangleAdj => write!(f, "triangleadj"),
            TypeModifier::Vertices => write!(f, "vertices"),
            TypeModifier::Primitives => write!(f, "primitives"),
            TypeModifier::Indices => write!(f, "indices"),
            TypeModifier::Payload => write!(f, "payload"),
            TypeModifier::AddressSpace(address_space) => write!(f, "{:?}", address_space),
        }
    }
}

impl std::fmt::Debug for AddressSpace {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            AddressSpace::Device => write!(f, "device"),
            AddressSpace::Constant => write!(f, "constant"),
            AddressSpace::Thread => write!(f, "thread"),
            AddressSpace::ThreadGroup => write!(f, "threadgroup"),
            AddressSpace::ThreadGroupImageBlock => write!(f, "threadgroup_imageblock"),
            AddressSpace::RayData => write!(f, "ray_data"),
            AddressSpace::ObjectData => write!(f, "object_data"),
        }
    }
}

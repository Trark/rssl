use crate::*;

/// A typed RSSL expression
#[derive(PartialEq, Debug, Clone)]
pub enum Expression {
    Literal(Constant),
    /// Reference to a variable in a local scope
    Variable(VariableId),
    /// Reference to a variable in the struct that owns this expression
    /// TODO: Non-string identifiers
    MemberVariable(String),
    /// Reference to a variable in a global scope
    Global(GlobalId),
    /// Reference to a variable in a constant buffer global
    ConstantVariable(ConstantBufferMemberId),
    /// Reference to an enum value
    EnumValue(EnumValueId),
    TernaryConditional(Box<Expression>, Box<Expression>, Box<Expression>),
    /// Chain of expressions
    Sequence(Vec<Expression>),
    Swizzle(Box<Expression>, Vec<SwizzleSlot>),
    MatrixSwizzle(Box<Expression>, Vec<MatrixSwizzleSlot>),
    ArraySubscript(Box<Expression>, Box<Expression>),
    Member(Box<Expression>, String),
    Call(FunctionId, CallType, Vec<Expression>),
    Constructor(TypeId, Vec<ConstructorSlot>),
    Cast(TypeId, Box<Expression>),
    SizeOf(TypeId),
    IntrinsicOp(IntrinsicOp, Vec<Expression>),
}

/// A single part of a vector or scalar swizzle operation
#[derive(PartialEq, Eq, Debug, Clone)]
pub enum SwizzleSlot {
    X, // x or r
    Y, // y or g
    Z, // z or b
    W, // w or a
}

/// A component in a single dimension of a matrix
#[derive(PartialEq, Eq, Debug, Copy, Clone)]
pub enum ComponentIndex {
    First,
    Second,
    Third,
    Forth,
}

/// A single part of a matrix swizzle operation
#[derive(PartialEq, Eq, Debug, Clone)]
pub struct MatrixSwizzleSlot(pub ComponentIndex, pub ComponentIndex);

/// Element passed to a numeric constructor
/// Constructors can take variable numbers of arguments depending on dimensions
/// of the types of the input expressions
#[derive(PartialEq, Debug, Clone)]
pub struct ConstructorSlot {
    /// Vector dimension or Matrix total element count or 1 for scalars
    pub arity: u32,
    /// The expression argument for this slot
    /// The type of this expression must be the scalar type of the numeric
    /// constructor this is used in with the arity above
    pub expr: Expression,
}

/// The form of a function invocation
#[derive(PartialEq, Eq, Debug, Clone)]
pub enum CallType {
    /// Call is for a free function
    FreeFunction,

    /// Call is for a method invocation where the first argument is the object
    MethodExternal,

    /// Call is for a method invocation from within a class scope
    MethodInternal,
}

use crate::*;
use rssl_text::Located;

/// A typed RSSL expression
#[derive(PartialEq, Debug, Clone)]
pub enum Expression {
    Literal(Literal),
    /// Reference to a variable in a local scope
    Variable(VariableRef),
    /// Reference to a variable in the struct that owns this expression
    /// TODO: Non-string identifiers
    MemberVariable(String),
    /// Reference to a variable in a global scope
    Global(GlobalId),
    /// Reference to a variable in a constant buffer global
    ConstantVariable(ConstantBufferId, String),
    TernaryConditional(Box<Expression>, Box<Expression>, Box<Expression>),
    /// Chain of expressions
    Sequence(Vec<Expression>),
    Swizzle(Box<Expression>, Vec<SwizzleSlot>),
    ArraySubscript(Box<Expression>, Box<Expression>),
    Member(Box<Expression>, String),
    Call(
        FunctionId,
        CallType,
        Vec<Located<TypeOrConstant>>,
        Vec<Expression>,
    ),
    Constructor(TypeId, Vec<ConstructorSlot>),
    Cast(TypeId, Box<Expression>),
    SizeOf(TypeId),
    Intrinsic(Intrinsic, Vec<Located<TypeOrConstant>>, Vec<Expression>),
}

/// A single part of a swizzle operation
#[derive(PartialEq, Eq, Debug, Clone)]
pub enum SwizzleSlot {
    X, // x or r
    Y, // y or g
    Z, // z or b
    W, // w or a
}

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

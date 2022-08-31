use crate::*;

#[derive(PartialEq, Debug, Clone)]
pub enum Expression {
    Literal(Literal),
    Variable(VariableRef),
    Global(GlobalId),
    ConstantVariable(ConstantBufferId, String),
    TernaryConditional(Box<Expression>, Box<Expression>, Box<Expression>),
    Swizzle(Box<Expression>, Vec<SwizzleSlot>),
    ArraySubscript(Box<Expression>, Box<Expression>),
    Member(Box<Expression>, String),
    Call(FunctionId, Vec<Expression>),
    Constructor(TypeLayout, Vec<ConstructorSlot>),
    Cast(Type, Box<Expression>),
    SizeOf(Type),
    Intrinsic(Intrinsic, Vec<Expression>),
}

#[derive(PartialEq, Debug, Clone)]
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

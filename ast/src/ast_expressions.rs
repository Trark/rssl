use crate::ast_types::Type;
use crate::primitive_types::DataLayout;
use rssl_text::Located;

#[derive(PartialEq, Debug, Clone)]
pub enum Expression {
    Literal(Literal),
    Variable(String),
    UnaryOperation(UnaryOp, Box<Located<Expression>>),
    BinaryOperation(BinOp, Box<Located<Expression>>, Box<Located<Expression>>),
    TernaryConditional(
        Box<Located<Expression>>,
        Box<Located<Expression>>,
        Box<Located<Expression>>,
    ),
    ArraySubscript(Box<Located<Expression>>, Box<Located<Expression>>),
    Member(Box<Located<Expression>>, String),
    Call(
        /// Function to invoke
        Box<Located<Expression>>,
        /// Template arguments
        Vec<Located<Type>>,
        /// Arguments
        Vec<Located<Expression>>,
    ),
    NumericConstructor(DataLayout, Vec<Located<Expression>>),
    Cast(Located<Type>, Box<Located<Expression>>),
    SizeOf(Located<Type>),
}

#[derive(PartialEq, Debug, Clone)]
pub enum Literal {
    Bool(bool),
    UntypedInt(u64),
    Int(u64),
    UInt(u64),
    Long(u64),
    Half(f32),
    Float(f32),
    Double(f64),
}

#[derive(PartialEq, Debug, Clone)]
pub enum UnaryOp {
    PrefixIncrement,
    PrefixDecrement,
    PostfixIncrement,
    PostfixDecrement,
    Plus,
    Minus,
    LogicalNot,
    BitwiseNot,
}

#[derive(PartialEq, Debug, Clone)]
pub enum BinOp {
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulus,
    LeftShift,
    RightShift,
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    BooleanAnd,
    BooleanOr,
    LessThan,
    LessEqual,
    GreaterThan,
    GreaterEqual,
    Equality,
    Inequality,
    Assignment,
    SumAssignment,
    DifferenceAssignment,
    ProductAssignment,
    QuotientAssignment,
    RemainderAssignment,
    Sequence,
}

use crate::*;
use rssl_text::*;

/// A block of statements with the local definition types and names
#[derive(PartialEq, Clone, Default, Debug)]
pub struct ScopeBlock(pub Vec<Statement>, pub ScopedDeclarations);

/// Set of active declarations in the current scope
#[derive(PartialEq, Clone, Default, Debug)]
#[allow(clippy::derive_partial_eq_without_eq)]
pub struct ScopedDeclarations {
    pub variables: Vec<VariableId>,
}

/// A typed RSSL statement with all metadata
#[derive(PartialEq, Debug, Clone)]
pub struct Statement {
    pub kind: StatementKind,
    pub location: SourceLocation,
    pub attributes: Vec<StatementAttribute>,
}

/// A typed RSSL statement
#[derive(PartialEq, Debug, Clone)]
pub enum StatementKind {
    Expression(Expression),
    Var(VarDef),
    Block(ScopeBlock),
    If(Expression, ScopeBlock),
    IfElse(Expression, ScopeBlock, ScopeBlock),
    For(ForInit, Option<Expression>, Option<Expression>, ScopeBlock),
    While(Expression, ScopeBlock),
    DoWhile(ScopeBlock, Expression),
    Switch(Expression, ScopeBlock),
    Break,
    Continue,
    Discard,
    Return(Option<Expression>),
    CaseLabel(Constant),
    DefaultLabel,
}

/// A local variable definition
#[derive(PartialEq, Debug, Clone)]
pub struct VarDef {
    /// Unique identifier for the local variable
    pub id: VariableId,

    /// Initializer for the local variable
    pub init: Option<Initializer>,
}

/// An initialiser for a for loop variable
#[derive(PartialEq, Debug, Clone)]
pub enum ForInit {
    Empty,
    Expression(Expression),
    Definitions(Vec<VarDef>),
}

/// The node for representing the initial value of a variable
#[derive(PartialEq, Debug, Clone)]
pub enum Initializer {
    /// Variable is initialized to the value of an expression
    Expression(Expression),
    /// Variable is initialized in parts (composite types and arrays)
    Aggregate(Vec<Initializer>),
}

/// An attribute that is applied to a statement
#[derive(PartialEq, Debug, Clone)]
#[allow(clippy::derive_partial_eq_without_eq)]
pub enum StatementAttribute {
    Branch,
    Flatten,
    Unroll(Option<u64>),
    Loop,
    Fastopt,
    AllowUavCondition,
}

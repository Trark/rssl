use crate::*;
use std::collections::HashMap;

/// A block of statements with the local definition types and names
#[derive(PartialEq, Clone, Default, Debug)]
pub struct ScopeBlock(pub Vec<Statement>, pub ScopedDeclarations);

/// Map of declarations in the current scope
#[derive(PartialEq, Clone, Default, Debug)]
#[allow(clippy::derive_partial_eq_without_eq)]
pub struct ScopedDeclarations {
    pub variables: HashMap<VariableId, (String, TypeId)>,
}

/// A typed RSSL statement
#[derive(PartialEq, Debug, Clone)]
pub enum Statement {
    Expression(Expression),
    Var(VarDef),
    Block(ScopeBlock),
    If(Expression, ScopeBlock),
    IfElse(Expression, ScopeBlock, ScopeBlock),
    For(ForInit, Expression, Expression, ScopeBlock),
    While(Expression, ScopeBlock),
    Break,
    Continue,
    Discard,
    Return(Option<Expression>),
}

/// A local variable definition
#[derive(PartialEq, Debug, Clone)]
pub struct VarDef {
    pub id: VariableId,
    pub local_type: LocalType,
    pub init: Option<Initializer>,
}

/// The type of any local variable declaration
#[derive(PartialEq, Eq, Debug, Clone)]
pub struct LocalType(pub TypeId, pub LocalStorage);

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

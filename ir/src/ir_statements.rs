use crate::*;
use std::collections::HashMap;

#[derive(PartialEq, Debug, Clone)]
pub struct ScopeBlock(pub Vec<Statement>, pub ScopedDeclarations);

/// Map of declarations in the current scope
#[derive(PartialEq, Debug, Clone)]
#[allow(clippy::derive_partial_eq_without_eq)]
pub struct ScopedDeclarations {
    pub variables: HashMap<VariableId, (String, Type)>,
}

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

#[derive(PartialEq, Debug, Clone)]
pub struct VarDef {
    pub id: VariableId,
    pub local_type: LocalType,
    pub init: Option<Initializer>,
}

/// The type of any local variable declaration
#[derive(PartialEq, Eq, Debug, Clone)]
pub struct LocalType(pub Type, pub LocalStorage);

#[derive(PartialEq, Debug, Clone)]
pub enum ForInit {
    Empty,
    Expression(Expression),
    Definitions(Vec<VarDef>),
}

#[derive(PartialEq, Debug, Clone)]
/// The node for representing the initial value of a variable
pub enum Initializer {
    /// Variable is initialized to the value of an expression
    Expression(Expression),
    /// Variable is initialized in parts (composite types and arrays)
    Aggregate(Vec<Initializer>),
}

impl From<Type> for LocalType {
    fn from(ty: Type) -> LocalType {
        LocalType(ty, LocalStorage::default())
    }
}

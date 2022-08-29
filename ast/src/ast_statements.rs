use crate::ast_expressions::Expression;
use crate::ast_types::Type;
use crate::primitive_types::*;
use rssl_text::Located;

#[derive(PartialEq, Debug, Clone)]
pub enum Statement {
    Empty,
    Expression(Located<Expression>),
    Var(VarDef),
    Block(Vec<Statement>),
    If(Located<Expression>, Box<Statement>),
    IfElse(Located<Expression>, Box<Statement>, Box<Statement>),
    For(
        InitStatement,
        Located<Expression>,
        Located<Expression>,
        Box<Statement>,
    ),
    While(Located<Expression>, Box<Statement>),
    Break,
    Continue,
    Return(Option<Located<Expression>>),
}

#[derive(PartialEq, Debug, Clone)]
pub enum InitStatement {
    Empty,
    Expression(Located<Expression>),
    Declaration(VarDef),
}

#[derive(PartialEq, Debug, Clone)]
pub struct VarDef {
    pub local_type: LocalType,
    pub defs: Vec<LocalVariableName>,
}

/// The type of any local variable declaration
#[derive(PartialEq, Debug, Clone)]
pub struct LocalType(
    pub Type,
    pub LocalStorage,
    pub Option<InterpolationModifier>,
);

#[derive(PartialEq, Debug, Clone)]
pub struct LocalVariableName {
    pub name: Located<String>,
    pub bind: VariableBind,
    pub init: Option<Initializer>,
}

#[derive(PartialEq, Debug, Clone)]
pub enum VariableBind {
    Normal,
    Array(Option<Located<Expression>>),
}

#[derive(PartialEq, Debug, Clone)]
/// The node for representing the initial value of a variable
pub enum Initializer {
    /// Variable is initialized to the value of an expression
    Expression(Located<Expression>),
    /// Variable is initialized in parts (composite types and arrays)
    Aggregate(Vec<Initializer>),
}

impl From<Type> for LocalType {
    fn from(ty: Type) -> LocalType {
        LocalType(ty, LocalStorage::default(), None)
    }
}

impl VarDef {
    pub fn one(name: Located<String>, local_type: LocalType) -> VarDef {
        VarDef {
            local_type,
            defs: vec![LocalVariableName {
                name,
                bind: VariableBind::Normal,
                init: None,
            }],
        }
    }
    pub fn one_with_expr(
        name: Located<String>,
        local_type: LocalType,
        expr: Located<Expression>,
    ) -> VarDef {
        VarDef {
            local_type,
            defs: vec![LocalVariableName {
                name,
                bind: VariableBind::Normal,
                init: Some(Initializer::Expression(expr)),
            }],
        }
    }
}

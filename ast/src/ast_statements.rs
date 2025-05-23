use crate::ast_declarations::{Declarator, InitDeclarator};
use crate::ast_expressions::Expression;
use crate::ast_types::Type;
use crate::*;
use rssl_text::*;

/// An RSSL statement with all metadata
#[derive(PartialEq, Debug, Clone)]
pub struct Statement {
    pub kind: StatementKind,
    pub location: SourceLocation,
    pub attributes: Vec<Attribute>,
}

/// An RSSL statement type
#[derive(PartialEq, Debug, Clone)]
pub enum StatementKind {
    Empty,
    Expression(Expression),
    Var(VarDef),
    AmbiguousDeclarationOrExpression(VarDef, Expression),
    Block(Vec<Statement>),
    If(Located<Expression>, Box<Statement>),
    IfElse(Located<Expression>, Box<Statement>, Box<Statement>),
    For(
        InitStatement,
        Option<Located<Expression>>,
        Option<Located<Expression>>,
        Box<Statement>,
    ),
    While(Located<Expression>, Box<Statement>),
    DoWhile(Box<Statement>, Located<Expression>),
    Switch(Located<Expression>, Box<Statement>),
    Break,
    Continue,
    Discard,
    Return(Option<Located<Expression>>),
    CaseLabel(Located<Expression>, Box<Statement>),
    DefaultLabel(Box<Statement>),
}

/// An initialiser for a for loop variable
#[derive(PartialEq, Debug, Clone)]
pub enum InitStatement {
    Empty,
    Expression(Located<Expression>),
    Declaration(VarDef),
}

/// A local variable definition
#[derive(PartialEq, Debug, Clone)]
pub struct VarDef {
    pub local_type: Type,
    pub defs: Vec<InitDeclarator>,
}

/// The node for representing the initial value of a variable
#[derive(PartialEq, Debug, Clone)]
pub enum Initializer {
    /// Variable is initialized to the value of an expression
    Expression(Located<Expression>),
    /// Variable is initialized in parts (composite types and arrays)
    Aggregate(Vec<Initializer>),
    /// Global is initialised with a static sampler
    StaticSampler(Vec<PipelineProperty>),
}

/// An attribute that is applied to a block of code
#[derive(PartialEq, Debug, Clone)]
pub struct Attribute {
    /// Name of the attribute
    pub name: Vec<Located<String>>,

    /// Arguments for the attribute
    pub arguments: Vec<Located<Expression>>,

    // If the attribute was declared with two brackets or one
    pub two_square_brackets: bool,
}

impl VarDef {
    pub fn one(name: Located<String>, local_type: Type) -> VarDef {
        VarDef {
            local_type,
            defs: Vec::from([InitDeclarator {
                declarator: Declarator::Identifier(ScopedIdentifier::unqualified(name), Vec::new()),
                location_annotations: Vec::new(),
                init: None,
            }]),
        }
    }

    pub fn one_with_expr(
        name: Located<String>,
        local_type: Type,
        expr: Located<Expression>,
    ) -> VarDef {
        VarDef {
            local_type,
            defs: Vec::from([InitDeclarator {
                declarator: Declarator::Identifier(ScopedIdentifier::unqualified(name), Vec::new()),
                location_annotations: Vec::new(),
                init: Some(Initializer::Expression(expr)),
            }]),
        }
    }
}

impl Attribute {
    pub fn numthreads(x: u64, y: u64, z: u64) -> Attribute {
        let x_node = Located::none(Expression::Literal(Literal::IntUntyped(x)));
        let y_node = Located::none(Expression::Literal(Literal::IntUntyped(y)));
        let z_node = Located::none(Expression::Literal(Literal::IntUntyped(z)));
        Attribute {
            name: Vec::from([Located::none("numthreads".to_string())]),
            arguments: Vec::from([x_node, y_node, z_node]),
            two_square_brackets: false,
        }
    }
}

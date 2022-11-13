use crate::ast_types::Type;
use crate::ast_types::TypeLayout;
use rssl_text::{Located, SourceLocation};

#[derive(PartialEq, Debug, Clone)]
pub enum Expression {
    Literal(Literal),
    Identifier(ScopedIdentifier),
    UnaryOperation(UnaryOp, Box<Located<Expression>>),
    BinaryOperation(BinOp, Box<Located<Expression>>, Box<Located<Expression>>),
    TernaryConditional(
        Box<Located<Expression>>,
        Box<Located<Expression>>,
        Box<Located<Expression>>,
    ),
    ArraySubscript(Box<Located<Expression>>, Box<Located<Expression>>),
    Member(Box<Located<Expression>>, ScopedIdentifier),
    Call(
        /// Function to invoke
        Box<Located<Expression>>,
        /// Template arguments
        Vec<Located<Type>>,
        /// Arguments
        Vec<Located<Expression>>,
    ),
    Constructor(TypeLayout, Vec<Located<Expression>>),
    Cast(Located<Type>, Box<Located<Expression>>),
    SizeOf(Located<Type>),
    /// Set of expressions which may be selected depending on known type names
    AmbiguousParseBranch(Vec<ConstrainedExpression>),
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

/// An identifier in an expression that may be qualified in a namespace or type
#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Clone)]
pub struct ScopedIdentifier {
    pub base: ScopedIdentifierBase,
    pub identifiers: Vec<Located<String>>,
}

/// Description for how a scoped identifier starts
#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Clone)]
pub enum ScopedIdentifierBase {
    /// Scoped name is not qualified to root namespace
    Relative,

    /// Scoped name is qualified to root namespace
    Absolute,
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

/// Expression which can only be activated when certain symbols are types
#[derive(PartialEq, Debug, Clone)]
pub struct ConstrainedExpression {
    /// The parsed expression
    pub expr: Located<Expression>,

    /// The set of type names we require for this path
    pub expected_type_names: Vec<ScopedIdentifier>,
}

impl ScopedIdentifier {
    /// Make a name from a single part and without location information
    pub fn trivial(name: &str) -> Self {
        ScopedIdentifier {
            base: ScopedIdentifierBase::Relative,
            identifiers: Vec::from([Located::none(name.to_string())]),
        }
    }

    /// Get the identifier if it is a single name without additional qualificatioon
    pub fn try_trivial(&self) -> Option<&Located<String>> {
        if self.base == ScopedIdentifierBase::Relative && self.identifiers.len() == 1 {
            Some(&self.identifiers[0])
        } else {
            None
        }
    }

    /// Get the location of the leaf part of the identifier
    pub fn get_location(&self) -> SourceLocation {
        self.identifiers.last().unwrap().location
    }

    /// Remove location information from a scoped name
    pub fn unlocate(mut self) -> ScopedIdentifier {
        for name in &mut self.identifiers {
            name.location = rssl_text::SourceLocation::UNKNOWN;
        }
        self
    }
}

impl From<Located<&str>> for ScopedIdentifier {
    fn from(unscoped_name: Located<&str>) -> Self {
        ScopedIdentifier {
            base: ScopedIdentifierBase::Relative,
            identifiers: Vec::from([Located::new(
                unscoped_name.node.to_string(),
                unscoped_name.location,
            )]),
        }
    }
}

impl std::fmt::Display for ScopedIdentifier {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        if self.base == ScopedIdentifierBase::Absolute {
            write!(f, "::")?;
        }
        let (last, scopes) = self.identifiers.split_last().unwrap();
        for scope in scopes {
            write!(f, "{}::", scope.node)?;
        }
        write!(f, "{}", last.node)
    }
}

impl std::fmt::Debug for ScopedIdentifier {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        if self.base == ScopedIdentifierBase::Absolute {
            write!(f, ":: ")?;
        }
        let (last, scopes) = self.identifiers.split_last().unwrap();
        for scope in scopes {
            write!(f, "{:?} :: ", scope)?;
        }
        write!(f, "{:?}", last)
    }
}

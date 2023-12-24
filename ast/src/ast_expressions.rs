use crate::ast_types::{Type, TypeId};
use rssl_text::{Locate, Located, SourceLocation};

/// An RSSL expression
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
    /// A function call or constructor invocation
    /// In the constructor case the function to invoke is an identifier for a type name
    Call(
        /// Function to invoke
        Box<Located<Expression>>,
        /// Template arguments
        Vec<ExpressionOrType>,
        /// Arguments
        Vec<Located<Expression>>,
    ),
    Cast(Box<TypeId>, Box<Located<Expression>>),
    /// sizeof() an expression or a direct type
    SizeOf(Box<ExpressionOrType>),
    /// Set of expressions which may be selected depending on known type names
    AmbiguousParseBranch(Vec<ConstrainedExpression>),
}

/// A literal value for any primitive type
#[derive(PartialEq, Debug, Clone)]
pub enum Literal {
    Bool(bool),
    IntUntyped(u64),
    IntUnsigned32(u64),
    IntUnsigned64(u64),
    IntSigned64(i64),
    FloatUntyped(f64),
    Float16(f32),
    Float32(f32),
    Float64(f64),
    String(String),
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

/// Any operator which takes a single expression as an argument
#[derive(PartialEq, Eq, Debug, Clone)]
pub enum UnaryOp {
    PrefixIncrement,
    PrefixDecrement,
    PostfixIncrement,
    PostfixDecrement,
    Plus,
    Minus,
    LogicalNot,
    BitwiseNot,
    Dereference,
    AddressOf,
}

/// Any operator which takes two expressions as arguments
#[derive(PartialEq, Eq, Debug, Clone)]
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
    LeftShiftAssignment,
    RightShiftAssignment,
    BitwiseAndAssignment,
    BitwiseOrAssignment,
    BitwiseXorAssignment,
    Sequence,
}

/// Either an expression or a type
#[derive(PartialEq, Clone)]
pub enum ExpressionOrType {
    /// Fragment must be an expression
    Expression(Located<Expression>),

    /// Fragment must be a type
    Type(TypeId),

    /// Fragment may be a type or an expression depending on context
    Either(Located<Expression>, TypeId),
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

    /// Make a name from a single part and with location information
    pub fn unqualified(name: Located<String>) -> Self {
        ScopedIdentifier {
            base: ScopedIdentifierBase::Relative,
            identifiers: Vec::from([name]),
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

    /// Remove location information from a scoped name
    pub fn unlocate(mut self) -> ScopedIdentifier {
        for name in &mut self.identifiers {
            name.location = rssl_text::SourceLocation::UNKNOWN;
        }
        self
    }
}

impl Locate for ScopedIdentifier {
    /// Get the location of the leaf part of the identifier
    fn get_location(&self) -> SourceLocation {
        self.identifiers.last().unwrap().location
    }
}

impl Locate for ExpressionOrType {
    fn get_location(&self) -> SourceLocation {
        match self {
            ExpressionOrType::Expression(expr) => expr.get_location(),
            ExpressionOrType::Type(ty) => ty.get_location(),
            ExpressionOrType::Either(ty, _) => ty.get_location(),
        }
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

impl From<Located<&str>> for ExpressionOrType {
    fn from(name: Located<&str>) -> Self {
        let location = name.location;
        ExpressionOrType::Either(
            Located::new(Expression::Identifier(name.clone().into()), name.location),
            TypeId::from(Type {
                layout: name.into(),
                modifiers: Default::default(),
                location,
            }),
        )
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
            write!(f, "{} @ {}::", scope.node, scope.location.get_raw())?;
        }
        write!(f, "{} @ {}", last.node, last.location.get_raw())
    }
}

impl std::fmt::Debug for ExpressionOrType {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            ExpressionOrType::Type(ty) => write!(f, "{ty:?}"),
            ExpressionOrType::Expression(expr) => write!(f, "{expr:?}"),
            ExpressionOrType::Either(expr, ty) => write!(f, "[{expr:?} | {ty:?}]"),
        }
    }
}

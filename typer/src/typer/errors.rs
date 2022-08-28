use crate::typer::functions::FunctionOverload;
use rssl_ast as ast;
use rssl_ir as ir;
use rssl_ir::ExpressionType;
use rssl_text::*;

pub type TyperResult<T> = Result<T, TyperError>;

#[derive(PartialEq, Debug, Clone)]
pub enum TyperError {
    Unimplemented,
    ExpressionSequenceOperatorNotImplemented,

    ValueAlreadyDefined(String, ErrorType, ErrorType),
    StructAlreadyDefined(String),
    ConstantBufferAlreadyDefined(String),

    ConstantSlotAlreadyUsed(ir::ConstantBufferId, ir::ConstantBufferId),
    ReadResourceSlotAlreadyUsed(ir::GlobalId, ir::GlobalId),
    ReadWriteResourceSlotAlreadyUsed(ir::GlobalId, ir::GlobalId),
    SamplerResourceSlotAlreadyUsed(ir::GlobalId, ir::GlobalId),

    UnknownIdentifier(String),
    UnknownType(ErrorType),

    TypeDoesNotHaveMembers(ErrorType),
    UnknownTypeMember(ErrorType, String),
    InvalidSwizzle(ErrorType, String),

    ArrayIndexingNonArrayType,
    ArraySubscriptIndexNotInteger,

    CallOnNonFunction,

    FunctionPassedToAnotherFunction(ErrorType, ErrorType),
    FunctionArgumentTypeMismatch(Vec<FunctionOverload>, Vec<ExpressionType>),
    NumericConstructorWrongArgumentCount,

    UnaryOperationWrongTypes(ast::UnaryOp, ErrorType),
    BinaryOperationWrongTypes(ast::BinOp, ErrorType, ErrorType),
    BinaryOperationNonNumericType,
    TernaryConditionRequiresBoolean(ErrorType),
    TernaryArmsMustHaveSameType(ErrorType, ErrorType),

    ExpectedValueExpression(ErrorType),

    InvalidCast(ErrorType, ErrorType),

    InitializerExpressionWrongType(ir::Type, ir::Type, SourceLocation),
    InitializerAggregateDoesNotMatchType,
    InitializerAggregateWrongDimension,
    InitializerAggregateWrongElementType,

    WrongTypeInConstructor,
    WrongTypeInReturnStatement,
    FunctionNotCalled,

    MutableRequired,
    LvalueRequired,
    ArrayDimensionsMustBeConstantExpression(ast::Expression),
    ArrayDimensionNotSpecified,

    /// Failed to find member of a constant buffer
    ConstantDoesNotExist(ir::ConstantBufferId, String),

    /// Failed to find member of a struct
    StructMemberDoesNotExist(ir::StructId, String),

    /// Swizzle is not allowed on the type
    InvalidTypeForSwizzle(ir::TypeLayout),

    /// Attempted to use member access on a non-composite type
    MemberNodeMustBeUsedOnStruct(ir::TypeLayout, String),

    /// Attempted to index into a non-indexable type
    ArrayIndexMustBeUsedOnArrayType(ir::TypeLayout),
}

#[derive(PartialEq, Debug, Clone)]
pub enum ErrorType {
    Untyped(ast::Type),
    Value(ir::Type),
    Function(String, Vec<FunctionOverload>),
    Method(String, ir::Type, Vec<FunctionOverload>),
    Unknown,
}

pub trait ToErrorType {
    fn to_error_type(&self) -> ErrorType;
}

impl ToErrorType for ir::Type {
    fn to_error_type(&self) -> ErrorType {
        ErrorType::Value(self.clone())
    }
}

impl ToErrorType for ir::ExpressionType {
    fn to_error_type(&self) -> ErrorType {
        ErrorType::Value(self.0.clone())
    }
}

impl TyperError {
    /// Get formatter to print the error
    pub fn display<'a>(&'a self, source_manager: &'a SourceManager) -> TyperErrorPrinter<'a> {
        TyperErrorPrinter(self, source_manager)
    }
}

impl std::fmt::Display for TyperError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            TyperError::Unimplemented => write!(f, "unimplemented"),
            TyperError::ExpressionSequenceOperatorNotImplemented => {
                write!(f, "operator ',' not implemented")
            }

            TyperError::ValueAlreadyDefined(_, _, _) => write!(f, "identifier already defined"),
            TyperError::StructAlreadyDefined(_) => write!(f, "struct aready defined"),
            TyperError::ConstantBufferAlreadyDefined(_) => write!(f, "cbuffer aready defined"),

            TyperError::ConstantSlotAlreadyUsed(_, _) => {
                write!(f, "global constant slot already used")
            }
            TyperError::ReadResourceSlotAlreadyUsed(_, _) => {
                write!(f, "global resource slot already used")
            }
            TyperError::ReadWriteResourceSlotAlreadyUsed(_, _) => {
                write!(f, "global writable resource slot already used")
            }
            TyperError::SamplerResourceSlotAlreadyUsed(_, _) => {
                write!(f, "sampler slot already used")
            }

            TyperError::UnknownIdentifier(name) => {
                write!(f, "'{}' was not declared in this scope", name)
            }
            TyperError::UnknownType(_) => write!(f, "unknown type name"),

            TyperError::TypeDoesNotHaveMembers(_) => {
                write!(f, "unknown member (type has no members)")
            }
            TyperError::UnknownTypeMember(_, _) => write!(f, "unknown member"),
            TyperError::InvalidSwizzle(_, _) => write!(f, "invalid swizzle"),

            TyperError::ArrayIndexingNonArrayType => {
                write!(f, "array index applied to non-array type")
            }
            TyperError::ArraySubscriptIndexNotInteger => {
                write!(f, "array subscripts must be integers")
            }

            TyperError::CallOnNonFunction => {
                write!(f, "function call applied to non-function type")
            }

            TyperError::FunctionPassedToAnotherFunction(_, _) => {
                write!(f, "functions can not be passed to other functions")
            }
            TyperError::FunctionArgumentTypeMismatch(overloads, types) => {
                // TODO: Proper naming
                let func_name = match &overloads[0].0 {
                    crate::typer::functions::FunctionName::User(id) => format!("<{}>", id.0),
                    _ => "<unknown>".to_string(),
                };
                write!(f, "Failed to find overload for {}(", func_name)?;
                if let Some((last_arg, not_last)) = types.split_last() {
                    for arg in not_last {
                        write!(f, "{:?}, ", arg.0)?;
                    }
                    write!(f, "{:?}", last_arg.0)?;
                }
                write!(f, ")")?;
                for overload in overloads {
                    write!(f, "\nCandidate function: {:?} {}(", overload.1, func_name)?;
                    if let Some((last_param, not_last)) = overload.2.split_last() {
                        for param in not_last {
                            write!(f, "{:?}, ", param)?;
                        }
                        write!(f, "{:?}", last_param)?;
                    }
                    write!(f, ")")?;
                }
                Ok(())
            }
            TyperError::NumericConstructorWrongArgumentCount => {
                write!(f, "wrong number of arguments to constructor")
            }

            TyperError::UnaryOperationWrongTypes(_, _) => {
                write!(f, "operation does not support the given types")
            }
            TyperError::BinaryOperationWrongTypes(_, _, _) => {
                write!(f, "operation does not support the given types")
            }
            TyperError::BinaryOperationNonNumericType => {
                write!(f, "non-numeric type in binary operation")
            }
            TyperError::TernaryConditionRequiresBoolean(_) => {
                write!(f, "ternary condition must be boolean")
            }
            TyperError::TernaryArmsMustHaveSameType(_, _) => {
                write!(f, "ternary arms must have the same type")
            }

            TyperError::ExpectedValueExpression(_) => write!(f, "expected a value expression"),

            TyperError::InvalidCast(_, _) => write!(f, "invalid cast"),

            TyperError::InitializerExpressionWrongType(actual, expected, _) => {
                // TODO: Custom type naming
                write!(
                    f,
                    "Variable of type {:?} was initialised with an expression of type {:?}",
                    expected, actual
                )
            }
            TyperError::InitializerAggregateDoesNotMatchType => {
                write!(f, "initializer does not match type")
            }
            TyperError::InitializerAggregateWrongDimension => {
                write!(f, "initializer has incorrect number of elements")
            }
            TyperError::InitializerAggregateWrongElementType => {
                write!(f, "initializer element has incorrect type")
            }

            TyperError::WrongTypeInConstructor => write!(f, "wrong type in numeric constructor"),
            TyperError::WrongTypeInReturnStatement => write!(f, "wrong type in return statement"),
            TyperError::FunctionNotCalled => write!(f, "function not called"),

            TyperError::MutableRequired => write!(f, "non-const is required in this context"),
            TyperError::LvalueRequired => write!(f, "lvalue is required in this context"),
            TyperError::ArrayDimensionsMustBeConstantExpression(_) => {
                write!(f, "array dimensions must be constant")
            }
            TyperError::ArrayDimensionNotSpecified => write!(f, "array not given any dimensions"),
            TyperError::ConstantDoesNotExist(_, name) => {
                write!(f, "Constant buffer does not contain member '{}'", name)
            }
            TyperError::StructMemberDoesNotExist(_, name) => {
                write!(f, "Struct does not contain member '{}'", name)
            }
            TyperError::InvalidTypeForSwizzle(_) => {
                write!(f, "Invalid use of swizzle operation")
            }
            TyperError::MemberNodeMustBeUsedOnStruct(_, name) => {
                write!(f, "Non-aggregate type can not contain member '{}'", name)
            }
            TyperError::ArrayIndexMustBeUsedOnArrayType(_) => {
                write!(f, "Non-indexable type can not be indexed")
            }
        }
    }
}

/// Prints typer errors
pub struct TyperErrorPrinter<'a>(&'a TyperError, &'a SourceManager);

impl<'a> std::fmt::Display for TyperErrorPrinter<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let TyperErrorPrinter(err, source_manager) = self;

        let loc_opt = match err {
            TyperError::InitializerExpressionWrongType(_, _, loc) => Some(*loc),
            _ => None,
        };

        if let Some(loc) = loc_opt {
            // Get file location info
            let file_location = source_manager.get_file_location(loc);

            // Print basic failure reason
            writeln!(f, "{}: {}", file_location, err)?;

            // Print source that caused the error
            source_manager.write_source_for_error(f, loc_opt)
        } else {
            // Print basic failure reason
            writeln!(f, "{}", err)
        }
    }
}

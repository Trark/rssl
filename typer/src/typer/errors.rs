use crate::typer::functions::FunctionOverload;
use crate::typer::scopes::Context;
use rssl_ast as ast;
use rssl_ir as ir;
use rssl_ir::ExpressionType;
use rssl_text::*;

#[derive(Debug, Clone)]
pub struct TyperExternalError(pub TyperError, pub Context);

pub type TyperResult<T> = Result<T, TyperError>;

#[derive(PartialEq, Debug, Clone)]
pub enum TyperError {
    ValueAlreadyDefined(String, ErrorType, ErrorType),
    StructAlreadyDefined(String),
    ConstantBufferAlreadyDefined(String),

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

    InvalidCast(ErrorType, ErrorType),

    InitializerExpressionWrongType(ir::Type, ir::Type, SourceLocation),
    InitializerAggregateDoesNotMatchType,
    InitializerAggregateWrongDimension,

    WrongTypeInConstructor,
    WrongTypeInReturnStatement(ir::Type, ir::Type),
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

impl TyperExternalError {
    /// Get formatter to print the error
    pub fn display<'a>(&'a self, source_manager: &'a SourceManager) -> TyperErrorPrinter<'a> {
        TyperErrorPrinter(self, source_manager)
    }
}

/// Prints typer errors
pub struct TyperErrorPrinter<'a>(&'a TyperExternalError, &'a SourceManager);

enum Severity {
    Error,
    Note,
}

impl<'a> std::fmt::Display for TyperErrorPrinter<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let TyperErrorPrinter(TyperExternalError(err, context), source_manager) = self;

        // Shared error message printing logic
        let mut write_message = |write: &dyn Fn(&mut std::fmt::Formatter) -> std::fmt::Result,
                                 loc_opt: Option<SourceLocation>,
                                 sev: Severity| {
            let sev_str = match sev {
                Severity::Error => "error",
                Severity::Note => "note",
            };
            if let Some(loc) = loc_opt {
                // Get file location info
                let file_location = source_manager.get_file_location(loc);

                // Print basic failure reason
                write!(f, "{}: {}: ", file_location, sev_str)?;
                write(f)?;
                writeln!(f)?;

                // Print source that caused the error
                source_manager.write_source_for_error(f, loc_opt)
            } else {
                // Print basic failure reason
                write!(f, "{}: ", sev_str)?;
                write(f)?;
                writeln!(f)
            }
        };

        match err {
            TyperError::ValueAlreadyDefined(_, _, _) => write_message(
                &|f| write!(f, "identifier already defined"),
                None,
                Severity::Error,
            ),
            TyperError::StructAlreadyDefined(_) => write_message(
                &|f| write!(f, "struct aready defined"),
                None,
                Severity::Error,
            ),
            TyperError::ConstantBufferAlreadyDefined(_) => write_message(
                &|f| write!(f, "cbuffer aready defined"),
                None,
                Severity::Error,
            ),
            TyperError::UnknownIdentifier(name) => write_message(
                &|f| write!(f, "'{}' was not declared in this scope", name),
                None,
                Severity::Error,
            ),
            TyperError::UnknownType(_) => {
                write_message(&|f| write!(f, "unknown type name"), None, Severity::Error)
            }
            TyperError::TypeDoesNotHaveMembers(_) => write_message(
                &|f| write!(f, "unknown member (type has no members)"),
                None,
                Severity::Error,
            ),
            TyperError::UnknownTypeMember(_, _) => {
                write_message(&|f| write!(f, "unknown member"), None, Severity::Error)
            }
            TyperError::InvalidSwizzle(_, _) => {
                write_message(&|f| write!(f, "invalid swizzle"), None, Severity::Error)
            }
            TyperError::ArrayIndexingNonArrayType => write_message(
                &|f| write!(f, "array index applied to non-array type"),
                None,
                Severity::Error,
            ),
            TyperError::ArraySubscriptIndexNotInteger => write_message(
                &|f| write!(f, "array subscripts must be integers"),
                None,
                Severity::Error,
            ),
            TyperError::CallOnNonFunction => write_message(
                &|f| write!(f, "function call applied to non-function type"),
                None,
                Severity::Error,
            ),
            TyperError::FunctionPassedToAnotherFunction(_, _) => write_message(
                &|f| write!(f, "functions can not be passed to other functions"),
                None,
                Severity::Error,
            ),
            TyperError::FunctionArgumentTypeMismatch(overloads, types) => {
                let func_name = get_function_name(&overloads[0].0, context);
                write_message(
                    &|f| {
                        write!(f, "no matching function for call to {}(", func_name)?;
                        if let Some((last_arg, not_last)) = types.split_last() {
                            for arg in not_last {
                                write!(f, "{:?}, ", arg.0)?;
                            }
                            write!(f, "{:?}", last_arg.0)?;
                        }
                        write!(f, ")")
                    },
                    None,
                    Severity::Error,
                )?;
                for overload in overloads {
                    write_message(
                        &|f| {
                            write!(
                                f,
                                "candidate function not viable: {:?} {}(",
                                overload.1, func_name
                            )?;
                            if let Some((last_param, not_last)) = overload.2.split_last() {
                                for param in not_last {
                                    write!(f, "{:?}, ", param)?;
                                }
                                write!(f, "{:?}", last_param)?;
                            }
                            write!(f, ")")
                        },
                        None,
                        Severity::Note,
                    )?;
                }
                Ok(())
            }
            TyperError::NumericConstructorWrongArgumentCount => write_message(
                &|f| write!(f, "wrong number of arguments to constructor"),
                None,
                Severity::Error,
            ),
            TyperError::UnaryOperationWrongTypes(_, _) => write_message(
                &|f| write!(f, "operation does not support the given types"),
                None,
                Severity::Error,
            ),
            TyperError::BinaryOperationWrongTypes(_, _, _) => write_message(
                &|f| write!(f, "operation does not support the given types"),
                None,
                Severity::Error,
            ),
            TyperError::BinaryOperationNonNumericType => write_message(
                &|f| write!(f, "non-numeric type in binary operation"),
                None,
                Severity::Error,
            ),
            TyperError::TernaryConditionRequiresBoolean(_) => write_message(
                &|f| write!(f, "ternary condition must be boolean"),
                None,
                Severity::Error,
            ),
            TyperError::TernaryArmsMustHaveSameType(_, _) => write_message(
                &|f| write!(f, "ternary arms must have the same type"),
                None,
                Severity::Error,
            ),
            TyperError::InvalidCast(_, _) => {
                write_message(&|f| write!(f, "invalid cast"), None, Severity::Error)
            }
            TyperError::InitializerExpressionWrongType(actual, expected, loc) => write_message(
                &|f| {
                    write!(
                        f,
                        "Variable of type '{}' was initialised with an expression of type '{}'",
                        get_type_string(expected, context),
                        get_type_string(actual, context),
                    )
                },
                Some(*loc),
                Severity::Error,
            ),
            TyperError::InitializerAggregateDoesNotMatchType => write_message(
                &|f| write!(f, "initializer does not match type"),
                None,
                Severity::Error,
            ),
            TyperError::InitializerAggregateWrongDimension => write_message(
                &|f| write!(f, "initializer has incorrect number of elements"),
                None,
                Severity::Error,
            ),
            TyperError::WrongTypeInConstructor => write_message(
                &|f| write!(f, "wrong type in numeric constructor"),
                None,
                Severity::Error,
            ),
            TyperError::WrongTypeInReturnStatement(actual, expected) => write_message(
                &|f| {
                    write!(
                        f,
                        "function return expected type {:?} but received {:?}",
                        expected, actual
                    )
                },
                None,
                Severity::Error,
            ),
            TyperError::FunctionNotCalled => {
                write_message(&|f| write!(f, "function not called"), None, Severity::Error)
            }
            TyperError::MutableRequired => write_message(
                &|f| write!(f, "non-const is required in this context"),
                None,
                Severity::Error,
            ),
            TyperError::LvalueRequired => write_message(
                &|f| write!(f, "lvalue is required in this context"),
                None,
                Severity::Error,
            ),
            TyperError::ArrayDimensionsMustBeConstantExpression(_) => write_message(
                &|f| write!(f, "array dimensions must be constant"),
                None,
                Severity::Error,
            ),
            TyperError::ArrayDimensionNotSpecified => write_message(
                &|f| write!(f, "array not given any dimensions"),
                None,
                Severity::Error,
            ),
            TyperError::ConstantDoesNotExist(_, name) => write_message(
                &|f| write!(f, "constant buffer does not contain member '{}'", name),
                None,
                Severity::Error,
            ),
            TyperError::StructMemberDoesNotExist(_, name) => write_message(
                &|f| write!(f, "struct does not contain member '{}'", name),
                None,
                Severity::Error,
            ),
            TyperError::InvalidTypeForSwizzle(_) => write_message(
                &|f| write!(f, "invalid use of swizzle operation"),
                None,
                Severity::Error,
            ),
            TyperError::MemberNodeMustBeUsedOnStruct(_, name) => write_message(
                &|f| write!(f, "non-aggregate type can not contain member '{}'", name),
                None,
                Severity::Error,
            ),
            TyperError::ArrayIndexMustBeUsedOnArrayType(_) => write_message(
                &|f| write!(f, "non-indexable type can not be indexed"),
                None,
                Severity::Error,
            ),
        }
    }
}

/// Get a string name from a function name for error display
fn get_function_name(name: &crate::typer::functions::FunctionName, context: &Context) -> String {
    use crate::typer::functions::FunctionName;
    match name {
        FunctionName::User(id) => match context.get_function_name(id) {
            Some(s) => s.to_string(),
            None => format!("<{:?}>", id),
        },
        FunctionName::Intrinsic(_) => "<unnamed intrinsic>".to_string(),
    }
}

/// Get a string name from a type for error display
fn get_type_string(ty: &ir::Type, context: &Context) -> String {
    format!("{:?}{}", ty.1, get_type_layout_string(&ty.0, context))
}

/// Get a string name from a type layout for error display
fn get_type_layout_string(tyl: &ir::TypeLayout, context: &Context) -> String {
    match tyl {
        ir::TypeLayout::Struct(ref sid) => get_struct_name(sid, context),
        ir::TypeLayout::Object(ref ot) => format!("{:?}", ot),
        ir::TypeLayout::Array(ref ty, ref len) => {
            format!("{}[{}]", get_type_layout_string(ty, context), len)
        }
        _ => format!("{:?}", tyl),
    }
}

/// Get a string name from a struct name for error display
fn get_struct_name(id: &ir::StructId, context: &Context) -> String {
    match context.get_struct_name(id) {
        Some(s) => s.to_string(),
        None => format!("struct<{:?}>", id),
    }
}

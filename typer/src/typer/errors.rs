use super::functions::Callable;
use super::functions::FunctionOverload;
use super::scopes::Context;
use rssl_ast as ast;
use rssl_ir as ir;
use rssl_text::*;

#[derive(Debug, Clone)]
pub struct TyperExternalError(pub TyperError, pub Context);

pub type TyperResult<T> = Result<T, TyperError>;

#[derive(PartialEq, Debug, Clone)]
pub enum TyperError {
    ValueAlreadyDefined(Located<String>, ErrorType, ErrorType),
    StructAlreadyDefined(Located<String>, ir::Type),
    ConstantBufferAlreadyDefined(Located<String>, ir::ConstantBufferId),
    TemplateTypeAlreadyDefined(Located<String>, ir::TemplateTypeId),

    UnknownIdentifier(String),
    UnknownType(ErrorType),

    TypeDoesNotHaveMembers(ErrorType),
    UnknownTypeMember(ErrorType, String),
    InvalidSwizzle(ErrorType, String),

    ArrayIndexingNonArrayType,
    ArraySubscriptIndexNotInteger,

    CallOnNonFunction,

    FunctionPassedToAnotherFunction(ErrorType, ErrorType),
    FunctionArgumentTypeMismatch(
        Vec<FunctionOverload>,
        Vec<ir::ExpressionType>,
        SourceLocation,
        bool,
    ),
    ConstructorWrongArgumentCount,

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
    Function(Vec<FunctionOverload>),
    Method(ir::Type, Vec<FunctionOverload>),
    Unknown,
}

impl From<&ast::ScopedName> for ErrorType {
    fn from(name: &ast::ScopedName) -> Self {
        ErrorType::Untyped(ast::Type::from_layout(ast::TypeLayout::Custom(
            name.clone(),
            Vec::new(),
        )))
    }
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
                                 loc: SourceLocation,
                                 sev: Severity| {
            let sev_str = match sev {
                Severity::Error => "error",
                Severity::Note => "note",
            };
            if loc != SourceLocation::UNKNOWN {
                // Get file location info
                let file_location = source_manager.get_file_location(loc);

                // Print basic failure reason
                write!(f, "{}: {}: ", file_location, sev_str)?;
                write(f)?;
                writeln!(f)?;

                // Print source that caused the error
                source_manager.write_source_for_error(f, Some(loc))
            } else {
                // Print basic failure reason
                write!(f, "{}: ", sev_str)?;
                write(f)?;
                writeln!(f)
            }
        };

        match err {
            TyperError::ValueAlreadyDefined(name, _, _) => write_message(
                &|f| write!(f, "redefinition of '{}'", name.node),
                name.location,
                Severity::Error,
            ),
            TyperError::StructAlreadyDefined(name, previous_type) => {
                write_message(
                    &|f| write!(f, "redefinition of '{}'", name.node),
                    name.location,
                    Severity::Error,
                )?;
                let previous_location = context.get_type_location(previous_type);
                if previous_location != SourceLocation::UNKNOWN {
                    write_message(
                        &|f| write!(f, "previous definition is here"),
                        previous_location,
                        Severity::Note,
                    )?;
                }
                Ok(())
            }
            TyperError::ConstantBufferAlreadyDefined(name, previous_id) => {
                write_message(
                    &|f| write!(f, "redefinition of '{}'", name.node),
                    name.location,
                    Severity::Error,
                )?;
                write_message(
                    &|f| write!(f, "previous definition is here"),
                    context.get_cbuffer_location(*previous_id),
                    Severity::Note,
                )
            }
            TyperError::TemplateTypeAlreadyDefined(name, previous_id) => {
                write_message(
                    &|f| write!(f, "redefinition of '{}'", name.node),
                    name.location,
                    Severity::Error,
                )?;
                write_message(
                    &|f| {
                        write!(
                            f,
                            "previous definition was with template type with index {}",
                            previous_id.0
                        )
                    },
                    SourceLocation::UNKNOWN,
                    Severity::Note,
                )
            }
            TyperError::UnknownIdentifier(name) => write_message(
                &|f| write!(f, "'{}' was not declared in this scope", name),
                SourceLocation::UNKNOWN,
                Severity::Error,
            ),
            TyperError::UnknownType(et) => write_message(
                &|f| write!(f, "unknown type name: {:?}", et),
                SourceLocation::UNKNOWN,
                Severity::Error,
            ),
            TyperError::TypeDoesNotHaveMembers(_) => write_message(
                &|f| write!(f, "unknown member (type has no members)"),
                SourceLocation::UNKNOWN,
                Severity::Error,
            ),
            TyperError::UnknownTypeMember(_, _) => write_message(
                &|f| write!(f, "unknown member"),
                SourceLocation::UNKNOWN,
                Severity::Error,
            ),
            TyperError::InvalidSwizzle(_, _) => write_message(
                &|f| write!(f, "invalid swizzle"),
                SourceLocation::UNKNOWN,
                Severity::Error,
            ),
            TyperError::ArrayIndexingNonArrayType => write_message(
                &|f| write!(f, "array index applied to non-array type"),
                SourceLocation::UNKNOWN,
                Severity::Error,
            ),
            TyperError::ArraySubscriptIndexNotInteger => write_message(
                &|f| write!(f, "array subscripts must be integers"),
                SourceLocation::UNKNOWN,
                Severity::Error,
            ),
            TyperError::CallOnNonFunction => write_message(
                &|f| write!(f, "function call applied to non-function type"),
                SourceLocation::UNKNOWN,
                Severity::Error,
            ),
            TyperError::FunctionPassedToAnotherFunction(_, _) => write_message(
                &|f| write!(f, "functions can not be passed to other functions"),
                SourceLocation::UNKNOWN,
                Severity::Error,
            ),
            TyperError::FunctionArgumentTypeMismatch(
                overloads,
                types,
                call_location,
                ambiguous_success,
            ) => {
                let func_name = context.get_function_or_intrinsic_name(&overloads[0].0);
                write_message(
                    &|f| {
                        if *ambiguous_success {
                            write!(f, "ambiguous call to {}(", func_name)?;
                        } else {
                            write!(f, "no matching function for call to {}(", func_name)?;
                        }
                        if let Some((last_arg, not_last)) = types.split_last() {
                            for arg in not_last {
                                write!(f, "{:?}, ", arg.0)?;
                            }
                            write!(f, "{:?}", last_arg.0)?;
                        }
                        write!(f, ")")
                    },
                    *call_location,
                    Severity::Error,
                )?;
                for overload in overloads {
                    let location = get_function_location(&overload.0, context);
                    write_message(
                        &|f| {
                            write!(
                                f,
                                "candidate function{}: {:?} {}(",
                                if *ambiguous_success {
                                    ""
                                } else {
                                    " not viable"
                                },
                                overload.1.return_type.return_type,
                                func_name
                            )?;
                            if let Some((last_param, not_last)) =
                                overload.1.param_types.split_last()
                            {
                                for param in not_last {
                                    write!(f, "{:?}, ", param)?;
                                }
                                write!(f, "{:?}", last_param)?;
                            }
                            write!(f, ")")
                        },
                        location,
                        Severity::Note,
                    )?;
                }
                Ok(())
            }
            TyperError::ConstructorWrongArgumentCount => write_message(
                &|f| write!(f, "wrong number of arguments to constructor"),
                SourceLocation::UNKNOWN,
                Severity::Error,
            ),
            TyperError::UnaryOperationWrongTypes(_, _) => write_message(
                &|f| write!(f, "operation does not support the given types"),
                SourceLocation::UNKNOWN,
                Severity::Error,
            ),
            TyperError::BinaryOperationWrongTypes(_, _, _) => write_message(
                &|f| write!(f, "operation does not support the given types"),
                SourceLocation::UNKNOWN,
                Severity::Error,
            ),
            TyperError::BinaryOperationNonNumericType => write_message(
                &|f| write!(f, "non-numeric type in binary operation"),
                SourceLocation::UNKNOWN,
                Severity::Error,
            ),
            TyperError::TernaryConditionRequiresBoolean(_) => write_message(
                &|f| write!(f, "ternary condition must be boolean"),
                SourceLocation::UNKNOWN,
                Severity::Error,
            ),
            TyperError::TernaryArmsMustHaveSameType(_, _) => write_message(
                &|f| write!(f, "ternary arms must have the same type"),
                SourceLocation::UNKNOWN,
                Severity::Error,
            ),
            TyperError::InvalidCast(_, _) => write_message(
                &|f| write!(f, "invalid cast"),
                SourceLocation::UNKNOWN,
                Severity::Error,
            ),
            TyperError::InitializerExpressionWrongType(actual, expected, loc) => write_message(
                &|f| {
                    write!(
                        f,
                        "Variable of type '{}' was initialised with an expression of type '{}'",
                        get_type_string(expected, context),
                        get_type_string(actual, context),
                    )
                },
                *loc,
                Severity::Error,
            ),
            TyperError::InitializerAggregateDoesNotMatchType => write_message(
                &|f| write!(f, "initializer does not match type"),
                SourceLocation::UNKNOWN,
                Severity::Error,
            ),
            TyperError::InitializerAggregateWrongDimension => write_message(
                &|f| write!(f, "initializer has incorrect number of elements"),
                SourceLocation::UNKNOWN,
                Severity::Error,
            ),
            TyperError::WrongTypeInConstructor => write_message(
                &|f| write!(f, "wrong type in numeric constructor"),
                SourceLocation::UNKNOWN,
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
                SourceLocation::UNKNOWN,
                Severity::Error,
            ),
            TyperError::FunctionNotCalled => write_message(
                &|f| write!(f, "function not called"),
                SourceLocation::UNKNOWN,
                Severity::Error,
            ),
            TyperError::MutableRequired => write_message(
                &|f| write!(f, "non-const is required in this context"),
                SourceLocation::UNKNOWN,
                Severity::Error,
            ),
            TyperError::LvalueRequired => write_message(
                &|f| write!(f, "lvalue is required in this context"),
                SourceLocation::UNKNOWN,
                Severity::Error,
            ),
            TyperError::ArrayDimensionsMustBeConstantExpression(_) => write_message(
                &|f| write!(f, "array dimensions must be constant"),
                SourceLocation::UNKNOWN,
                Severity::Error,
            ),
            TyperError::ArrayDimensionNotSpecified => write_message(
                &|f| write!(f, "array not given any dimensions"),
                SourceLocation::UNKNOWN,
                Severity::Error,
            ),
            TyperError::ConstantDoesNotExist(_, name) => write_message(
                &|f| write!(f, "constant buffer does not contain member '{}'", name),
                SourceLocation::UNKNOWN,
                Severity::Error,
            ),
            TyperError::StructMemberDoesNotExist(_, name) => write_message(
                &|f| write!(f, "struct does not contain member '{}'", name),
                SourceLocation::UNKNOWN,
                Severity::Error,
            ),
            TyperError::InvalidTypeForSwizzle(_) => write_message(
                &|f| write!(f, "invalid use of swizzle operation"),
                SourceLocation::UNKNOWN,
                Severity::Error,
            ),
            TyperError::MemberNodeMustBeUsedOnStruct(_, name) => write_message(
                &|f| write!(f, "non-aggregate type can not contain member '{}'", name),
                SourceLocation::UNKNOWN,
                Severity::Error,
            ),
            TyperError::ArrayIndexMustBeUsedOnArrayType(_) => write_message(
                &|f| write!(f, "non-indexable type can not be indexed"),
                SourceLocation::UNKNOWN,
                Severity::Error,
            ),
        }
    }
}

/// Get the location of a function
fn get_function_location(name: &Callable, context: &Context) -> SourceLocation {
    match *name {
        Callable::Function(id) => context.get_function_location(id),
        Callable::Intrinsic(_) => SourceLocation::UNKNOWN,
    }
}

/// Get a string name from a type for error display
fn get_type_string(ty: &ir::Type, context: &Context) -> String {
    format!("{:?}{}", ty.1, get_type_layout_string(&ty.0, context))
}

/// Get a string name from a type layout for error display
fn get_type_layout_string(tyl: &ir::TypeLayout, context: &Context) -> String {
    match *tyl {
        ir::TypeLayout::Struct(sid) => get_struct_name(sid, context),
        ir::TypeLayout::Object(ref ot) => format!("{:?}", ot),
        ir::TypeLayout::Array(ref ty, ref len) => {
            format!("{}[{}]", get_type_layout_string(ty, context), len)
        }
        _ => format!("{:?}", tyl),
    }
}

/// Get a string name from a struct name for error display
fn get_struct_name(id: ir::StructId, context: &Context) -> String {
    context.get_struct_name(id).to_string()
}

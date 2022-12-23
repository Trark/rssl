use super::scopes::Context;
use rssl_ast as ast;
use rssl_ir as ir;
use rssl_text::*;

#[derive(Debug, Clone)]
pub struct TyperExternalError(pub TyperError, pub Context);

pub type TyperResult<T> = Result<T, TyperError>;

/// An error that occurred when trying to type check the input RSSL
#[derive(PartialEq, Debug, Clone)]
pub enum TyperError {
    ValueAlreadyDefined(Located<String>, ErrorType, ErrorType),
    StructAlreadyDefined(Located<String>, ir::TypeId),
    ConstantBufferAlreadyDefined(Located<String>, ir::ConstantBufferId),
    TemplateTypeAlreadyDefined(Located<String>, ir::TemplateTypeId),

    UnknownIdentifier(ast::ScopedIdentifier),
    UnknownType(ErrorType, SourceLocation),

    TypeDoesNotHaveMembers(ErrorType, SourceLocation),
    /// Failed to find member of a type
    MemberDoesNotExist(ir::TypeId, ast::ScopedIdentifier),
    InvalidSwizzle(ir::TypeId, String, SourceLocation),
    /// Member identifier is part of a different type
    MemberIsForDifferentType(ir::TypeId, ir::TypeId, ast::ScopedIdentifier),
    /// Member identifier does not point to a type member
    IdentifierIsNotAMember(ir::TypeId, ast::ScopedIdentifier),

    ArrayIndexingNonArrayType(SourceLocation),
    ArraySubscriptIndexNotInteger(SourceLocation),

    CallOnNonFunction(SourceLocation),

    FunctionPassedToAnotherFunction(ErrorType, ErrorType, SourceLocation),
    FunctionArgumentTypeMismatch(
        Vec<ir::FunctionId>,
        Vec<ir::ExpressionType>,
        SourceLocation,
        bool,
    ),
    ConstructorWrongArgumentCount(SourceLocation),

    UnaryOperationWrongTypes(ast::UnaryOp, ErrorType, SourceLocation),
    BinaryOperationWrongTypes(ast::BinOp, ErrorType, ErrorType, SourceLocation),
    BinaryOperationNonNumericType(SourceLocation),
    TernaryConditionRequiresBoolean(ErrorType, SourceLocation),
    TernaryArmsMustHaveSameType(ErrorType, ErrorType, SourceLocation),

    InvalidCast(ErrorType, ErrorType, SourceLocation),

    InitializerExpressionWrongType(ir::TypeId, ir::TypeId, SourceLocation),
    InitializerAggregateDoesNotMatchType(ir::TypeId, SourceLocation),
    InitializerAggregateWrongDimension(SourceLocation),

    WrongTypeInConstructor(SourceLocation),
    WrongTypeInReturnStatement(ir::TypeId, ir::TypeId, SourceLocation),
    FunctionNotCalled(SourceLocation),

    MutableRequired(SourceLocation),
    LvalueRequired(SourceLocation),
    ArrayDimensionsMustBeConstantExpression(ast::Expression, SourceLocation),
    ArrayDimensionsMustBeNonZero(SourceLocation),
    ArrayDimensionNotSpecified(SourceLocation),

    /// Failed to find member of a constant buffer
    ConstantDoesNotExist(ir::ConstantBufferId, String, SourceLocation),

    /// Failed to find member of a struct
    StructMemberDoesNotExist(ir::StructId, String, SourceLocation),

    /// Identifier in an expression resolved as a type
    ExpectedExpressionReceivedType(ast::ScopedIdentifier, ir::TypeId),

    /// Identifier in a type context resolved as an expression
    ExpectedTypeReceivedExpression(ast::ScopedIdentifier),

    /// Swizzle is not allowed on the type
    InvalidTypeForSwizzle(ir::TypeId, SourceLocation),

    /// Attempted to use member access on a non-composite type
    MemberNodeMustBeUsedOnStruct(ir::TypeId, String, SourceLocation),

    /// Attempted to index into a non-indexable type
    ArrayIndexMustBeUsedOnArrayType(ir::TypeId, SourceLocation),

    /// Expression in a constant context could not be evaluated
    ExpressionIsNotConstantExpression(SourceLocation),

    /// A variable was declared with an incomplete type
    VariableHasIncompleteType(ir::TypeId, SourceLocation),

    /// A struct member was declared with const
    StructMemberMayNotBeConst(SourceLocation),

    /// Incorrect register type was used for a resource
    InvalidRegisterType(ir::RegisterType, ir::RegisterType, SourceLocation),

    /// Register annotation not allowed on type
    InvalidRegisterAnnotation(ir::TypeId, SourceLocation),
}

#[derive(PartialEq, Debug, Clone)]
pub enum ErrorType {
    Untyped(ast::Type),
    Value(ir::TypeId),
    Function(Vec<ir::FunctionId>),
    Method(ir::TypeId, Vec<ir::FunctionId>),
    Unknown,
}

impl From<&ast::ScopedIdentifier> for ErrorType {
    fn from(name: &ast::ScopedIdentifier) -> Self {
        ErrorType::Untyped(ast::Type::from_layout(ast::TypeLayout(
            name.clone(),
            Default::default(),
        )))
    }
}

pub trait ToErrorType {
    fn to_error_type(&self) -> ErrorType;
}

impl ToErrorType for ir::TypeId {
    fn to_error_type(&self) -> ErrorType {
        ErrorType::Value(*self)
    }
}

impl ToErrorType for ir::ExpressionType {
    fn to_error_type(&self) -> ErrorType {
        ErrorType::Value(self.0)
    }
}

impl CompileError for TyperExternalError {
    fn print(&self, w: &mut MessagePrinter) -> std::fmt::Result {
        let context = &self.1;
        match &self.0 {
            TyperError::ValueAlreadyDefined(name, _, _) => w.write_message(
                &|f| write!(f, "redefinition of '{}'", name.node),
                name.location,
                Severity::Error,
            ),
            TyperError::StructAlreadyDefined(name, previous_type) => {
                w.write_message(
                    &|f| write!(f, "redefinition of '{}'", name.node),
                    name.location,
                    Severity::Error,
                )?;
                let previous_location = context.module.get_type_location(*previous_type);
                if previous_location != SourceLocation::UNKNOWN {
                    w.write_message(
                        &|f| write!(f, "previous definition is here"),
                        previous_location,
                        Severity::Note,
                    )?;
                }
                Ok(())
            }
            TyperError::ConstantBufferAlreadyDefined(name, previous_id) => {
                w.write_message(
                    &|f| write!(f, "redefinition of '{}'", name.node),
                    name.location,
                    Severity::Error,
                )?;
                w.write_message(
                    &|f| write!(f, "previous definition is here"),
                    context.module.get_cbuffer_location(*previous_id),
                    Severity::Note,
                )
            }
            TyperError::TemplateTypeAlreadyDefined(name, previous_id) => {
                w.write_message(
                    &|f| write!(f, "redefinition of '{}'", name.node),
                    name.location,
                    Severity::Error,
                )?;
                w.write_message(
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
            TyperError::UnknownIdentifier(name) => w.write_message(
                &|f| write!(f, "'{}' was not declared in this scope", name),
                name.get_location(),
                Severity::Error,
            ),
            TyperError::UnknownType(et, loc) => w.write_message(
                &|f| write!(f, "unknown type name: {:?}", et),
                *loc,
                Severity::Error,
            ),
            TyperError::TypeDoesNotHaveMembers(_, loc) => w.write_message(
                &|f| write!(f, "unknown member (type has no members)"),
                *loc,
                Severity::Error,
            ),
            TyperError::MemberDoesNotExist(ty, name) => w.write_message(
                &|f| {
                    write!(
                        f,
                        "type '{}' does not contain member '{}'",
                        get_type_id_string(*ty, context),
                        name
                    )
                },
                name.get_location(),
                Severity::Error,
            ),
            TyperError::InvalidSwizzle(ty, swizzle, loc) => w.write_message(
                &|f| {
                    write!(
                        f,
                        "invalid swizzle '{}' on type '{}'",
                        swizzle,
                        get_type_id_string(*ty, context)
                    )
                },
                *loc,
                Severity::Error,
            ),
            TyperError::MemberIsForDifferentType(sampled_ty, found_tyl, path) => w.write_message(
                &|f| {
                    write!(
                        f,
                        "accessing member of type '{}' ('{}') on type '{}'",
                        get_type_id_string(*found_tyl, context),
                        path,
                        get_type_id_string(*sampled_ty, context),
                    )
                },
                path.get_location(),
                Severity::Error,
            ),
            TyperError::IdentifierIsNotAMember(ty, path) => w.write_message(
                &|f| {
                    write!(
                        f,
                        "identifier '{}' is not a member of type '{}'",
                        path,
                        get_type_id_string(*ty, context)
                    )
                },
                path.get_location(),
                Severity::Error,
            ),
            TyperError::ArrayIndexingNonArrayType(loc) => w.write_message(
                &|f| write!(f, "array index applied to non-array type"),
                *loc,
                Severity::Error,
            ),
            TyperError::ArraySubscriptIndexNotInteger(loc) => w.write_message(
                &|f| write!(f, "array subscripts must be integers"),
                *loc,
                Severity::Error,
            ),
            TyperError::CallOnNonFunction(loc) => w.write_message(
                &|f| write!(f, "function call applied to non-function type"),
                *loc,
                Severity::Error,
            ),
            TyperError::FunctionPassedToAnotherFunction(_, _, loc) => w.write_message(
                &|f| write!(f, "functions can not be passed to other functions"),
                *loc,
                Severity::Error,
            ),
            TyperError::FunctionArgumentTypeMismatch(
                overloads,
                types,
                call_location,
                ambiguous_success,
            ) => {
                let func_name = context.module.get_function_name(overloads[0]);
                w.write_message(
                    &|f| {
                        if *ambiguous_success {
                            write!(f, "ambiguous call to {}(", func_name)?;
                        } else {
                            write!(f, "no matching function for call to {}(", func_name)?;
                        }
                        if let Some((last_arg, not_last)) = types.split_last() {
                            for arg in not_last {
                                write!(f, "{}, ", get_type_id_string(arg.0, context))?;
                            }
                            write!(f, "{}", get_type_id_string(last_arg.0, context))?;
                        }
                        write!(f, ")")
                    },
                    *call_location,
                    Severity::Error,
                )?;
                for overload in overloads {
                    let location = get_function_location(*overload, context);
                    let signature = context
                        .module
                        .function_registry
                        .get_function_signature(*overload);
                    w.write_message(
                        &|f| {
                            write!(
                                f,
                                "candidate function{}: {} {}(",
                                if *ambiguous_success {
                                    ""
                                } else {
                                    " not viable"
                                },
                                get_type_id_string(signature.return_type.return_type, context),
                                func_name
                            )?;
                            if let Some((last_param, not_last)) = signature.param_types.split_last()
                            {
                                for param in not_last {
                                    write!(f, "{}, ", get_param_type_string(param, context))?;
                                }
                                write!(f, "{}", get_param_type_string(last_param, context))?;
                            }
                            write!(f, ")")
                        },
                        location,
                        Severity::Note,
                    )?;
                }
                Ok(())
            }
            TyperError::ConstructorWrongArgumentCount(loc) => w.write_message(
                &|f| write!(f, "wrong number of arguments to constructor"),
                *loc,
                Severity::Error,
            ),
            TyperError::UnaryOperationWrongTypes(_, _, loc) => w.write_message(
                &|f| write!(f, "operation does not support the given types"),
                *loc,
                Severity::Error,
            ),
            TyperError::BinaryOperationWrongTypes(_, _, _, loc) => w.write_message(
                &|f| write!(f, "operation does not support the given types"),
                *loc,
                Severity::Error,
            ),
            TyperError::BinaryOperationNonNumericType(loc) => w.write_message(
                &|f| write!(f, "non-numeric type in binary operation"),
                *loc,
                Severity::Error,
            ),
            TyperError::TernaryConditionRequiresBoolean(_, loc) => w.write_message(
                &|f| write!(f, "ternary condition must be boolean"),
                *loc,
                Severity::Error,
            ),
            TyperError::TernaryArmsMustHaveSameType(_, _, loc) => w.write_message(
                &|f| write!(f, "ternary arms must have the same type"),
                *loc,
                Severity::Error,
            ),
            TyperError::InvalidCast(_, _, loc) => {
                w.write_message(&|f| write!(f, "invalid cast"), *loc, Severity::Error)
            }
            TyperError::InitializerExpressionWrongType(actual, expected, loc) => w.write_message(
                &|f| {
                    write!(
                        f,
                        "variable of type '{}' was initialised with an expression of type '{}'",
                        get_type_id_string(*expected, context),
                        get_type_id_string(*actual, context),
                    )
                },
                *loc,
                Severity::Error,
            ),
            TyperError::InitializerAggregateDoesNotMatchType(ty, loc) => w.write_message(
                &|f| {
                    write!(
                        f,
                        "aggregate initializer does not match the members of type '{}'",
                        get_type_id_string(*ty, context)
                    )
                },
                *loc,
                Severity::Error,
            ),
            TyperError::InitializerAggregateWrongDimension(loc) => w.write_message(
                &|f| write!(f, "aggregate initializer has incorrect number of elements"),
                *loc,
                Severity::Error,
            ),
            TyperError::WrongTypeInConstructor(loc) => w.write_message(
                &|f| write!(f, "wrong type in numeric constructor"),
                *loc,
                Severity::Error,
            ),
            TyperError::WrongTypeInReturnStatement(actual, expected, loc) => w.write_message(
                &|f| {
                    write!(
                        f,
                        "function return expected type {:?} but received {:?}",
                        expected, actual
                    )
                },
                *loc,
                Severity::Error,
            ),
            TyperError::FunctionNotCalled(loc) => {
                w.write_message(&|f| write!(f, "function not called"), *loc, Severity::Error)
            }
            TyperError::MutableRequired(loc) => w.write_message(
                &|f| write!(f, "non-const is required in this context"),
                *loc,
                Severity::Error,
            ),
            TyperError::LvalueRequired(loc) => w.write_message(
                &|f| write!(f, "lvalue is required in this context"),
                *loc,
                Severity::Error,
            ),
            TyperError::ArrayDimensionsMustBeConstantExpression(_, loc) => w.write_message(
                &|f| write!(f, "array dimensions must be constant"),
                *loc,
                Severity::Error,
            ),
            TyperError::ArrayDimensionsMustBeNonZero(loc) => w.write_message(
                &|f| write!(f, "array dimensions must be non-zero"),
                *loc,
                Severity::Error,
            ),
            TyperError::ArrayDimensionNotSpecified(loc) => w.write_message(
                &|f| write!(f, "array not given any dimensions"),
                *loc,
                Severity::Error,
            ),
            TyperError::ConstantDoesNotExist(_, name, loc) => w.write_message(
                &|f| write!(f, "constant buffer does not contain member '{}'", name),
                *loc,
                Severity::Error,
            ),
            TyperError::StructMemberDoesNotExist(id, name, loc) => w.write_message(
                &|f| {
                    write!(
                        f,
                        "struct '{}' does not contain member '{}'",
                        get_struct_name(*id, context),
                        name
                    )
                },
                *loc,
                Severity::Error,
            ),
            TyperError::ExpectedExpressionReceivedType(name, _) => w.write_message(
                &|f| write!(f, "identifier '{}' is not expected to be a type", name),
                name.get_location(),
                Severity::Error,
            ),
            TyperError::ExpectedTypeReceivedExpression(name) => w.write_message(
                &|f| write!(f, "identifier '{}' is expected to be a type", name),
                name.get_location(),
                Severity::Error,
            ),
            TyperError::InvalidTypeForSwizzle(ty, loc) => w.write_message(
                &|f| {
                    write!(
                        f,
                        "invalid use of swizzle operation on type '{}'",
                        get_type_id_string(*ty, context)
                    )
                },
                *loc,
                Severity::Error,
            ),
            TyperError::MemberNodeMustBeUsedOnStruct(ty, name, loc) => w.write_message(
                &|f| {
                    write!(
                        f,
                        "non-aggregate type '{}' can not contain member '{}'",
                        get_type_id_string(*ty, context),
                        name
                    )
                },
                *loc,
                Severity::Error,
            ),
            TyperError::ArrayIndexMustBeUsedOnArrayType(ty, loc) => w.write_message(
                &|f| {
                    write!(
                        f,
                        "non-indexable type '{}' can not be indexed",
                        get_type_id_string(*ty, context),
                    )
                },
                *loc,
                Severity::Error,
            ),
            TyperError::ExpressionIsNotConstantExpression(loc) => w.write_message(
                &|f| {
                    write!(
                        f,
                        "expression could not be evaluated as a constant expression"
                    )
                },
                *loc,
                Severity::Error,
            ),
            TyperError::VariableHasIncompleteType(ty, loc) => w.write_message(
                &|f| {
                    write!(
                        f,
                        "Variable declared with incomplete type '{}'",
                        get_type_id_string(*ty, context)
                    )
                },
                *loc,
                Severity::Error,
            ),
            TyperError::StructMemberMayNotBeConst(loc) => w.write_message(
                &|f| write!(f, "struct member was declared const"),
                *loc,
                Severity::Error,
            ),
            TyperError::InvalidRegisterType(used, expected, loc) => w.write_message(
                &|f| {
                    write!(
                        f,
                        "invalid register type '{}' - expected '{}'",
                        used, expected
                    )
                },
                *loc,
                Severity::Error,
            ),
            TyperError::InvalidRegisterAnnotation(ty, loc) => w.write_message(
                &|f| {
                    write!(
                        f,
                        "register() is not allowed on '{}'",
                        get_type_id_string(*ty, context)
                    )
                },
                *loc,
                Severity::Error,
            ),
        }
    }
}

/// Get the location of a function
fn get_function_location(id: ir::FunctionId, context: &Context) -> SourceLocation {
    context.module.get_function_location(id)
}

/// Get a string name from a type id for error display
fn get_type_id_string(id: ir::TypeId, context: &Context) -> String {
    get_type_string(context.module.type_registry.get_type_layout(id), context)
}

/// Get a string name from a type for error display
fn get_type_string(tyl: &ir::TypeLayout, context: &Context) -> String {
    match *tyl {
        ir::TypeLayout::Struct(sid) => get_struct_name(sid, context),
        ir::TypeLayout::Object(ref ot) => get_object_type_string(ot, context),
        ir::TypeLayout::Array(ref ty, ref len) => {
            format!("{}[{}]", get_type_string(ty, context), len)
        }
        ir::TypeLayout::Modifier(modifier, ref ty) => {
            format!("{:?}{}", modifier, get_type_string(ty, context))
        }
        _ => format!("{:?}", tyl),
    }
}

/// Get a string name from an intrinsic object type for error display
fn get_object_type_string(object_type: &ir::ObjectType, context: &Context) -> String {
    use ir::ObjectType::*;
    match *object_type {
        Buffer(ty) => format!("Buffer<{}>", get_type_id_string(ty, context)),
        RWBuffer(ty) => format!("RWBuffer<{:?}>", get_type_id_string(ty, context)),
        ByteAddressBuffer => "ByteAddressBuffer".to_string(),
        RWByteAddressBuffer => "RWByteAddressBuffer".to_string(),
        BufferAddress => "BufferAddress".to_string(),
        RWBufferAddress => "RWBufferAddress".to_string(),
        StructuredBuffer(ty) => {
            format!("StructuredBuffer<{}>", get_type_id_string(ty, context))
        }
        RWStructuredBuffer(ty) => {
            format!("RWStructuredBuffer<{}>", get_type_id_string(ty, context))
        }
        Texture2D(ty) => format!("Texture2D<{}>", get_type_id_string(ty, context)),
        Texture2DMips(ty) => format!("Texture2D<{}>::Mips", get_type_id_string(ty, context)),
        Texture2DMipsSlice(ty) => {
            format!("Texture2D<{}>::MipsSlice", get_type_id_string(ty, context))
        }
        RWTexture2D(ty) => format!("RWTexture2D<{}>", get_type_id_string(ty, context)),
        ConstantBuffer(ty) => format!("ConstantBuffer<{}>", get_type_id_string(ty, context)),
        SamplerState => "SamplerState".to_string(),
        SamplerComparisonState => "SamplerComparisonState".to_string(),
    }
}

/// Get a string name from a param type for error display
fn get_param_type_string(param: &ir::ParamType, context: &Context) -> String {
    match &param.2 {
        None => format!("{:?} {}", param.1, get_type_id_string(param.0, context)),
        Some(m) => format!(
            "{:?} {:?} {}",
            param.1,
            m,
            get_type_id_string(param.0, context)
        ),
    }
}

/// Get a string name from a struct name for error display
fn get_struct_name(id: ir::StructId, context: &Context) -> String {
    context.module.get_struct_name(id).to_string()
}

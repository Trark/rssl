use super::errors::*;
use super::expressions::parse_expr;
use super::scopes::*;
use super::statements::{apply_variable_bind, parse_statement_list};
use super::types::{
    apply_template_type_substitution, is_illegal_variable_name, parse_input_modifier,
    parse_interpolation_modifier, parse_precise, parse_type_for_usage, TypePosition,
};
use rssl_ast as ast;
use rssl_ir as ir;
use rssl_text::Located;

/// Trait for applying template arguments onto another type
pub trait ApplyTemplates {
    /// Transforms a signature with template parameters with concrete arguments
    fn apply_templates(
        self,
        template_args: &[Located<ir::TypeOrConstant>],
        context: &mut Context,
    ) -> Self;
}

impl ApplyTemplates for ir::FunctionSignature {
    fn apply_templates(
        mut self,
        template_args: &[Located<ir::TypeOrConstant>],
        context: &mut Context,
    ) -> Self {
        for param_type in &mut self.param_types {
            param_type.type_id =
                apply_template_type_substitution(param_type.type_id, template_args, context);
        }

        self.return_type.return_type =
            apply_template_type_substitution(self.return_type.return_type, template_args, context);

        self
    }
}

/// Parse a function in a root context
pub fn parse_rootdefinition_function(
    fd: &ast::FunctionDefinition,
    context: &mut Context,
) -> TyperResult<ir::RootDefinition> {
    let ir_fd = parse_function(fd, context)?;
    Ok(ir::RootDefinition::Function(ir_fd))
}

/// Parse just the signature part of a function
pub fn parse_function_signature(
    fd: &ast::FunctionDefinition,
    object_type: Option<ir::StructId>,
    context: &mut Context,
) -> TyperResult<(ir::FunctionSignature, ScopeIndex)> {
    // Deny restricted non-keyword names
    if is_illegal_variable_name(&fd.name) {
        return Err(TyperError::IllegalFunctionName(fd.name.location));
    }

    // We being the scope now so we can use template arguments inside parameter types
    let scope = context.push_scope();

    if let Some(id) = object_type {
        context.set_owning_struct(id);
    }

    let mut template_param_count = 0;
    for template_param in &fd.template_params.0 {
        match template_param {
            ast::TemplateParam::Type(ty_param) => {
                context.insert_template_type(ty_param.name.clone())?;
            }
            ast::TemplateParam::Value(_) => {
                todo!("Non-type template arguments")
            }
        }
        template_param_count += 1;
    }

    let return_type = parse_returntype(&fd.returntype, context)?;

    // We save the return type of the current function for return statement parsing
    context.set_function_return_type(return_type.return_type);

    let param_types = {
        let mut vec = vec![];
        for param in &fd.params {
            let parsed_param = parse_paramtype(param, context)?;

            // const and other modifiers do not affect the signature type
            let type_id = strip_param_type(parsed_param.type_id, context);

            let var_type = ir::ParamType {
                type_id,
                input_modifier: parsed_param.input_modifier,
            };

            vec.push(var_type);
        }
        vec
    };

    context.pop_scope();

    Ok((
        ir::FunctionSignature {
            return_type,
            template_params: ir::TemplateParamCount(template_param_count),
            param_types,
        },
        scope,
    ))
}

/// Parse the rest of a function after previously having parsed the signature
pub fn parse_function_body(
    fd: &ast::FunctionDefinition,
    id: ir::FunctionId,
    #[allow(unused)] signature: ir::FunctionSignature,
    context: &mut Context,
) -> TyperResult<()> {
    context.revisit_function(id);

    let func_params = {
        let mut vec = Vec::new();
        for ast_param in &fd.params {
            let parsed_param = parse_paramtype(ast_param, context).unwrap();

            // Signature type should match reparsed type
            #[cfg(debug_assertions)]
            {
                let old_id = signature.param_types[vec.len()].type_id;
                let new_id = strip_param_type(parsed_param.type_id, context);
                debug_assert!(
                    old_id == new_id,
                    "{} != {}",
                    context.module.get_type_name_short(old_id),
                    context.module.get_type_name_short(new_id),
                );
                debug_assert!(
                    signature.param_types[vec.len()].input_modifier == parsed_param.input_modifier
                );
            }

            // Register the parameter in the module
            let var_id =
                context
                    .module
                    .variable_registry
                    .register_local_variable(ir::LocalVariable {
                        name: ast_param.name.clone(),
                        type_id: parsed_param.type_id,
                        storage_class: ir::LocalStorage::Local,
                        precise: parsed_param.precise,
                    });

            // Register the parameter in the scope
            context.insert_variable(ast_param.name.clone(), var_id, parsed_param.type_id)?;

            vec.push(ir::FunctionParam {
                id: var_id,
                param_type: ir::ParamType {
                    type_id: parsed_param.type_id,
                    input_modifier: parsed_param.input_modifier,
                },
                interpolation_modifier: parsed_param.interpolation_modifier,
                precise: parsed_param.precise,
                semantic: ast_param.semantic.clone(),
            });
        }
        vec
    };

    let attributes = parse_function_attributes(&fd.attributes, context)?;

    // Parse the function
    let body_ir = parse_statement_list(&fd.body, context)?;
    let decls = context.pop_scope_with_locals();

    let def = ir::FunctionImplementation {
        params: func_params,
        scope_block: ir::ScopeBlock(body_ir, decls),
        attributes,
    };

    context.module.function_registry.set_implementation(id, def);

    Ok(())
}

fn parse_function(
    fd: &ast::FunctionDefinition,
    context: &mut Context,
) -> TyperResult<ir::FunctionId> {
    let (signature, scope) = parse_function_signature(fd, None, context)?;

    // Register the function signature
    let id = context.register_function(fd.name.clone(), signature.clone(), scope, fd.clone())?;
    context.add_function_to_current_scope(id)?;

    if signature.template_params.0 == 0 {
        parse_function_body(fd, id, signature, context)?;
    } else {
        let attributes = parse_function_attributes(&fd.attributes, context)?;
        let def = ir::FunctionImplementation {
            params: Default::default(),
            scope_block: ir::ScopeBlock(
                Default::default(),
                ir::ScopedDeclarations {
                    variables: Default::default(),
                },
            ),
            attributes,
        };
        context.module.function_registry.set_implementation(id, def);
    };

    Ok(id)
}

fn parse_returntype(
    return_type: &ast::FunctionReturn,
    context: &mut Context,
) -> TyperResult<ir::FunctionReturn> {
    let ty = parse_type_for_usage(&return_type.return_type, TypePosition::Return, context)?;

    // Deny storage class modifiers except static
    // These apply to function not the return type - and static is the only option so there is nothing to set in the result
    for modifier in &return_type.return_type.modifiers.modifiers {
        match &modifier.node {
            ast::TypeModifier::Extern | ast::TypeModifier::GroupShared => {
                return Err(TyperError::ModifierNotSupported(
                    modifier.node,
                    modifier.location,
                    TypePosition::Local,
                ))
            }
            _ => continue,
        }
    }

    Ok(ir::FunctionReturn {
        return_type: ty,
        semantic: return_type.semantic.clone(),
    })
}

struct ParsedParam {
    type_id: ir::TypeId,
    input_modifier: ir::InputModifier,
    interpolation_modifier: Option<ir::InterpolationModifier>,
    precise: bool,
}

/// Parse a type used for a function parameter
fn parse_paramtype(param: &ast::FunctionParam, context: &mut Context) -> TyperResult<ParsedParam> {
    let param_type = &param.param_type;
    let ty = parse_type_for_usage(param_type, TypePosition::Parameter, context)?;

    let ty_unmodified = context.module.type_registry.remove_modifier(ty);
    let tyl = context.module.type_registry.get_type_layer(ty_unmodified);
    if tyl == ir::TypeLayer::Void {
        return Err(TyperError::VariableHasIncompleteType(
            ty,
            param_type.location,
        ));
    }

    let input_modifier = parse_input_modifier(&param_type.modifiers)?;
    let interpolation_modifier = parse_interpolation_modifier(&param_type.modifiers)?;

    if let Some((interpolation_modifier, modifier_location)) = interpolation_modifier {
        // Require in modifier for mesh shader inputs
        if interpolation_modifier == ir::InterpolationModifier::Payload
            && input_modifier != Some(ir::InputModifier::In)
        {
            return Err(TyperError::InterpolationModifierRequiresInputModifier(
                interpolation_modifier,
                modifier_location,
                ir::InputModifier::In,
            ));
        }

        // Require out modifier for mesh shader outputs
        if matches!(
            interpolation_modifier,
            ir::InterpolationModifier::Vertices
                | ir::InterpolationModifier::Primitives
                | ir::InterpolationModifier::Indices
        ) && input_modifier != Some(ir::InputModifier::Out)
        {
            return Err(TyperError::InterpolationModifierRequiresInputModifier(
                interpolation_modifier,
                modifier_location,
                ir::InputModifier::Out,
            ));
        }

        if interpolation_modifier == ir::InterpolationModifier::Indices
            && !(matches!(
                tyl,
                ir::TypeLayer::Vector(_, 2) | ir::TypeLayer::Vector(_, 3)
            ) && context.module.type_registry.extract_scalar(ty_unmodified)
                == Some(ir::ScalarType::UInt))
        {
            return Err(TyperError::MeshShaderIndicesRequiresIndexType(
                param_type.location,
                ty,
            ));
        }
    }

    // Calculate if we are precise
    let precise_result = parse_precise(&param_type.modifiers)?;

    // Set default input modifier
    let input_modifier = input_modifier.unwrap_or(ir::InputModifier::In);

    // Remove interpolation modifier location
    let interpolation_modifier = interpolation_modifier.map(|(im, _)| im);

    // If the parameter has type information bound to the name then apply it to the type now
    let type_id = apply_variable_bind(ty, param.name.location, &param.bind, &None, false, context)?;

    Ok(ParsedParam {
        type_id,
        input_modifier,
        interpolation_modifier,
        precise: precise_result.is_some(),
    })
}

/// Remove modifiers like const from param type as seen from signature
fn strip_param_type(id: ir::TypeId, context: &mut Context) -> ir::TypeId {
    let top_layer = context.module.type_registry.get_type_layer(id);

    match top_layer {
        ir::TypeLayer::Array(inner_id, len) => {
            // Strip the inner type
            let stripped = strip_param_type(inner_id, context);
            // Recombine the array back on the stripped type
            context
                .module
                .type_registry
                .register_type(ir::TypeLayer::Array(stripped, len))
        }
        // Modifiers appear after array so we do not need to recurve to remove more modifiers
        ir::TypeLayer::Modifier(_, inner_id) => inner_id,
        _ => id,
    }
}

/// Process all function attributes
fn parse_function_attributes(
    ast_attributes: &[ast::Attribute],
    context: &mut Context,
) -> TyperResult<Vec<ir::FunctionAttribute>> {
    let mut ir_attributes = Vec::new();
    for ast_attribute in ast_attributes {
        ir_attributes.push(parse_function_attribute(ast_attribute, context)?);
    }
    Ok(ir_attributes)
}

/// Process a single function attribute
fn parse_function_attribute(
    attribute: &ast::Attribute,
    context: &mut Context,
) -> TyperResult<ir::FunctionAttribute> {
    match attribute.name.to_lowercase().as_str() {
        "numthreads" => {
            if attribute.arguments.len() == 3 {
                let x = parse_expr(&attribute.arguments[0], context)?.0;
                let y = parse_expr(&attribute.arguments[1], context)?.0;
                let z = parse_expr(&attribute.arguments[2], context)?.0;
                Ok(ir::FunctionAttribute::NumThreads(x, y, z))
            } else {
                Err(TyperError::FunctionAttributeUnexpectedArgumentCount(
                    attribute.name.node.clone(),
                    attribute.name.location,
                ))
            }
        }
        "wavesize" => {
            if attribute.arguments.len() == 1 {
                let size = parse_expr(&attribute.arguments[0], context)?.0;
                Ok(ir::FunctionAttribute::WaveSize(size))
            } else {
                Err(TyperError::FunctionAttributeUnexpectedArgumentCount(
                    attribute.name.node.clone(),
                    attribute.name.location,
                ))
            }
        }
        "outputtopology" => {
            if attribute.arguments.len() == 1 {
                match &attribute.arguments[0].node {
                    ast::Expression::Literal(ast::Literal::String(s)) => {
                        Ok(ir::FunctionAttribute::OutputTopology(s.clone()))
                    }
                    _ => Err(TyperError::FunctionAttributeUnexpectedArgumentCount(
                        attribute.name.node.clone(),
                        attribute.name.location,
                    )),
                }
            } else {
                Err(TyperError::FunctionAttributeUnexpectedArgumentCount(
                    attribute.name.node.clone(),
                    attribute.name.location,
                ))
            }
        }
        _ => Err(TyperError::FunctionAttributeUnknown(
            attribute.name.node.clone(),
            attribute.name.location,
        )),
    }
}

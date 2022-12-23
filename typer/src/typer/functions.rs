use super::errors::*;
use super::expressions::parse_expr;
use super::scopes::*;
use super::statements::{apply_variable_bind, parse_statement_list};
use super::types::{apply_template_type_substitution, parse_type};
use rssl_ast as ast;
use rssl_ir as ir;
use rssl_text::Located;

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct FunctionOverload(pub ir::FunctionId, pub ir::FunctionSignature);

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
            param_type.0 = apply_template_type_substitution(param_type.0, template_args, context);
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
    // We being the scope now so we can use template arguments inside parameter types
    let scope = context.push_scope();

    if let Some(id) = object_type {
        context.set_owning_struct(id);
    }

    let mut template_param_count = 0;
    for template_param in &fd.template_params.0 {
        context.insert_template_type(template_param.clone())?;
        template_param_count += 1;
    }

    let return_type = parse_returntype(&fd.returntype, context)?;

    // We save the return type of the current function for return statement parsing
    context.set_function_return_type(return_type.return_type);

    let param_types = {
        let mut vec = vec![];
        for param in &fd.params {
            let mut var_type = parse_paramtype(&param.param_type, context)?;

            let type_layout = context
                .module
                .type_registry
                .get_type_layout(var_type.0)
                .clone();

            // If the parameter has type information bound to the name then apply it to the type now
            let type_layout = apply_variable_bind(type_layout, &param.bind, &None, context)?;

            var_type.0 = context.module.type_registry.register_type(type_layout);

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
    signature: ir::FunctionSignature,
    context: &mut Context,
) -> TyperResult<()> {
    context.revisit_function(id);

    let func_params = {
        let mut vec = Vec::new();
        for (var_type, ast_param) in signature.param_types.into_iter().zip(&fd.params) {
            let var_id = context.insert_variable(ast_param.name.clone(), var_type.0)?;
            vec.push(ir::FunctionParam {
                id: var_id,
                param_type: var_type,
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
    let ty = parse_type(&return_type.return_type, context)?;

    Ok(ir::FunctionReturn {
        return_type: ty,
        semantic: return_type.semantic.clone(),
    })
}

fn parse_paramtype(
    param_type: &ast::ParamType,
    context: &mut Context,
) -> TyperResult<ir::ParamType> {
    let ty = parse_type(&param_type.0, context)?;

    let ty_unmodified = context.module.type_registry.remove_modifier(ty);
    let tyl = context.module.type_registry.get_type_layout(ty_unmodified);
    if tyl.is_void() {
        return Err(TyperError::VariableHasIncompleteType(
            ty,
            param_type.0.location,
        ));
    }

    Ok(ir::ParamType(ty, param_type.1, param_type.2.clone()))
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
        _ => Err(TyperError::FunctionAttributeUnknown(
            attribute.name.node.clone(),
            attribute.name.location,
        )),
    }
}

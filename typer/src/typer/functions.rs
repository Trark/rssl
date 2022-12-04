use super::errors::*;
use super::expressions::parse_expr;
use super::scopes::*;
use super::statements::{apply_variable_bind, parse_statement_list};
use super::types::{apply_template_type_substitution, parse_type};
use rssl_ast as ast;
use rssl_ir as ir;
use rssl_text::Located;

#[derive(PartialEq, Debug, Clone)]
pub enum Callable {
    Function(ir::FunctionId),
    Intrinsic(crate::intrinsics::IntrinsicFactory),
}

#[derive(PartialEq, Debug, Clone)]
pub struct FunctionOverload(pub Callable, pub FunctionSignature);

/// Describes the signature for a function
#[derive(PartialEq, Eq, Debug, Clone)]
pub struct FunctionSignature {
    // TODO: Function and parameter names
    pub return_type: ir::FunctionReturn,
    pub template_params: ir::TemplateParamCount,
    pub param_types: Vec<ir::ParamType>,
}

impl FunctionSignature {
    /// Transforms a signature with template parameters with concrete arguments
    pub fn apply_templates(mut self, template_args: &[Located<ir::TypeOrConstant>]) -> Self {
        for param_type in &mut self.param_types {
            let ty = param_type.0.clone();
            param_type.0 = apply_template_type_substitution(ty, template_args);
        }

        self.return_type.return_type =
            apply_template_type_substitution(self.return_type.return_type, template_args);

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
) -> TyperResult<(FunctionSignature, ScopeIndex)> {
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
    context.set_function_return_type(return_type.return_type.clone());

    let param_types = {
        let mut vec = vec![];
        for param in &fd.params {
            let mut var_type = parse_paramtype(&param.param_type, context)?;

            // If the parameter has type information bound to the name then apply it to the type now
            var_type.0 = apply_variable_bind(var_type.0, &param.bind, &None)?;

            vec.push(var_type);
        }
        vec
    };

    context.pop_scope();

    Ok((
        FunctionSignature {
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
    signature: FunctionSignature,
    context: &mut Context,
) -> TyperResult<()> {
    context.revisit_function(id);

    let return_type = signature.return_type;

    let func_params = {
        let mut vec = Vec::new();
        for (var_type, ast_param) in signature.param_types.into_iter().zip(&fd.params) {
            let var_id = context.insert_variable(ast_param.name.clone(), var_type.0.clone())?;
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

    let def = ir::FunctionDefinition {
        id,
        returntype: return_type,
        params: func_params,
        scope_block: ir::ScopeBlock(body_ir, decls),
        attributes,
    };

    context.module.function_registry[id.0 as usize] = Some(def);
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
        let def = ir::FunctionDefinition {
            id,
            returntype: signature.return_type,
            params: Default::default(),
            scope_block: ir::ScopeBlock(
                Default::default(),
                ir::ScopedDeclarations {
                    variables: Default::default(),
                },
            ),
            attributes,
        };
        context.module.function_registry[id.0 as usize] = Some(def);
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
    if ty.is_void() {
        return Err(TyperError::VariableHasIncompleteType(
            ty,
            param_type.0.location,
        ));
    }
    Ok(ir::ParamType(
        ty,
        param_type.1.clone(),
        param_type.2.clone(),
    ))
}

/// Process all function attributes
fn parse_function_attributes(
    ast_attributes: &[ast::FunctionAttribute],
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
    attribute: &ast::FunctionAttribute,
    context: &mut Context,
) -> TyperResult<ir::FunctionAttribute> {
    Ok(match attribute {
        ast::FunctionAttribute::NumThreads(x, y, z) => {
            let x = parse_expr(x, context)?.0;
            let y = parse_expr(y, context)?.0;
            let z = parse_expr(z, context)?.0;
            ir::FunctionAttribute::NumThreads(x, y, z)
        }
    })
}

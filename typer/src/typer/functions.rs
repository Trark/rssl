use super::errors::*;
use super::scopes::*;
use super::statements::parse_statement_list;
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
#[derive(PartialEq, Debug, Clone)]
pub struct FunctionSignature {
    // TODO: Function and parameter names
    pub return_type: ir::FunctionReturn,
    pub template_params: ir::TemplateParamCount,
    pub param_types: Vec<ir::ParamType>,
}

impl FunctionSignature {
    /// Transforms a signature with template parameters with concrete arguments
    pub fn apply_templates(mut self, template_args: &[Located<ir::Type>]) -> Self {
        for param_type in &mut self.param_types {
            let ty = param_type.0.clone();
            (*param_type).0 = apply_template_type_substitution(ty, template_args);
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
            let var_type = parse_paramtype(&param.param_type, context)?;
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
) -> TyperResult<ir::FunctionDefinition> {
    context.revisit_function(id);

    let return_type = signature.return_type;

    let func_params = {
        let mut vec = Vec::new();
        for (var_type, ast_param) in signature.param_types.into_iter().zip(&fd.params) {
            let var_id = context.insert_variable(ast_param.name.clone(), var_type.0.clone())?;
            vec.push(ir::FunctionParam {
                id: var_id,
                param_type: var_type,
            });
        }
        vec
    };

    // Parse the function
    let body_ir = parse_statement_list(&fd.body, context)?;
    let decls = context.pop_scope_with_locals();

    let fd_ir = ir::FunctionDefinition {
        id,
        returntype: return_type,
        params: func_params,
        scope_block: ir::ScopeBlock(body_ir, decls),
        attributes: fd.attributes.clone(),
    };
    Ok(fd_ir)
}

fn parse_function(
    fd: &ast::FunctionDefinition,
    context: &mut Context,
) -> TyperResult<ir::FunctionDefinition> {
    let (signature, scope) = parse_function_signature(fd, None, context)?;

    // Register the function signature
    let id = context.register_function(fd.name.clone(), signature.clone(), scope, fd.clone())?;
    context.add_function_to_current_scope(id)?;

    if signature.template_params.0 == 0 {
        parse_function_body(fd, id, signature, context)
    } else {
        Ok(ir::FunctionDefinition {
            id,
            returntype: signature.return_type,
            params: Default::default(),
            scope_block: ir::ScopeBlock(
                Default::default(),
                ir::ScopedDeclarations {
                    variables: Default::default(),
                },
            ),
            attributes: fd.attributes.clone(),
        })
    }
}

fn parse_returntype(
    return_type: &ast::FunctionReturn,
    context: &Context,
) -> TyperResult<ir::FunctionReturn> {
    let ty = parse_type(&return_type.return_type, context)?;
    Ok(ir::FunctionReturn { return_type: ty })
}

fn parse_paramtype(param_type: &ast::ParamType, context: &Context) -> TyperResult<ir::ParamType> {
    let ty = parse_type(&param_type.0, context)?;
    Ok(ir::ParamType(
        ty,
        param_type.1.clone(),
        param_type.2.clone(),
    ))
}

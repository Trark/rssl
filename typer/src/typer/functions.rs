use super::errors::*;
use super::scopes::*;
use super::statements::parse_statement_list;
use super::types::parse_type;
use rssl_ast as ast;
use rssl_ir as ir;

#[derive(PartialEq, Debug, Clone)]
pub enum Callable {
    Function(ir::FunctionId),
    Intrinsic(crate::intrinsics::IntrinsicFactory),
}

#[derive(PartialEq, Debug, Clone)]
pub struct FunctionOverload(pub Callable, pub FunctionSignature);

/// Parse a function in a root context
pub fn parse_rootdefinition_function(
    fd: &ast::FunctionDefinition,
    context: &mut Context,
) -> TyperResult<ir::RootDefinition> {
    let ir_fd = parse_function(fd, context)?;
    Ok(ir::RootDefinition::Function(ir_fd))
}

/// Describes the signature for a function
#[derive(PartialEq, Debug, Clone)]
pub struct FunctionSignature {
    // TODO: Function and parameter names
    pub return_type: ir::FunctionReturn,
    pub param_types: Vec<ir::ParamType>,
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

    if let Some(ref template_params) = fd.template_params {
        for template_param in &template_params.0 {
            context.insert_template_type(template_param.clone())?;
        }
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
    scope: ScopeIndex,
    context: &mut Context,
) -> TyperResult<ir::FunctionDefinition> {
    context.revisit_scope(scope);

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
    let id = context.register_function(fd.name.clone(), signature.clone())?;
    context.add_function_to_current_scope(id)?;

    parse_function_body(fd, id, signature, scope, context)
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

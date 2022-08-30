use super::errors::*;
use super::scopes::*;
use super::statements::parse_statement_list;
use super::types::parse_type;
use rssl_ast as ast;
use rssl_ir as ir;

#[derive(PartialEq, Debug, Clone)]
pub enum FunctionName {
    Intrinsic(crate::intrinsics::IntrinsicFactory),
    User(ir::FunctionId),
}

#[derive(PartialEq, Debug, Clone)]
pub struct FunctionOverload(pub FunctionName, pub ir::Type, pub Vec<ir::ParamType>);

pub fn parse_rootdefinition_function(
    fd: &ast::FunctionDefinition,
    context: &mut Context,
) -> TyperResult<ir::RootDefinition> {
    context.push_scope();
    if let Some(ref template_params) = fd.template_params {
        for template_param in &template_params.0 {
            context.insert_template_type(template_param.clone())?;
        }
    }

    let return_type = parse_returntype(&fd.returntype, context)?;
    // We save the return type of the current function for return statement parsing
    context.set_function_return_type(return_type.return_type.clone());

    let func_params = {
        let mut vec = vec![];
        for param in &fd.params {
            let var_type = parse_paramtype(&param.param_type, context)?;
            let var_id = context.insert_variable(param.name.clone(), var_type.0.clone())?;
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

    // Register the function signature
    // TODO: Recursive calls? Needs to move register earlier
    let id = {
        let return_type = return_type.return_type.clone();
        let param_types = func_params.iter().map(|p| p.param_type.clone()).collect();
        context.insert_function(fd.name.clone(), return_type, param_types)?
    };

    let fd_ir = ir::FunctionDefinition {
        id,
        returntype: return_type,
        params: func_params,
        scope_block: ir::ScopeBlock(body_ir, decls),
        attributes: fd.attributes.clone(),
    };
    Ok(ir::RootDefinition::Function(fd_ir))
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

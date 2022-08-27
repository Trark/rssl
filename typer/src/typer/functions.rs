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
    mut context: GlobalContext,
) -> TyperResult<(ir::RootDefinition, GlobalContext)> {
    let return_type = parse_returntype(&fd.returntype, &context)?;
    // Set the return type of the current function (for return statement parsing)
    assert_eq!(context.current_return_type, None);
    context.current_return_type = Some(return_type.return_type.clone());

    let mut scoped_context = ScopeContext::from_global(&context);
    let func_params = {
        let mut vec = vec![];
        for param in &fd.params {
            let var_type = parse_paramtype(&param.param_type, &context)?;
            let var_id = scoped_context.insert_variable(param.name.clone(), var_type.0.clone())?;
            vec.push(ir::FunctionParam {
                id: var_id,
                param_type: var_type,
            });
        }
        vec
    };
    let (body_ir, scoped_context) = parse_statement_list(&fd.body, scoped_context)?;
    let decls = scoped_context.destruct();

    // Unset the return type for the current function
    assert!(context.current_return_type != None);
    context.current_return_type = None;

    let fd_ir = ir::FunctionDefinition {
        id: context.make_function_id(),
        returntype: return_type,
        params: func_params,
        scope_block: ir::ScopeBlock(body_ir, decls),
        attributes: fd.attributes.clone(),
    };
    let func_type = FunctionOverload(
        FunctionName::User(fd_ir.id),
        fd_ir.returntype.return_type.clone(),
        fd_ir.params.iter().map(|p| p.param_type.clone()).collect(),
    );
    context.insert_function(fd.name.clone(), func_type)?;
    Ok((ir::RootDefinition::Function(fd_ir), context))
}

fn parse_returntype(
    return_type: &ast::FunctionReturn,
    struct_finder: &dyn StructIdFinder,
) -> TyperResult<ir::FunctionReturn> {
    let ty = parse_type(&return_type.return_type, struct_finder)?;
    Ok(ir::FunctionReturn { return_type: ty })
}

fn parse_paramtype(
    param_type: &ast::ParamType,
    struct_finder: &dyn StructIdFinder,
) -> TyperResult<ir::ParamType> {
    let ty = parse_type(&param_type.0, struct_finder)?;
    Ok(ir::ParamType(
        ty,
        param_type.1.clone(),
        param_type.2.clone(),
    ))
}

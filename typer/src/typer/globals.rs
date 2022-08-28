use std::collections::HashMap;

use super::errors::*;
use super::scopes::*;
use super::statements::{apply_variable_bind, parse_initializer_opt};
use super::types::parse_type;
use rssl_ast as ast;
use rssl_ir as ir;

pub fn parse_rootdefinition_globalvariable(
    gv: &ast::GlobalVariable,
    context: &mut Context,
) -> TyperResult<Vec<ir::RootDefinition>> {
    let var_type = parse_globaltype(&gv.global_type, context)?;

    let mut defs = vec![];

    for def in &gv.defs {
        // Resolve type
        let ir::GlobalType(lty, gs, interp) = var_type.clone();
        let bind = &def.bind;
        let gv_tyl = apply_variable_bind(lty, bind, &def.init)?;
        let gv_type = ir::GlobalType(gv_tyl, gs, interp);

        // Insert variable
        let var_name = def.name.clone();
        let input_type = gv_type.0.clone();
        let var_id = context.insert_global(var_name.clone(), input_type.clone())?;

        let var_init = parse_initializer_opt(&def.init, &input_type.0, context)?;
        let gv_ir = ir::GlobalVariable {
            id: var_id,
            global_type: gv_type,
            init: var_init,
        };

        defs.push(ir::RootDefinition::GlobalVariable(gv_ir));
    }

    Ok(defs)
}

fn parse_globaltype(
    global_type: &ast::GlobalType,
    context: &Context,
) -> TyperResult<ir::GlobalType> {
    let ty = parse_type(&global_type.0, context)?;
    Ok(ir::GlobalType(
        ty,
        global_type.1.clone(),
        global_type.2.clone(),
    ))
}

pub fn parse_rootdefinition_constantbuffer(
    cb: &ast::ConstantBuffer,
    context: &mut Context,
) -> TyperResult<ir::RootDefinition> {
    let cb_name = cb.name.clone();
    let mut members = vec![];
    let mut members_map = HashMap::new();
    for member in &cb.members {
        let base_type = parse_type(&member.ty, context)?;
        for def in &member.defs {
            let var_name = def.name.clone();
            let var_offset = def.offset.clone();
            let var_type = apply_variable_bind(base_type.clone(), &def.bind, &None)?;
            members_map.insert(var_name.clone(), var_type.clone());
            members.push(ir::ConstantVariable {
                name: var_name,
                typename: var_type,
                offset: var_offset,
            });
        }
    }
    let id = match context.insert_cbuffer(&cb_name, members_map) {
        Some(id) => id,
        None => return Err(TyperError::ConstantBufferAlreadyDefined(cb_name.clone())),
    };
    let cb_ir = ir::ConstantBuffer { id, members };
    Ok(ir::RootDefinition::ConstantBuffer(cb_ir))
}

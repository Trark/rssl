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
        let ir::GlobalType(lty, gs) = var_type.clone();
        let bind = &def.bind;
        let gv_tyl = apply_variable_bind(lty, bind, &def.init)?;
        let gv_type = ir::GlobalType(gv_tyl, gs);

        // Insert variable
        let var_name = def.name.clone();
        let input_type = gv_type.0.clone();
        let var_id = context.insert_global(var_name.clone(), gv_type)?;

        let var_init = parse_initializer_opt(&def.init, &input_type, context)?;

        let gv_ir = &mut context.module.global_registry[var_id.0 as usize];
        gv_ir.lang_slot = def.slot.clone().map(|r| ir::LanguageBinding {
            set: 0,
            index: r.index,
        });
        gv_ir.init = var_init;

        defs.push(ir::RootDefinition::GlobalVariable(var_id));
    }

    Ok(defs)
}

fn parse_globaltype(
    global_type: &ast::GlobalType,
    context: &mut Context,
) -> TyperResult<ir::GlobalType> {
    let ty = parse_type(&global_type.0, context)?;
    Ok(ir::GlobalType(ty, global_type.1.clone()))
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
    let id = match context.insert_cbuffer(cb_name.clone(), members_map) {
        Ok(id) => id,
        Err(id) => return Err(TyperError::ConstantBufferAlreadyDefined(cb_name, id)),
    };
    let cb_ir = &mut context.module.cbuffer_registry[id.0 as usize];
    cb_ir.lang_binding = cb
        .slot
        .clone()
        .map(|c| ir::LanguageBinding { set: 0, index: c.0 });
    cb_ir.members = members;

    Ok(ir::RootDefinition::ConstantBuffer(id))
}

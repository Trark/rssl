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
    let (base_id, storage_class) = parse_globaltype(&gv.global_type, context)?;

    let base_type_layout = context
        .module
        .type_registry
        .get_type_layout(base_id)
        .clone();

    let mut defs = vec![];
    for global_variable in &gv.defs {
        // Resolve type bind
        let type_layout = apply_variable_bind(
            base_type_layout.clone(),
            &global_variable.bind,
            &global_variable.init,
        )?;

        // Parse the initializer
        let var_init = parse_initializer_opt(&global_variable.init, &type_layout, context)?;

        // Register the type
        let type_id = context.module.type_registry.register_type(type_layout);

        // Insert variable
        let var_id = context.insert_global(global_variable.name.clone(), type_id, storage_class)?;

        let gv_ir = &mut context.module.global_registry[var_id.0 as usize];
        gv_ir.lang_slot = global_variable.slot.clone().map(|r| ir::LanguageBinding {
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
) -> TyperResult<(ir::TypeId, ir::GlobalStorage)> {
    let mut ty = parse_type(&global_type.0, context)?;
    if ty.is_void() {
        return Err(TyperError::VariableHasIncompleteType(
            ty,
            global_type.0.location,
        ));
    }

    // All extern variables are implicitly const
    if global_type.1 == ir::GlobalStorage::Extern {
        ty = ty.make_const();
    }

    let ty = context.module.type_registry.register_type(ty);

    Ok((ty, global_type.1))
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
        if base_type.is_void() {
            return Err(TyperError::VariableHasIncompleteType(
                base_type,
                member.ty.location,
            ));
        }
        for def in &member.defs {
            let var_name = def.name.clone();
            let var_offset = def.offset.clone();
            let var_type = apply_variable_bind(base_type.clone(), &def.bind, &None)?;
            let type_id = context.module.type_registry.register_type(var_type);
            members_map.insert(var_name.clone(), type_id);
            members.push(ir::ConstantVariable {
                name: var_name,
                type_id,
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

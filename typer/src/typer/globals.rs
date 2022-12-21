use std::collections::HashMap;

use super::errors::*;
use super::scopes::*;
use super::statements::{apply_variable_bind, evaluate_constexpr_int, parse_initializer_opt};
use super::types::parse_type;
use rssl_ast as ast;
use rssl_ir as ir;
use rssl_text::*;

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
            context,
        )?;

        // Register the type
        let type_id = context.module.type_registry.register_type(type_layout);

        // Parse the initializer
        let var_init = parse_initializer_opt(
            &global_variable.init,
            type_id,
            global_variable.name.get_location(),
            context,
        )?;

        // Attempt to resolve the initializer as a constant expression
        let evaluated_value = (|| {
            if let Some(ir::Initializer::Expression(expr)) = &var_init {
                let (type_unmodified, type_mod) =
                    context.module.type_registry.extract_modifier(type_id);
                if type_mod.is_const {
                    if let ir::TypeLayer::Scalar(ir::ScalarType::UInt) =
                        context.module.type_registry.get_type_layer(type_unmodified)
                    {
                        if let Ok(value) = evaluate_constexpr_int(expr, context) {
                            return Some(value);
                        }
                    }
                }
            }
            None
        })();

        // Insert variable
        let var_id = context.insert_global(global_variable.name.clone(), type_id, storage_class)?;

        let gv_ir = &mut context.module.global_registry[var_id.0 as usize];
        gv_ir.lang_slot = match &global_variable.slot {
            Some(slot) => {
                if let ir::TypeLayout::Object(ot) = base_type_layout.clone().remove_modifier() {
                    let expected_slot_type = ot.get_register_type();
                    if slot.slot_type != expected_slot_type {
                        return Err(TyperError::InvalidRegisterType(
                            slot.slot_type,
                            expected_slot_type,
                            global_variable.name.location,
                        ));
                    };
                    Some(ir::LanguageBinding {
                        set: 0,
                        index: slot.index,
                    })
                } else {
                    return Err(TyperError::InvalidRegisterAnnotation(
                        type_id,
                        global_variable.name.location,
                    ));
                }
            }
            None => None,
        };
        gv_ir.init = var_init;

        gv_ir.constexpr_value = evaluated_value;

        defs.push(ir::RootDefinition::GlobalVariable(var_id));
    }

    Ok(defs)
}

fn parse_globaltype(
    global_type: &ast::GlobalType,
    context: &mut Context,
) -> TyperResult<(ir::TypeId, ir::GlobalStorage)> {
    let mut ty = parse_type(&global_type.0, context)?;

    let ty_unmodified = context.module.type_registry.remove_modifier(ty);
    let tyl = context.module.type_registry.get_type_layout(ty_unmodified);
    if tyl.is_void() {
        return Err(TyperError::VariableHasIncompleteType(
            ty,
            global_type.0.location,
        ));
    }

    // All extern variables are implicitly const
    if global_type.1 == ir::GlobalStorage::Extern {
        ty = context.module.type_registry.make_const(ty);
    }

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
        let base_type_layout = context
            .module
            .type_registry
            .get_type_layout(base_type)
            .clone();

        let base_type_unmodified = context.module.type_registry.remove_modifier(base_type);
        let base_type_layout_unmodified = context
            .module
            .type_registry
            .get_type_layout(base_type_unmodified);
        if base_type_layout_unmodified.is_void() {
            return Err(TyperError::VariableHasIncompleteType(
                base_type,
                member.ty.location,
            ));
        }

        for def in &member.defs {
            let var_name = def.name.clone();
            let var_offset = def.offset.clone();
            let var_type =
                apply_variable_bind(base_type_layout.clone(), &def.bind, &None, context)?;
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
    cb_ir.lang_binding = match &cb.slot {
        Some(slot) => {
            if slot.slot_type != ir::RegisterType::B {
                return Err(TyperError::InvalidRegisterType(
                    slot.slot_type,
                    ir::RegisterType::B,
                    cb.name.location,
                ));
            };
            Some(ir::LanguageBinding {
                set: 0,
                index: slot.index,
            })
        }
        None => None,
    };
    cb_ir.members = members;

    Ok(ir::RootDefinition::ConstantBuffer(id))
}

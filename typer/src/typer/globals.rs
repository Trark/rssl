use super::errors::*;
use super::expressions::parse_expr;
use super::scopes::*;
use super::statements::{apply_variable_bind, parse_initializer_opt};
use super::types::{is_illegal_variable_name, parse_type_for_usage, TypePosition};
use crate::evaluator::evaluate_constexpr;
use rssl_ast as ast;
use rssl_ir as ir;
use rssl_text::*;

pub fn parse_rootdefinition_globalvariable(
    gv: &ast::GlobalVariable,
    context: &mut Context,
) -> TyperResult<Vec<ir::RootDefinition>> {
    let (base_id, storage_class) = parse_globaltype(&gv.global_type, context)?;

    let attribute_result = parse_attributes_for_global(&gv.attributes, context)?;

    let mut defs = vec![];
    for global_variable in &gv.defs {
        // Deny restricted non-keyword names
        if is_illegal_variable_name(&global_variable.name) {
            return Err(TyperError::IllegalVariableName(
                global_variable.name.get_location(),
            ));
        }

        // Resolve type bind
        let type_id = apply_variable_bind(
            base_id,
            global_variable.name.location,
            &global_variable.bind,
            &global_variable.init,
            true,
            context,
        )?;

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
                let (_, type_mod) = context.module.type_registry.extract_modifier(type_id);
                if type_mod.is_const {
                    if let Ok(value) = evaluate_constexpr(expr, &mut context.module) {
                        return Some(value);
                    }
                }
            }
            None
        })();

        // Insert variable
        let var_id = context.insert_global(global_variable.name.clone(), type_id, storage_class)?;

        let gv_ir = &mut context.module.global_registry[var_id.0 as usize];
        gv_ir.lang_slot = match &global_variable.slot {
            Some(register) => {
                let unmodified_base_id = context.module.type_registry.remove_modifier(base_id);
                let unmodified_base_tyl = context
                    .module
                    .type_registry
                    .get_type_layer(unmodified_base_id);
                if let ir::TypeLayer::Object(ot) = unmodified_base_tyl {
                    let expected_slot_type = ot.get_register_type();

                    let index = if let Some(slot) = &register.slot {
                        if slot.slot_type != expected_slot_type {
                            return Err(TyperError::InvalidRegisterType(
                                slot.slot_type,
                                expected_slot_type,
                                global_variable.name.location,
                            ));
                        }
                        Some(slot.index)
                    } else {
                        None
                    };

                    ir::LanguageBinding {
                        set: register.space,
                        index,
                    }
                } else {
                    return Err(TyperError::InvalidRegisterAnnotation(
                        type_id,
                        global_variable.name.location,
                    ));
                }
            }
            None => ir::LanguageBinding::default(),
        };

        // Override binding index with value from attribute
        if let Some(binding_index) = attribute_result.binding_index_override {
            gv_ir.lang_slot.index = Some(binding_index);
        }

        // Override binding group with value from attribute
        if let Some(binding_group) = attribute_result.binding_group_override {
            gv_ir.lang_slot.set = Some(binding_group);
        }

        gv_ir.init = var_init;

        gv_ir.constexpr_value = evaluated_value;

        defs.push(ir::RootDefinition::GlobalVariable(var_id));
    }

    Ok(defs)
}

/// Parse a type used for a global variable
fn parse_globaltype(
    global_type: &ast::Type,
    context: &mut Context,
) -> TyperResult<(ir::TypeId, ir::GlobalStorage)> {
    let mut ty = parse_type_for_usage(global_type, TypePosition::Global, context)?;

    // Calculate the global storage type
    let mut global_storage = None;
    for modifier in &global_type.modifiers.modifiers {
        let next_gs = match &modifier.node {
            ast::TypeModifier::Extern => ir::GlobalStorage::Extern,
            ast::TypeModifier::Static => ir::GlobalStorage::Static,
            ast::TypeModifier::GroupShared => ir::GlobalStorage::GroupShared,
            _ => continue,
        };

        if let Some((current_gs, current_source)) = global_storage {
            if current_gs == next_gs {
                // TODO: Warn for duplicate modifier
            } else {
                return Err(TyperError::ModifierConflict(
                    modifier.node,
                    modifier.location,
                    current_source,
                ));
            }
        } else {
            global_storage = Some((next_gs, modifier.node));
        }
    }
    let global_storage = global_storage
        .map(|(gs, _)| gs)
        .unwrap_or(ir::GlobalStorage::Extern);

    if context.module.type_registry.is_void(ty) {
        return Err(TyperError::VariableHasIncompleteType(
            ty,
            global_type.location,
        ));
    }

    // All extern variables are implicitly const
    if global_storage == ir::GlobalStorage::Extern {
        ty = context.module.type_registry.make_const(ty);
    }

    Ok((ty, global_storage))
}

pub fn parse_rootdefinition_constantbuffer(
    cb: &ast::ConstantBuffer,
    context: &mut Context,
) -> TyperResult<ir::RootDefinition> {
    let cb_name = cb.name.clone();
    let mut members = Vec::new();
    for member in &cb.members {
        let base_type =
            parse_type_for_usage(&member.ty, TypePosition::ConstantBufferMember, context)?;

        if context.module.type_registry.is_void(base_type) {
            return Err(TyperError::VariableHasIncompleteType(
                base_type,
                member.ty.location,
            ));
        }

        for def in &member.defs {
            // Deny restricted non-keyword names
            if is_illegal_variable_name(&def.name) {
                return Err(TyperError::IllegalVariableName(def.name.get_location()));
            }

            let var_name = def.name.clone();
            let var_offset = def.offset.clone();
            let type_id = apply_variable_bind(
                base_type,
                def.name.location,
                &def.bind,
                &None,
                false,
                context,
            )?;
            members.push(ir::ConstantVariable {
                name: var_name,
                type_id,
                offset: var_offset,
            });
        }
    }

    // Register the constant buffer with the module
    let id = ir::ConstantBufferId(context.module.cbuffer_registry.len() as u32);
    context.module.cbuffer_registry.push(ir::ConstantBuffer {
        name: cb_name,
        namespace: context.get_current_namespace(),
        lang_binding: ir::LanguageBinding::default(),
        api_binding: None,
        members: Vec::new(),
    });

    let cb_ir = &mut context.module.cbuffer_registry[id.0 as usize];
    cb_ir.lang_binding = match &cb.slot {
        Some(register) => {
            let index = if let Some(slot) = &register.slot {
                if slot.slot_type != ir::RegisterType::B {
                    return Err(TyperError::InvalidRegisterType(
                        slot.slot_type,
                        ir::RegisterType::B,
                        cb.name.location,
                    ));
                };
                Some(slot.index)
            } else {
                None
            };

            ir::LanguageBinding {
                set: register.space,
                index,
            }
        }
        None => ir::LanguageBinding::default(),
    };
    cb_ir.members = members;

    // Insert it into the scopes
    context.insert_cbuffer(id)?;

    Ok(ir::RootDefinition::ConstantBuffer(id))
}

/// Set of data accumulated from processing all attributes
struct GlobalAttributeResult {
    binding_index_override: Option<u32>,
    binding_group_override: Option<u32>,
}

/// Process all attributes for a global variable
fn parse_attributes_for_global(
    attributes: &[ast::Attribute],
    context: &mut Context,
) -> TyperResult<GlobalAttributeResult> {
    let mut result = GlobalAttributeResult {
        binding_index_override: None,
        binding_group_override: None,
    };

    for attribute in attributes {
        match attribute.name.as_slice() {
            [namespace, leaf] => {
                match namespace.node.as_str() {
                    "rssl" => {
                        // RSSL specific attributes
                        match leaf.as_str() {
                            "bind_group" => {
                                if attribute.arguments.len() == 1 {
                                    let group_index =
                                        parse_expr_as_u32(&attribute.arguments[0], context)?;
                                    result.binding_group_override = Some(group_index);
                                } else {
                                    return Err(
                                        TyperError::GlobalAttributeUnexpectedArgumentCount(
                                            leaf.node.clone(),
                                            leaf.location,
                                        ),
                                    );
                                }
                            }
                            _ => {
                                return Err(TyperError::GlobalAttributeUnknown(
                                    leaf.node.clone(),
                                    leaf.location,
                                ))
                            }
                        }
                    }
                    "vk" => {
                        // Attributes from HLSL SPIR-V mappings
                        match leaf.as_str() {
                            "binding" => {
                                if attribute.arguments.len() == 1 {
                                    let binding_index =
                                        parse_expr_as_u32(&attribute.arguments[0], context)?;
                                    result.binding_index_override = Some(binding_index);
                                } else if attribute.arguments.len() == 2 {
                                    let binding_index =
                                        parse_expr_as_u32(&attribute.arguments[0], context)?;
                                    let group_index =
                                        parse_expr_as_u32(&attribute.arguments[1], context)?;
                                    result.binding_index_override = Some(binding_index);
                                    result.binding_group_override = Some(group_index);
                                } else {
                                    return Err(
                                        TyperError::GlobalAttributeUnexpectedArgumentCount(
                                            leaf.node.clone(),
                                            leaf.location,
                                        ),
                                    );
                                }
                            }
                            _ => {
                                return Err(TyperError::GlobalAttributeUnknown(
                                    leaf.node.clone(),
                                    leaf.location,
                                ))
                            }
                        }
                    }
                    _ => {
                        return Err(TyperError::GlobalAttributeUnknown(
                            namespace.node.clone(),
                            namespace.location,
                        ))
                    }
                }
            }
            [first, ..] => {
                return Err(TyperError::GlobalAttributeUnknown(
                    first.node.clone(),
                    first.location,
                ))
            }
            _ => panic!("Attribute with no name"),
        }
    }

    Ok(result)
}

/// Type check and constant evaluate an ast expression to get a u32
fn parse_expr_as_u32(expr: &Located<ast::Expression>, context: &mut Context) -> TyperResult<u32> {
    let expr_ir = parse_expr(expr, context)?.0;
    let evaluated = match evaluate_constexpr(&expr_ir, &mut context.module) {
        Ok(value) => value,
        Err(_) => return Err(TyperError::ExpressionIsNotConstantExpression(expr.location)),
    };
    let value = match evaluated.to_uint64() {
        Some(v) if v <= u32::MAX as u64 => v as u32,
        _ => return Err(TyperError::ExpressionIsNotConstantExpression(expr.location)),
    };
    Ok(value)
}

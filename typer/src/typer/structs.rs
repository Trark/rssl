use super::errors::*;
use super::functions::{parse_function_body, parse_function_signature};
use super::scopes::*;
use super::statements::apply_variable_bind;
use super::types::{
    is_illegal_type_name, is_illegal_variable_name, parse_interpolation_modifier, parse_precise,
    parse_type_for_usage, TypePosition,
};
use rssl_ast as ast;
use rssl_ir as ir;
use rssl_text::*;
use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::collections::HashSet;

/// Process a struct definition
pub fn parse_rootdefinition_struct(
    sd: &ast::StructDefinition,
    context: &mut Context,
) -> TyperResult<ir::RootDefinition> {
    // Deny restricted non-keyword names
    if is_illegal_type_name(&sd.name) {
        return Err(TyperError::IllegalStructName(sd.name.location));
    }

    if !sd.template_params.0.is_empty() {
        // Register the struct template
        let name = &sd.name;
        let id = match context.register_struct_template(name.clone(), sd.clone()) {
            Ok(id) => id,
            Err(id) => return Err(TyperError::TypeAlreadyDefined(name.clone(), id)),
        };

        Ok(ir::RootDefinition::StructTemplate(id))
    } else {
        let struct_def = parse_struct_internal(sd, &[], context)?;
        Ok(ir::RootDefinition::Struct(struct_def))
    }
}

/// Build a struct from a struct template
pub fn build_struct_from_template(
    sd: &ast::StructDefinition,
    template_args: &[ir::TypeOrConstant],
    context: &mut Context,
) -> TyperResult<ir::StructId> {
    let struct_def = parse_struct_internal(sd, template_args, context)?;
    Ok(struct_def)
}

/// Process a struct internals
fn parse_struct_internal(
    sd: &ast::StructDefinition,
    template_args: &[ir::TypeOrConstant],
    context: &mut Context,
) -> TyperResult<ir::StructId> {
    // Register the struct
    let name = &sd.name;
    let id = match context.begin_struct(name.clone(), template_args.is_empty()) {
        Ok(id) => id,
        Err(id) => return Err(TyperError::TypeAlreadyDefined(name.clone(), id)),
    };

    let mut base_structs = Vec::new();
    for parent in &sd.base_types {
        // TODO: Evaluate which keywords are allowed here
        let parent_id = parse_type_for_usage(parent, TypePosition::Free, context)?;
        if let ir::TypeLayer::Struct(parent_sid) =
            context.module.type_registry.get_type_layer(parent_id)
        {
            base_structs.push(parent_sid);
        } else {
            return Err(TyperError::IllegalStructBaseType(parent.location));
        }
    }

    context.push_scope_with_name(name);
    if !template_args.is_empty() {
        // Inconsistent number of args not gracefully handled
        assert_eq!(template_args.len(), sd.template_params.0.len());

        // Register template arguments
        for (template_param, template_arg) in sd.template_params.0.iter().zip(template_args) {
            match (template_param, template_arg) {
                (ast::TemplateParam::Type(ty_param), ir::TypeOrConstant::Type(ty)) => {
                    if let Some(name) = &ty_param.name {
                        context.register_typedef(name.clone(), *ty)?
                    }
                }
                (ast::TemplateParam::Value(_), ir::TypeOrConstant::Constant(_)) => {
                    todo!("Non-type template arguments")
                }
                _ => todo!("Inconsistent type/non-type parameter error handling"),
            }
        }
    }

    let mut members = Vec::new();
    let mut methods_to_parse = Vec::new();
    let mut member_map = HashSet::new();
    let mut method_map = HashMap::<_, Vec<_>>::new();

    // Add members from base types
    for base_struct in base_structs {
        let base_def = &context.module.struct_registry[base_struct.0 as usize];

        if !base_def.members.is_empty() {
            for member in &base_def.members {
                // We do not currently allow derived types to reuse names from parent type
                member_map.insert(member.name.clone());
                // Copy member from parent to child
                // This means the member is owned by the child and not the parent
                members.push(member.clone());
            }
        }

        if !base_def.methods.is_empty() {
            todo!("Inherited methods are not implemented");
        }
    }

    for ast_entry in &sd.members {
        match ast_entry {
            ast::StructEntry::Variable(ast_member) => {
                let base_type =
                    parse_type_for_usage(&ast_member.ty, TypePosition::StructMember, context)?;

                // Forbid storage classes except static
                // We do not handle static in any special way either currently
                // Forbid const when used directly on the member (but it can still appear on the type)
                for modifier in &ast_member.ty.modifiers.modifiers {
                    if matches!(
                        &modifier.node,
                        ast::TypeModifier::Const
                            | ast::TypeModifier::Extern
                            | ast::TypeModifier::GroupShared
                    ) {
                        return Err(TyperError::ModifierNotSupported(
                            modifier.node,
                            modifier.location,
                            TypePosition::StructMember,
                        ));
                    }
                }

                if context.module.type_registry.is_void(base_type) {
                    return Err(TyperError::VariableHasIncompleteType(
                        base_type,
                        ast_member.ty.location,
                    ));
                }

                let interpolation_modifier =
                    parse_interpolation_modifier(&ast_member.ty.modifiers)?;
                let interpolation_modifier = interpolation_modifier.map(|(im, _)| im);

                // Calculate if we are precise
                let precise_result = parse_precise(&ast_member.ty.modifiers)?;

                for def in &ast_member.defs {
                    // Deny restricted non-keyword names
                    if is_illegal_variable_name(&def.name) {
                        return Err(TyperError::IllegalVariableName(def.name.get_location()));
                    }

                    let name = def.name.node.clone();
                    let type_id = apply_variable_bind(
                        base_type,
                        def.name.location,
                        &def.bind,
                        &None,
                        false,
                        context,
                    )?;

                    if member_map.contains(&name) || method_map.contains_key(&name) {
                        return Err(TyperError::ValueAlreadyDefined(
                            def.name.clone(),
                            ErrorType::Unknown,
                            type_id.to_error_type(),
                        ));
                    }

                    member_map.insert(name.clone());
                    members.push(ir::StructMember {
                        name,
                        type_id,
                        semantic: def.semantic.clone(),
                        interpolation_modifier,
                        precise: precise_result.is_some(),
                    });
                }
            }
            ast::StructEntry::Method(ast_func) => {
                // Process the method signature
                let (signature, scope) = parse_function_signature(ast_func, Some(id), context)?;

                // Register the method signature in the function list
                // Functions can not be pre-declared multiple times like with free functions
                let id = context.register_function(
                    ast_func.name.clone(),
                    signature.clone(),
                    scope,
                    ast_func.clone(),
                )?;

                // Check for members with the same name
                if member_map.contains(&ast_func.name.node) {
                    return Err(TyperError::ValueAlreadyDefined(
                        ast_func.name.clone(),
                        ErrorType::Unknown,
                        ErrorType::Unknown,
                    ));
                }

                // Add the method to the struct function set
                match method_map.entry(ast_func.name.node.clone()) {
                    Entry::Occupied(mut o) => {
                        // Detect duplicate signatures
                        for existing_id in o.get() {
                            let existing_signature = context
                                .module
                                .function_registry
                                .get_function_signature(*existing_id);
                            if existing_signature.param_types == signature.param_types {
                                return Err(TyperError::ValueAlreadyDefined(
                                    ast_func.name.clone(),
                                    ErrorType::Unknown,
                                    ErrorType::Unknown,
                                ));
                            }
                        }

                        o.get_mut().push(id);
                    }
                    Entry::Vacant(v) => {
                        v.insert(Vec::from([id]));
                    }
                }

                // Add body on list to process later after other members are registered
                methods_to_parse.push((ast_func, id, signature));
            }
        }
    }

    // Store all the symbol names
    let def = &mut context.module.struct_registry[id.0 as usize];
    def.members = members;
    def.methods = methods_to_parse.iter().map(|(_, id, _)| *id).collect();

    // Process all the methods
    let mut methods = Vec::new();
    for (ast_func, id, signature) in methods_to_parse {
        if signature.template_params.is_empty() {
            if ast_func.body.is_some() {
                parse_function_body(ast_func, id, signature, context)?;
            }
            methods.push(id);
        } else {
            // Do not add the templated method to the tree for now
            // TODO: Template methods in the final output
        }
    }

    context.pop_scope();

    Ok(id)
}

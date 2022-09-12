use super::errors::*;
use super::functions::{parse_function_body, parse_function_signature};
use super::scopes::*;
use super::statements::apply_variable_bind;
use super::types::parse_type;
use rssl_ast as ast;
use rssl_ir as ir;
use std::collections::hash_map::Entry;
use std::collections::HashMap;

/// Process a struct definition
pub fn parse_rootdefinition_struct(
    sd: &ast::StructDefinition,
    context: &mut Context,
) -> TyperResult<ir::RootDefinition> {
    if !sd.template_params.0.is_empty() {
        // Register the struct template
        let name = &sd.name;
        let id = match context.register_struct_template(name.clone(), sd.clone()) {
            Ok(id) => id,
            Err(id) => return Err(TyperError::StructAlreadyDefined(name.clone(), id)),
        };

        Ok(ir::RootDefinition::StructTemplate(
            ir::StructTemplateDefinition {
                id,
                ast: sd.clone(),
            },
        ))
    } else {
        let struct_def = parse_struct_internal(sd, &[], context)?;
        Ok(ir::RootDefinition::Struct(struct_def))
    }
}

/// Build a struct from a struct template
pub fn build_struct_from_template(
    sd: &ast::StructDefinition,
    template_args: &[ir::Type],
    context: &mut Context,
) -> TyperResult<ir::StructId> {
    let struct_def = parse_struct_internal(sd, template_args, context)?;
    Ok(struct_def.id)
}

/// Process a struct internals
fn parse_struct_internal(
    sd: &ast::StructDefinition,
    template_args: &[ir::Type],
    context: &mut Context,
) -> TyperResult<ir::StructDefinition> {
    // Register the struct
    let name = &sd.name;
    let id = match context.begin_struct(name.clone(), template_args.is_empty()) {
        Ok(id) => id,
        Err(id) => return Err(TyperError::StructAlreadyDefined(name.clone(), id)),
    };

    context.push_scope();
    if !template_args.is_empty() {
        // Inconsistent number of args not gracefully handled
        assert_eq!(template_args.len(), sd.template_params.0.len());

        // Register template arguments
        for (template_param, template_arg) in sd.template_params.0.iter().zip(template_args) {
            context.register_typedef(template_param.clone(), template_arg.clone())?;
        }
    }

    let mut members = Vec::new();
    let mut methods_to_parse = Vec::new();
    let mut member_map = HashMap::new();
    let mut method_map = HashMap::<_, Vec<_>>::new();
    // TODO: Detect name reuse between member variables and methods
    for ast_entry in &sd.members {
        match ast_entry {
            ast::StructEntry::Variable(ast_member) => {
                let base_type = parse_type(&ast_member.ty, context)?;
                for def in &ast_member.defs {
                    let name = def.name.clone();
                    let typename = apply_variable_bind(base_type.clone(), &def.bind, &None)?;
                    member_map.insert(name.clone(), typename.clone());
                    members.push(ir::StructMember { name, typename });
                }
            }
            ast::StructEntry::Method(ast_func) => {
                // Process the method signature
                let (signature, scope) = parse_function_signature(ast_func, Some(id), context)?;

                // Register the method signature in the function list
                let id = context.register_function(
                    ast_func.name.clone(),
                    signature.clone(),
                    scope,
                    ast_func.clone(),
                )?;

                // Add the method to the struct function set
                match method_map.entry(ast_func.name.node.clone()) {
                    Entry::Occupied(mut o) => {
                        // TODO: Detect duplicate signatures
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

    // Store all the symbol names on the registered struct
    context.finish_struct(id, member_map, method_map);

    // Process all the methods
    let mut methods = Vec::new();
    for (ast_func, id, signature) in methods_to_parse {
        if signature.template_params.0 == 0 {
            let ir_func = parse_function_body(ast_func, id, signature, context)?;
            methods.push(ir_func);
        } else {
            // Do not add the templated method to the tree for now
            // TODO: Template methods in the final output
        }
    }

    context.pop_scope();

    Ok(ir::StructDefinition {
        id,
        members,
        methods,
    })
}

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
    // Register the struct
    let name = &sd.name;
    let id = match context.begin_struct(name.clone()) {
        Ok(id) => id,
        Err(id) => return Err(TyperError::StructAlreadyDefined(name.clone(), id)),
    };

    // Template structs are not currently supported
    if !sd.template_params.0.is_empty() {
        todo!();
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
                let id = context.register_function(ast_func.name.clone(), signature.clone())?;

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
                methods_to_parse.push((ast_func, id, signature, scope));
            }
        }
    }

    // Store all the symbol names on the registered struct
    context.finish_struct(id, member_map, method_map);

    // Process all the methods
    let mut methods = Vec::new();
    for (ast_func, id, signature, scope) in methods_to_parse {
        let ir_func = parse_function_body(ast_func, id, signature, scope, context)?;
        methods.push(ir_func);
    }

    let struct_def = ir::StructDefinition {
        id,
        members,
        methods,
    };
    Ok(ir::RootDefinition::Struct(struct_def))
}

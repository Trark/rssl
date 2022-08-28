use super::errors::*;
use super::scopes::*;
use super::statements::apply_variable_bind;
use super::types::parse_type;
use rssl_ast as ast;
use rssl_ir as ir;
use std::collections::HashMap;

/// Process a struct definition
pub fn parse_rootdefinition_struct(
    sd: &ast::StructDefinition,
    context: &mut Context,
) -> TyperResult<ir::RootDefinition> {
    let mut members = vec![];
    let mut member_map = HashMap::new();
    for ast_member in &sd.members {
        let base_type = parse_type(&ast_member.ty, context)?;
        for def in &ast_member.defs {
            let name = def.name.clone();
            let typename = apply_variable_bind(base_type.clone(), &def.bind, &None)?;
            member_map.insert(name.clone(), typename.clone());
            members.push(ir::StructMember { name, typename });
        }
    }
    let name = &sd.name;
    match context.insert_struct(name, member_map) {
        Some(id) => {
            let struct_def = ir::StructDefinition { id, members };
            Ok(ir::RootDefinition::Struct(struct_def))
        }
        None => Err(TyperError::StructAlreadyDefined(name.clone())),
    }
}

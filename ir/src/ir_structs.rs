use crate::ir_functions::FunctionDefinition;
use crate::ir_types::Type;
use crate::{StructId, StructTemplateId};

#[derive(PartialEq, Debug, Clone)]
pub struct StructDefinition {
    pub id: StructId,
    pub members: Vec<StructMember>,
    pub methods: Vec<FunctionDefinition>,
}

#[derive(PartialEq, Debug, Clone)]
pub struct StructMember {
    pub name: String,
    pub typename: Type,
}

#[derive(PartialEq, Debug, Clone)]
pub struct StructTemplateDefinition {
    pub id: StructTemplateId,
    pub ast: rssl_ast::StructDefinition,
}

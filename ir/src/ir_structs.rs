use crate::ir_types::Type;
use crate::StructId;

#[derive(PartialEq, Debug, Clone)]
pub struct StructDefinition {
    pub id: StructId,
    pub members: Vec<StructMember>,
}
#[derive(PartialEq, Debug, Clone)]
pub struct StructMember {
    pub name: String,
    pub typename: Type,
}

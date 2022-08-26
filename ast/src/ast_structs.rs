use crate::ast_statements::VariableBind;
use crate::ast_types::Type;

#[derive(PartialEq, Debug, Clone)]
pub struct StructDefinition {
    pub name: String,
    pub members: Vec<StructMember>,
}

#[derive(PartialEq, Debug, Clone)]
pub struct StructMember {
    pub ty: Type,
    pub defs: Vec<StructMemberName>,
}

#[derive(PartialEq, Debug, Clone)]
pub struct StructMemberName {
    pub name: String,
    pub bind: VariableBind,
}

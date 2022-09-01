use crate::ast_functions::FunctionDefinition;
use crate::ast_statements::VariableBind;
use crate::ast_types::Type;
use rssl_text::Located;

#[derive(PartialEq, Debug, Clone)]
pub struct StructDefinition {
    pub name: Located<String>,
    pub members: Vec<StructEntry>,
}

#[derive(PartialEq, Debug, Clone)]
pub enum StructEntry {
    Variable(StructMember),
    Method(FunctionDefinition),
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

use crate::*;
use rssl_text::Located;

/// A definition for a struct in RSSL
#[derive(PartialEq, Debug, Clone)]
pub struct StructDefinition {
    pub name: Located<String>,
    pub base_types: Vec<Type>,
    pub template_params: TemplateParamList,
    pub members: Vec<StructEntry>,
}

/// A declaration inside a struct
#[derive(PartialEq, Debug, Clone)]
pub enum StructEntry {
    Variable(StructMember),
    Method(FunctionDefinition),
}

/// A declaration for a variable member inside a struct
#[derive(PartialEq, Debug, Clone)]
pub struct StructMember {
    pub ty: Type,
    pub defs: Vec<StructMemberName>,
    pub attributes: Vec<Attribute>,
}

/// The name part of a [StructMember] - to support multiple named variables declared on the same line
#[derive(PartialEq, Debug, Clone)]
pub struct StructMemberName {
    pub name: Located<String>,
    pub bind: VariableBind,
    pub semantic: Option<Semantic>,
}

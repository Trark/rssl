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
    pub defs: Vec<InitDeclarator>,
    pub attributes: Vec<Attribute>,
}

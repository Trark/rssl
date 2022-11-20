use crate::ir_functions::FunctionDefinition;
use crate::ir_types::Type;
use crate::{StructId, StructTemplateId};

/// A definition for a struct in RSSL
#[derive(PartialEq, Debug, Clone)]
pub struct StructDefinition {
    pub id: StructId,
    pub members: Vec<StructMember>,
    pub methods: Vec<FunctionDefinition>,
}

/// A declaration for a variable member inside a struct
#[derive(PartialEq, Debug, Clone)]
#[allow(clippy::derive_partial_eq_without_eq)]
pub struct StructMember {
    pub name: String,
    pub typename: Type,
}

/// A declaration for a struct template which is still in AST form
// TODO: Resolve all templates so we do not depend on AST
#[derive(PartialEq, Debug, Clone)]
pub struct StructTemplateDefinition {
    pub id: StructTemplateId,
    pub ast: rssl_ast::StructDefinition,
}

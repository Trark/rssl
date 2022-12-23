use crate::*;
use rssl_text::Located;

/// A definition for a struct in RSSL
#[derive(PartialEq, Debug, Clone)]
pub struct StructDefinition {
    /// Unique identifier for the struct
    pub id: StructId,

    /// Unique identifier for the type
    pub type_id: TypeId,

    /// Short name for the struct
    pub name: Located<String>,

    /// Fully qualified name for the struct
    pub full_name: ScopedName,

    /// Data members of the struct
    pub members: Vec<StructMember>,

    /// Function members of the struct
    pub methods: Vec<FunctionId>,
}

/// A declaration for a variable member inside a struct
#[derive(PartialEq, Debug, Clone)]
#[allow(clippy::derive_partial_eq_without_eq)]
pub struct StructMember {
    /// Name of the member
    pub name: String,

    /// Type of the member
    pub type_id: TypeId,

    /// Optional semantic for when the struct is used in an entry point
    pub semantic: Option<Semantic>,

    /// Optional interpolation modifier for when the struct is used in an entry point
    pub interpolation_modifier: Option<InterpolationModifier>,
}

/// A declaration for a struct template which is still in AST form
// TODO: Resolve all templates so we do not depend on AST
#[derive(PartialEq, Debug, Clone)]
pub struct StructTemplateDefinition {
    /// Unique identifier for the struct template
    pub id: StructTemplateId,

    /// Unique identifier for the type
    pub type_id: TypeId,

    /// Short name for the struct template
    pub name: Located<String>,

    /// The raw AST that is used to generate instances of this template
    pub ast: rssl_ast::StructDefinition,
}

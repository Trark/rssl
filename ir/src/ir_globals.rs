use crate::*;
use rssl_text::Located;

/// A global variable definition
#[derive(PartialEq, Debug, Clone)]
pub struct GlobalVariable {
    /// Short name for the variable
    pub name: Located<String>,

    /// Namespace the variable is declared in - or None if it is not in a namespace
    pub namespace: Option<NamespaceId>,

    /// Type for the global variable
    pub type_id: TypeId,

    /// Storage class for the global variable
    pub storage_class: GlobalStorage,

    /// Binding point from the users perspective
    pub lang_slot: LanguageBinding,

    /// Binding point from the target APIs perspective
    pub api_slot: Option<ApiBinding>,

    /// Initializer for the global variable
    pub init: Option<Initializer>,

    /// Compile time evaluated value
    pub constexpr_value: Option<Constant>,
}

/// A constant buffer definition
#[derive(PartialEq, Eq, Debug, Clone)]
pub struct ConstantBuffer {
    /// Short name for the constant buffer
    pub name: Located<String>,

    /// Namespace the constant buffer is declared in - or None if it is not in a namespace
    pub namespace: Option<NamespaceId>,

    /// Binding point from the users perspective
    pub lang_binding: LanguageBinding,

    /// Binding point from the target APIs perspective
    pub api_binding: Option<ApiBinding>,

    /// Constant buffer member variables
    pub members: Vec<ConstantVariable>,
}

/// A variable definition inside a constant buffer
///
/// Not really "variable"...
#[derive(PartialEq, Eq, Debug, Clone)]
pub struct ConstantVariable {
    /// Name of the member
    pub name: Located<String>,

    /// Type of the constant buffer member
    pub type_id: TypeId,

    /// Packing information
    pub offset: Option<PackOffset>,
}

/// Binding slot from the perspective of language
#[derive(PartialEq, Eq, Debug, Copy, Clone, Default)]
pub struct LanguageBinding {
    /// Descriptor set index
    ///
    /// As the default space is always zero this value is always valid
    pub set: u32,

    /// Binding number into the descriptor set
    ///
    /// This is [None] for unspecified register index values before automatic slot assignment
    pub index: Option<u32>,
}

/// Binding slot from the perspective of target api
#[derive(PartialEq, Eq, Debug, Copy, Clone)]
pub struct ApiBinding {
    /// Descriptor set index
    pub set: u32,

    /// Where the binding is bound to
    pub location: ApiLocation,

    /// Type of binding or None if we do not care about the type
    pub slot_type: Option<RegisterType>,
}

/// Api location a binding is bound to
#[derive(PartialEq, Eq, Debug, Copy, Clone)]
pub enum ApiLocation {
    /// Binding number into the descriptor set
    Index(u32),

    /// Byte offset into an inline constant buffer
    InlineConstant(u32),
}

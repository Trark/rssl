use crate::*;
use rssl_text::Located;

/// A global variable definition
#[derive(PartialEq, Debug, Clone)]
pub struct GlobalVariable {
    /// Unique identifier for the global variable
    pub id: GlobalId,

    /// Short name for the variable
    pub name: Located<String>,

    /// Fully qualified name for the varibale
    pub full_name: ScopedName,

    /// Type for the global variable including global-specific modifiers
    pub global_type: GlobalType,

    /// Binding point from the users perspective
    pub lang_slot: Option<LanguageBinding>,

    /// Binding point from the target APIs perspective
    pub api_slot: Option<ApiBinding>,

    /// Initializer for the global variable
    pub init: Option<Initializer>,
}

/// The type of any global declaration
#[derive(PartialEq, Eq, Debug, Clone)]
pub struct GlobalType(pub TypeId, pub GlobalStorage);

/// A constant buffer definition
#[derive(PartialEq, Eq, Debug, Clone)]
pub struct ConstantBuffer {
    /// Unique identifier for the constant buffer
    pub id: ConstantBufferId,

    /// Short name for the constant buffer
    pub name: Located<String>,

    /// Binding point from the users perspective
    pub lang_binding: Option<LanguageBinding>,

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
    pub name: String,
    pub typename: TypeLayout,
    pub offset: Option<PackOffset>,
}

/// Binding slot from the perspective of language
#[derive(PartialEq, Eq, Debug, Copy, Clone)]
pub struct LanguageBinding {
    /// Descriptor set index
    pub set: u32,

    /// Binding number into the descriptor set
    pub index: u32,
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

use crate::*;

/// A global variable definition
#[derive(PartialEq, Debug, Clone)]
pub struct GlobalVariable {
    pub id: GlobalId,
    pub global_type: GlobalType,
    pub lang_slot: Option<LanguageBinding>,
    pub api_slot: Option<ApiBinding>,
    pub init: Option<Initializer>,
}

/// The type of any global declaration
#[derive(PartialEq, Eq, Debug, Clone)]
pub struct GlobalType(pub Type, pub GlobalStorage);

/// A constant buffer definition
#[derive(PartialEq, Eq, Debug, Clone)]
pub struct ConstantBuffer {
    pub id: ConstantBufferId,
    pub lang_slot: Option<LanguageBinding>,
    pub api_slot: Option<ApiBinding>,
    pub members: Vec<ConstantVariable>,
}

/// A variable definition inside a constant buffer
///
/// Not really "variable"...
#[derive(PartialEq, Eq, Debug, Clone)]
pub struct ConstantVariable {
    pub name: String,
    pub typename: Type,
    pub offset: Option<PackOffset>,
}

/// Binding slot from the perspective of language
#[derive(PartialEq, Eq, Debug, Copy, Clone)]
pub struct LanguageBinding {
    /// Descriptor set index
    pub set: u32,

    /// Slot index in set
    pub index: u32,
}

/// Binding slot from the perspective of target api
#[derive(PartialEq, Eq, Debug, Copy, Clone)]
pub struct ApiBinding {
    /// Descriptor set index
    pub set: u32,

    /// Slot index in set
    pub index: u32,

    /// Type of binding or None if we do not care about the type
    pub slot_type: Option<RegisterType>,
}

impl From<Type> for GlobalType {
    fn from(ty: Type) -> GlobalType {
        GlobalType(ty, GlobalStorage::default())
    }
}

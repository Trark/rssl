use crate::*;

/// A global variable definition
#[derive(PartialEq, Debug, Clone)]
pub struct GlobalVariable {
    pub id: GlobalId,
    pub global_type: GlobalType,
    pub slot: Option<GlobalSlot>,
    pub init: Option<Initializer>,
}

/// The type of any global declaration
#[derive(PartialEq, Eq, Debug, Clone)]
pub struct GlobalType(pub Type, pub GlobalStorage);

/// A constant buffer definition
#[derive(PartialEq, Eq, Debug, Clone)]
pub struct ConstantBuffer {
    pub id: ConstantBufferId,
    pub slot: Option<GlobalSlot>,
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

impl From<Type> for GlobalType {
    fn from(ty: Type) -> GlobalType {
        GlobalType(ty, GlobalStorage::default())
    }
}

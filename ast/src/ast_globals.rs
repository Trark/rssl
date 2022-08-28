use crate::ast_statements::{Initializer, VariableBind};
use crate::ast_types::Type;
use crate::primitive_types::*;
use rssl_text::Located;

#[derive(PartialEq, Debug, Clone)]
pub struct GlobalVariable {
    pub global_type: GlobalType,
    pub defs: Vec<GlobalVariableName>,
}

/// The type of any global declaration
#[derive(PartialEq, Debug, Clone)]
pub struct GlobalType(
    pub Type,
    pub GlobalStorage,
    pub Option<InterpolationModifier>,
);

#[derive(PartialEq, Debug, Clone)]
pub struct GlobalVariableName {
    pub name: Located<String>,
    pub bind: VariableBind,
    pub slot: Option<GlobalSlot>,
    pub init: Option<Initializer>,
}

#[derive(PartialEq, Debug, Clone)]
pub enum GlobalSlot {
    ReadSlot(u32),
    ReadWriteSlot(u32),
    SamplerSlot(u32),
    ConstantSlot(u32),
}

#[derive(PartialEq, Debug, Clone)]
pub struct ConstantBuffer {
    pub name: Located<String>,
    pub slot: Option<ConstantSlot>,
    pub members: Vec<ConstantVariable>,
}

#[derive(PartialEq, Debug, Clone)]
pub struct ConstantSlot(pub u32);

#[derive(PartialEq, Debug, Clone)]
pub struct ConstantVariable {
    pub ty: Type,
    pub defs: Vec<ConstantVariableName>,
}

#[derive(PartialEq, Debug, Clone)]
pub struct ConstantVariableName {
    pub name: String,
    pub bind: VariableBind,
    pub offset: Option<PackOffset>,
}

#[derive(PartialEq, Debug, Clone)]
pub struct PackOffset(pub u32, pub PackSubOffset);

#[derive(PartialEq, Debug, Clone)]
pub enum PackSubOffset {
    None,
    X,
    Y,
    Z,
    W,
}

impl From<Type> for GlobalType {
    fn from(ty: Type) -> GlobalType {
        GlobalType(ty, GlobalStorage::default(), None)
    }
}

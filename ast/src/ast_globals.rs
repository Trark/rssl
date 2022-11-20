use crate::ast_statements::{Initializer, VariableBind};
use crate::ast_types::Type;
use crate::primitive_types::*;
use rssl_text::Located;

/// A global variable definition
#[derive(PartialEq, Debug, Clone)]
pub struct GlobalVariable {
    pub global_type: GlobalType,
    pub defs: Vec<GlobalVariableName>,
}

/// The type of any global declaration
#[derive(PartialEq, Debug, Clone)]
pub struct GlobalType(pub Type, pub GlobalStorage);

/// The name part of a global variable definition - to support multiple definitions in a single line
#[derive(PartialEq, Debug, Clone)]
pub struct GlobalVariableName {
    pub name: Located<String>,
    pub bind: VariableBind,
    pub slot: Option<GlobalSlot>,
    pub init: Option<Initializer>,
}

/// The resource binding slot for a global parameter
#[derive(PartialEq, Eq, Debug, Clone)]
pub enum GlobalSlot {
    ReadSlot(u32),
    ReadWriteSlot(u32),
    SamplerSlot(u32),
    ConstantSlot(u32),
}

/// A constant buffer definition
#[derive(PartialEq, Debug, Clone)]
pub struct ConstantBuffer {
    pub name: Located<String>,
    pub slot: Option<ConstantSlot>,
    pub members: Vec<ConstantVariable>,
}

/// The resource binding slot for a global constant buffer parameter
#[derive(PartialEq, Eq, Debug, Clone)]
pub struct ConstantSlot(pub u32);

/// A variable definition inside a constant buffer
///
/// Not really "variable"...
#[derive(PartialEq, Debug, Clone)]
pub struct ConstantVariable {
    pub ty: Type,
    pub defs: Vec<ConstantVariableName>,
}

/// The name part of a constant buffer variable definition - to support multiple definitions in a single line
#[derive(PartialEq, Debug, Clone)]
pub struct ConstantVariableName {
    pub name: String,
    pub bind: VariableBind,
    pub offset: Option<PackOffset>,
}

/// Offset information for a packoffset construct
#[derive(PartialEq, Eq, Debug, Clone)]
pub struct PackOffset(pub u32, pub PackSubOffset);

/// The channel to offset in a [PackOffset]
#[derive(PartialEq, Eq, Debug, Clone)]
pub enum PackSubOffset {
    None,
    X,
    Y,
    Z,
    W,
}

impl From<Type> for GlobalType {
    fn from(ty: Type) -> GlobalType {
        GlobalType(ty, GlobalStorage::default())
    }
}

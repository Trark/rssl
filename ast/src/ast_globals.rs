use crate::ast_statements::Attribute;
use crate::ast_types::Type;
use crate::{InitDeclarator, LocationAnnotation};
use rssl_text::Located;

/// A global variable definition
#[derive(PartialEq, Debug, Clone)]
pub struct GlobalVariable {
    /// The base type
    pub global_type: Type,

    /// List of declarators defining a variable each
    pub defs: Vec<InitDeclarator>,

    /// Attributes that appear before the type
    pub attributes: Vec<Attribute>,
}

/// The resource binding annotation for a global parameter
#[derive(PartialEq, Eq, Debug, Clone)]
pub struct Register {
    /// Slot type and index
    pub slot: Option<RegisterSlot>,

    /// Space index the register is bound into
    pub space: Option<u32>,
}

/// The resource binding slot for a register annotation
#[derive(PartialEq, Eq, Debug, Clone)]
pub struct RegisterSlot {
    /// Type of resource that is bound
    pub slot_type: RegisterType,

    /// Slot index the resource is bound into
    pub index: u32,
}

/// The resource binding slot type for a global parameter
#[derive(PartialEq, Eq, Debug, Copy, Clone)]
pub enum RegisterType {
    T,
    U,
    S,
    B,
}

/// A constant buffer definition
#[derive(PartialEq, Debug, Clone)]
pub struct ConstantBuffer {
    pub name: Located<String>,
    pub location_annotations: Vec<LocationAnnotation>,
    pub members: Vec<ConstantVariable>,
    pub attributes: Vec<Attribute>,
}

/// A variable definition inside a constant buffer
///
/// Not really "variable"...
#[derive(PartialEq, Debug, Clone)]
pub struct ConstantVariable {
    pub ty: Type,
    pub defs: Vec<InitDeclarator>,
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

impl std::fmt::Display for RegisterType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RegisterType::T => write!(f, "t"),
            RegisterType::U => write!(f, "u"),
            RegisterType::S => write!(f, "s"),
            RegisterType::B => write!(f, "b"),
        }
    }
}

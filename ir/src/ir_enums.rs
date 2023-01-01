use crate::*;
use rssl_text::Located;

/// Container of all registered enums
#[derive(PartialEq, Clone, Default, Debug)]
pub struct EnumRegistry {
    /// The main data for an enum
    definitions: Vec<EnumDefinition>,

    /// The type id used which links to each base enum id without modifiers
    type_ids: Vec<TypeId>,

    /// The main data for an enum value
    enum_values: Vec<EnumValue>,
}

/// A definition for an enum in RSSL
#[derive(PartialEq, Debug, Clone)]
#[allow(clippy::derive_partial_eq_without_eq)]
pub struct EnumDefinition {
    /// Short name for the enum
    pub name: Located<String>,

    /// Fully qualified name for the enum
    pub full_name: ScopedName,
}

/// Id to an enum value
#[derive(PartialEq, Eq, Hash, PartialOrd, Ord, Debug, Clone, Copy)]
pub struct EnumValueId(pub u32);

/// An enum value inside an enum type
#[derive(PartialEq, Debug, Clone)]
#[allow(clippy::derive_partial_eq_without_eq)]
pub struct EnumValue {
    /// Id of the enum the value is for
    pub enum_id: EnumId,

    /// Id of the base type  for the num the value is for
    pub type_id: TypeId,

    /// Name of the value
    pub name: Located<String>,

    /// Value of the value
    pub value: Constant,
}

impl EnumRegistry {
    /// Register a new enum
    pub fn register_enum(&mut self, def: EnumDefinition) -> EnumId {
        let id = EnumId(self.definitions.len() as u32);
        self.definitions.push(def);

        // Set the type id to an invalid value - which we expect to be filled in almost immediately
        self.type_ids.push(TypeId(u32::MAX));

        id
    }

    /// Set the base type id for an enum
    pub fn set_enum_type_id(&mut self, id: EnumId, type_id: TypeId) {
        assert_eq!(self.type_ids[id.0 as usize], TypeId(u32::MAX));
        self.type_ids[id.0 as usize] = type_id;
    }

    /// Register a new enum value
    pub fn register_enum_value(
        &mut self,
        enum_id: EnumId,
        name: Located<String>,
        value: Constant,
    ) -> EnumValueId {
        let type_id = self.get_type_id(enum_id);

        let id = EnumValueId(self.enum_values.len() as u32);
        self.enum_values.push(EnumValue {
            enum_id,
            type_id,
            name,
            value,
        });

        id
    }

    /// Get the definition from an enum id
    #[inline]
    pub fn get_enum_definition(&self, id: EnumId) -> &EnumDefinition {
        &self.definitions[id.0 as usize]
    }

    /// Set the base type id for an enum
    #[inline]
    pub fn get_type_id(&self, id: EnumId) -> TypeId {
        assert_ne!(self.type_ids[id.0 as usize], TypeId(u32::MAX));
        self.type_ids[id.0 as usize]
    }

    /// Set the base type id for an enum
    #[inline]
    pub fn get_enum_value(&self, id: EnumValueId) -> &EnumValue {
        &self.enum_values[id.0 as usize]
    }
}

use crate::*;
use rssl_text::Located;

/// Container of all registered enums
#[derive(PartialEq, Clone, Default, Debug)]
pub struct EnumRegistry {
    /// The main data for an enum
    definitions: Vec<EnumDefinition>,

    /// The type id used which links to each base enum id without modifiers
    type_ids: Vec<TypeId>,

    /// The type id used which links to the underlying type for the enum
    underlying_type_ids: Vec<TypeId>,

    /// The scalar type for the underlying type for the enum
    underlying_scalars: Vec<ScalarType>,

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

    /// Id of the base type for the enum the value is for
    pub type_id: TypeId,

    /// Name of the value
    pub name: Located<String>,

    /// Underlying value of the enum value
    pub value: Constant,

    /// Id for the type of the constant
    pub underlying_type_id: TypeId,
}

impl EnumRegistry {
    /// Register a new enum
    pub fn register_enum(&mut self, def: EnumDefinition) -> EnumId {
        let id = EnumId(self.definitions.len() as u32);
        self.definitions.push(def);

        // Set the type id to an invalid value - which we expect to be filled in almost immediately
        self.type_ids.push(TypeId(u32::MAX));
        self.underlying_type_ids.push(TypeId(u32::MAX));
        self.underlying_scalars.push(ScalarType::UntypedInt);

        id
    }

    /// Set the base type id for an enum
    pub fn set_enum_type_id(&mut self, id: EnumId, type_id: TypeId) {
        assert_eq!(self.type_ids[id.0 as usize], TypeId(u32::MAX));
        self.type_ids[id.0 as usize] = type_id;
    }

    /// Set the base type id for an enum
    pub fn set_underlying_type_id(
        &mut self,
        id: EnumId,
        underlying_id: TypeId,
        scalar: ScalarType,
    ) {
        assert_eq!(self.underlying_type_ids[id.0 as usize], TypeId(u32::MAX));
        self.underlying_type_ids[id.0 as usize] = underlying_id;
        self.underlying_scalars[id.0 as usize] = scalar;
    }

    /// Register a new enum value
    pub fn register_enum_value(
        &mut self,
        enum_id: EnumId,
        name: Located<String>,
        value: Constant,
        underlying_type_id: TypeId,
    ) -> EnumValueId {
        let type_id = self.get_type_id(enum_id);

        let id = EnumValueId(self.enum_values.len() as u32);
        self.enum_values.push(EnumValue {
            enum_id,
            type_id,
            name,
            value,
            underlying_type_id,
        });

        id
    }

    /// Get the definition from an enum id
    #[inline]
    pub fn get_enum_definition(&self, id: EnumId) -> &EnumDefinition {
        &self.definitions[id.0 as usize]
    }

    /// Get the base type id for an enum
    #[inline]
    pub fn get_type_id(&self, id: EnumId) -> TypeId {
        assert_ne!(self.type_ids[id.0 as usize], TypeId(u32::MAX));
        self.type_ids[id.0 as usize]
    }

    /// Get the underlying type id for an enum
    #[inline]
    pub fn get_underlying_type_id(&self, id: EnumId) -> TypeId {
        assert_ne!(self.type_ids[id.0 as usize], TypeId(u32::MAX));
        self.underlying_type_ids[id.0 as usize]
    }

    /// Get the underlying type id for an enum
    #[inline]
    pub fn get_underlying_scalar(&self, id: EnumId) -> ScalarType {
        assert_ne!(
            self.underlying_scalars[id.0 as usize],
            ScalarType::UntypedInt
        );
        self.underlying_scalars[id.0 as usize]
    }

    /// Get the definition for an enum value
    #[inline]
    pub fn get_enum_value(&self, id: EnumValueId) -> &EnumValue {
        &self.enum_values[id.0 as usize]
    }
}

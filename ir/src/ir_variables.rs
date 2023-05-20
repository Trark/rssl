use crate::*;
use rssl_text::Located;

/// Id to function (in global scope)
#[derive(PartialEq, Eq, Hash, PartialOrd, Ord, Debug, Clone, Copy)]
pub struct FunctionId(pub u32);

/// Id to constant buffer
#[derive(PartialEq, Eq, Hash, PartialOrd, Ord, Debug, Clone, Copy)]
pub struct ConstantBufferId(pub u32);

/// Id to a global variable
#[derive(PartialEq, Eq, Hash, PartialOrd, Ord, Debug, Clone, Copy)]
pub struct GlobalId(pub u32);

/// Id to variable in current scope
#[derive(PartialEq, Eq, Hash, PartialOrd, Ord, Debug, Clone, Copy)]
pub struct VariableId(pub u32);

/// Container of all registered local variables
#[derive(PartialEq, Clone, Debug)]
pub struct VariableRegistry {
    local_variables: Vec<LocalVariable>,
}

/// Function local variable state
#[derive(PartialEq, Debug, Clone)]
pub struct LocalVariable {
    /// Name for the local variable
    pub name: Located<String>,

    /// Type for the local variable
    pub type_id: TypeId,

    /// Storage class for the local variable
    pub storage_class: LocalStorage,

    /// If the variable is considered precise
    pub precise: bool,
}

impl VariableRegistry {
    /// Register a new local variable with the module
    pub fn register_local_variable(&mut self, def: LocalVariable) -> VariableId {
        let id = VariableId(self.local_variables.len() as u32);

        self.local_variables.push(def);

        id
    }

    /// Get the stored data for a local variable from an id
    pub fn get_local_variable(&self, id: VariableId) -> &LocalVariable {
        &self.local_variables[id.0 as usize]
    }
}

impl Default for VariableRegistry {
    fn default() -> Self {
        VariableRegistry {
            local_variables: Vec::with_capacity(1024),
        }
    }
}

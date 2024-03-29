use crate::*;
use rssl_text::Located;

/// Id to function (in global scope)
#[derive(PartialEq, Eq, Hash, PartialOrd, Ord, Debug, Clone, Copy)]
pub struct FunctionId(pub u32);

/// Id to constant buffer
#[derive(PartialEq, Eq, Hash, PartialOrd, Ord, Debug, Clone, Copy)]
pub struct ConstantBufferId(pub u32);

/// Id to constant buffer member
#[derive(PartialEq, Eq, Hash, PartialOrd, Ord, Debug, Clone, Copy)]
pub struct ConstantBufferMemberId(pub ConstantBufferId, pub u32);

/// Id to a global variable
#[derive(PartialEq, Eq, Hash, PartialOrd, Ord, Debug, Clone, Copy)]
pub struct GlobalId(pub u32);

/// Id to variable in current scope
#[derive(PartialEq, Eq, Hash, PartialOrd, Ord, Debug, Clone, Copy)]
pub struct VariableId(pub u32);

/// Id to a template value definition
#[derive(PartialEq, Eq, Hash, PartialOrd, Ord, Debug, Clone, Copy)]
pub struct TemplateValueId(pub u32);

/// Container of all registered local variables
#[derive(PartialEq, Clone, Debug)]
pub struct VariableRegistry {
    local_variables: Vec<LocalVariable>,

    template_values: Vec<TemplateParamValue>,
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

    /// Compile time evaluated value
    pub constexpr_value: Option<Constant>,
}

/// Template parameter value state
#[derive(PartialEq, Debug, Clone)]
pub struct TemplateParamValue {
    /// Name for the template parameter
    pub name: Option<Located<String>>,

    /// Type for the template parameter
    pub type_id: TypeId,

    /// Index into list of template arguments the parameter is contained in
    pub positional_index: u32,
}

impl VariableRegistry {
    /// Register a new local variable with the module
    pub fn register_local_variable(&mut self, def: LocalVariable) -> VariableId {
        let id = VariableId(self.local_variables.len() as u32);

        self.local_variables.push(def);

        id
    }

    /// Get the total number of registered variables
    pub fn get_variable_count(&self) -> u32 {
        self.local_variables.len() as u32
    }

    /// Iterate the set of valid variable ids
    pub fn iter(&self) -> VariableIdIterator {
        VariableIdIterator {
            current: 0,
            end: self.get_variable_count(),
        }
    }

    /// Get the stored data for a local variable from an id
    pub fn get_local_variable(&self, id: VariableId) -> &LocalVariable {
        &self.local_variables[id.0 as usize]
    }

    /// Register a new template value parameter with the module
    pub fn register_template_value(&mut self, def: TemplateParamValue) -> TemplateValueId {
        let id = TemplateValueId(self.template_values.len() as u32);

        self.template_values.push(def);

        id
    }

    /// Get the stored data for a template value parameter from an id
    pub fn get_template_value(&self, id: TemplateValueId) -> &TemplateParamValue {
        &self.template_values[id.0 as usize]
    }
}

impl Default for VariableRegistry {
    fn default() -> Self {
        VariableRegistry {
            local_variables: Vec::with_capacity(1024),
            template_values: Vec::with_capacity(1024),
        }
    }
}

pub struct VariableIdIterator {
    current: u32,
    end: u32,
}

impl Iterator for VariableIdIterator {
    type Item = VariableId;

    fn next(&mut self) -> Option<Self::Item> {
        let result = if self.current < self.end {
            Some(VariableId(self.current))
        } else {
            None
        };
        self.current += 1;
        result
    }
}

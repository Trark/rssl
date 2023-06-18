/// Id to namespace
#[derive(PartialEq, Eq, Hash, PartialOrd, Ord, Debug, Clone, Copy)]
pub struct NamespaceId(pub u32);

/// Container of all registered namespaces
#[derive(PartialEq, Clone, Debug, Default)]
pub struct NamespaceRegistry {
    names: Vec<String>,
    parents: Vec<Option<NamespaceId>>,
}

impl NamespaceRegistry {
    /// Register a new local variable with the module
    pub fn register_namespace(&mut self, name: String, parent: Option<NamespaceId>) -> NamespaceId {
        let id = NamespaceId(self.names.len() as u32);

        self.names.push(name);
        self.parents.push(parent);

        id
    }

    /// Get the leaf name for a namespace from an id
    pub fn get_namespace_name(&self, id: NamespaceId) -> &str {
        &self.names[id.0 as usize]
    }

    /// Get the parent namespace - or None if the parent is the global namespace
    pub fn get_namespace_parent(&self, id: NamespaceId) -> Option<NamespaceId> {
        self.parents[id.0 as usize]
    }
}

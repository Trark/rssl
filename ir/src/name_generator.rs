use crate::*;
use std::collections::{HashMap, HashSet};

/// Map of generated names for each symbol
#[derive(Debug)]
pub struct NameMap {
    names: HashMap<NameSymbol, NameString>,
}

/// Any kind of named symbol
#[derive(Debug, Eq, PartialEq, Hash, Copy, Clone)]
pub enum NameSymbol {
    Struct(StructId),
    Enum(EnumId),
    GlobalVariable(GlobalId),
    Function(FunctionId),
    Namespace(NamespaceId),
    LocalVariable(VariableId),
}

/// A name which may have namespace qualification
pub struct ScopedName(pub Vec<String>);

#[derive(Debug)]
struct NameString {
    namespace: Option<NamespaceId>,
    name: String,
}

impl NameMap {
    /// Construct a name map from the symbols within a module
    pub fn build(
        module: &Module,
        reserved_names: &[&str],
        intrinsics_are_reserved: bool,
    ) -> NameMap {
        let mut name_map = NameMap {
            names: HashMap::new(),
        };

        let mut scopes: HashMap<Option<NamespaceId>, HashMap<String, Vec<NameSymbol>>> =
            HashMap::new();

        scopes.insert(None, HashMap::new());

        for i in 0..module.namespace_registry.get_namespace_count() {
            let id = NamespaceId(i);

            let parent = module.namespace_registry.get_namespace_parent(id);
            let name = module.namespace_registry.get_namespace_name(id).to_string();

            // Insert the name into the parent scope
            let name_vec = scopes.get_mut(&parent).unwrap().entry(name).or_default();
            name_vec.push(NameSymbol::Namespace(id));

            // Add namespace as a scope
            scopes.insert(Some(id), HashMap::new());
        }

        for i in 0..module.struct_registry.len() {
            let id = StructId(i as u32);
            let def = &module.struct_registry[id.0 as usize];

            // Insert the name into containing scope
            let name_vec = scopes
                .get_mut(&def.namespace)
                .unwrap()
                .entry(def.name.node.clone())
                .or_default();
            name_vec.push(NameSymbol::Struct(id));
        }

        for i in 0..module.enum_registry.get_enum_count() {
            let id = EnumId(i);
            let def = &module.enum_registry.get_enum_definition(id);

            // Insert the name into containing scope
            let name_vec = scopes
                .get_mut(&def.namespace)
                .unwrap()
                .entry(def.name.node.clone())
                .or_default();
            name_vec.push(NameSymbol::Enum(id));
        }

        for i in 0..module.global_registry.len() {
            let id = GlobalId(i as u32);
            let def = &module.global_registry[id.0 as usize];

            // Do not assign names to intrinsics
            if def.is_intrinsic {
                if intrinsics_are_reserved {
                    // All intrinsic values should have names that are reserved
                    assert!(
                        reserved_names.contains(&def.name.node.as_str()),
                        "{}",
                        def.name.node
                    );
                }
                continue;
            }

            // Insert the name into containing scope
            let name_vec = scopes
                .get_mut(&def.namespace)
                .unwrap()
                .entry(def.name.node.clone())
                .or_default();
            name_vec.push(NameSymbol::GlobalVariable(id));
        }

        for id in module.function_registry.iter() {
            // Do not assign names to intrinsics
            let is_intrinsic = module.function_registry.get_intrinsic_data(id).is_some();
            if is_intrinsic {
                continue;
            }

            // Do not assign names to template functions - only the instantiations
            let sig = module.function_registry.get_function_signature(id);
            let is_template = !sig.template_params.is_empty();
            let is_template_inst = module
                .function_registry
                .get_template_instantiation_data(id)
                .is_some();
            if is_template && !is_template_inst {
                continue;
            }

            let name = module.function_registry.get_function_name_definition(id);

            // Insert the name into containing scope
            let name_vec = scopes
                .get_mut(&name.namespace)
                .unwrap()
                .entry(name.name.node.clone())
                .or_default();
            name_vec.push(NameSymbol::Function(id));
        }

        // Create set of reserved names to use as base for each scopes used names list
        // Reserved names are reserved in all scopes
        let mut reserved_name_set = HashSet::new();
        for name in reserved_names {
            reserved_name_set.insert(name.to_string());
        }

        let mut used_names_all_scopes = reserved_name_set.clone();

        for scope in &scopes {
            // Record used names within the current scope
            // Names may be reused in different namespaces
            let mut used_names = reserved_name_set.clone();

            let namespace = *scope.0;

            // Sort map first to ensure if a name generates a conflict with another generated name it will be consistent
            let mut name_to_symbol_vec = Vec::from_iter(scope.1.iter());
            name_to_symbol_vec.sort_by(|l, r| String::cmp(l.0, r.0));

            for (name, symbols) in name_to_symbol_vec {
                for symbol in symbols {
                    // Assign a name
                    let name = if symbols.len() == 1 && used_names.insert(name.clone()) {
                        // If there are no duplicate names and the direct name is free then use that
                        name.clone()
                    } else {
                        // Attempt to assign a name with an incrementing index
                        let mut counter = 0;
                        loop {
                            let candidate = format!("{}_{}", name, counter);

                            if used_names.insert(candidate.clone()) {
                                used_names_all_scopes.insert(candidate.clone());
                                break candidate;
                            }

                            counter += 1;
                        }
                    };

                    // Insert the name into the final map
                    if name_map
                        .names
                        .insert(*symbol, NameString { namespace, name })
                        .is_some()
                    {
                        panic!("duplicate name for {:?}", symbol)
                    }
                }
            }
        }

        // We generally assume the names setup for local variables will not conflict
        // This will not be the case if we generate a name - so first find all the names we do not want to generate into

        let mut all_local_names = HashSet::new();
        for id in module.variable_registry.iter() {
            let name = &module.variable_registry.get_local_variable(id).name.node;
            all_local_names.insert(name);
        }

        // TODO: Member variable names

        for id in module.variable_registry.iter() {
            let name = &module.variable_registry.get_local_variable(id).name.node;

            let picked_name = if used_names_all_scopes.contains(name) {
                // Potential conflict
                // Names used for different kinds of objects may not conflict but we assume they are
                // Names used in other namespaces may not conflict but we assume they may

                // Attempt to assign a name with an incrementing index
                // This may not conflict with a global name or any other local name in the general case
                let mut counter = 0;
                loop {
                    let candidate = format!("{}_{}", name, counter);

                    if !all_local_names.contains(&candidate)
                        && used_names_all_scopes.insert(candidate.clone())
                    {
                        break candidate;
                    }

                    counter += 1;
                }
            } else {
                String::from(name)
            };

            let symbol = NameSymbol::LocalVariable(id);
            let name_string = NameString {
                namespace: None,
                name: picked_name,
            };

            if name_map.names.insert(symbol, name_string).is_some() {
                panic!("duplicate name for {:?}", symbol)
            }
        }

        name_map
    }

    /// Get the leaf name for a given symbol
    pub fn get_name_leaf(&self, symbol: NameSymbol) -> &str {
        let name = match self.names.get(&symbol) {
            Some(name) => name,
            None => panic!("No name for symbol {:?}", symbol),
        };
        &name.name
    }

    /// Get the qualified name for a given symbol
    pub fn get_name_qualified(&self, symbol: NameSymbol) -> ScopedName {
        let name = match self.names.get(&symbol) {
            Some(name) => name,
            None => panic!("No name for symbol {:?}", symbol),
        };

        let mut segmented_name = Vec::from([name.name.clone()]);
        let mut namespace = name.namespace;
        while let Some(current) = namespace {
            let current_name = self.names.get(&NameSymbol::Namespace(current)).unwrap();
            segmented_name.insert(0, current_name.name.clone());
            namespace = current_name.namespace;
        }

        ScopedName(segmented_name)
    }
}

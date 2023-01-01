use crate::*;
use rssl_text::SourceLocation;
use std::collections::{HashMap, HashSet};

/// Represents a full parsed and type checked source file
#[derive(PartialEq, Clone, Default, Debug)]
pub struct Module {
    /// Container of all registered types in the module
    pub type_registry: TypeRegistry,

    /// Container of all struct types
    pub struct_registry: Vec<StructDefinition>,

    /// Container of all struct template types
    pub struct_template_registry: Vec<StructTemplateDefinition>,

    /// Container for all registered enums in the module
    pub enum_registry: EnumRegistry,

    /// Container of all registered functions in the module
    pub function_registry: FunctionRegistry,

    /// Container for all global variables
    pub global_registry: Vec<GlobalVariable>,

    /// Container for all constant buffers
    pub cbuffer_registry: Vec<ConstantBuffer>,

    /// The root definitions in the module
    pub root_definitions: Vec<RootDefinition>,

    /// Flags that change how we build the module
    pub flags: ModuleFlags,

    /// Generated inline constant buffers
    pub inline_constant_buffers: Vec<InlineConstantBuffer>,
}

/// Metadata on config of a [Module]
#[derive(PartialEq, Eq, Debug, Clone, Default)]
pub struct ModuleFlags {
    pub assigned_api_slots: bool,
    pub requires_buffer_address: bool,
    pub requires_vk_binding: bool,
}

/// A single top level definition in an RSSL file
///
/// While this is called a "root" definition - it may be nested inside namespaces.
/// We still maintain namespace scoping and definition ordering as it makes exporting to HLSL easier.
#[derive(PartialEq, Debug, Clone)]
pub enum RootDefinition {
    Struct(StructId),
    StructTemplate(StructTemplateId),
    Enum(EnumId),
    ConstantBuffer(ConstantBufferId),
    GlobalVariable(GlobalId),
    Function(FunctionId),
    Namespace(String, Vec<RootDefinition>),
}

/// A name which may have namespace qualification
#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Clone)]
pub struct ScopedName(pub Vec<String>);

/// A definition of a constant buffer that serves other bindings
#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone)]
pub struct InlineConstantBuffer {
    /// Descriptor set
    pub set: u32,

    /// Api binding slot
    pub api_location: u32,

    /// Size of the constant buffer
    pub size_in_bytes: u32,
}

/// Parameters for [Module::assign_api_bindings]
pub struct AssignBindingsParams {
    /// Generate slot type information for bindings
    pub require_slot_type: bool,

    /// Build bindings with buffer addresses as constant inputs
    pub support_buffer_address: bool,
}

impl Module {
    /// Create a new module
    pub fn create() -> Self {
        let mut module = Module::default();
        intrinsic_data::add_intrinsics(&mut module);
        module
    }

    /// Get the name from a struct id
    pub fn get_struct_name(&self, id: StructId) -> &str {
        assert!(id.0 < self.struct_registry.len() as u32);
        &self.struct_registry[id.0 as usize].name.node
    }

    /// Get the name from a struct template_id
    pub fn get_struct_template_name(&self, id: StructTemplateId) -> &str {
        assert!(id.0 < self.struct_template_registry.len() as u32);
        &self.struct_template_registry[id.0 as usize].name.node
    }

    /// Get the name from a function id
    pub fn get_function_name(&self, id: FunctionId) -> &str {
        self.function_registry.get_function_name(id)
    }

    /// Get the name from a global variable id
    pub fn get_global_name(&self, id: GlobalId) -> &str {
        assert!(id.0 < self.global_registry.len() as u32);
        &self.global_registry[id.0 as usize].name.node
    }

    /// Get the name from a constant buffer id
    pub fn get_cbuffer_name(&self, id: ConstantBufferId) -> &str {
        assert!(id.0 < self.cbuffer_registry.len() as u32);
        &self.cbuffer_registry[id.0 as usize].name.node
    }

    /// Get the source location from a type
    pub fn get_type_location(&self, id: TypeId) -> SourceLocation {
        let id = self.type_registry.remove_modifier(id);
        match self.type_registry.get_type_layer(id) {
            TypeLayer::Struct(id) => {
                assert!(id.0 < self.struct_registry.len() as u32);
                self.struct_registry[id.0 as usize].name.location
            }
            _ => SourceLocation::UNKNOWN,
        }
    }

    /// Get the source location from a function id
    pub fn get_function_location(&self, id: FunctionId) -> SourceLocation {
        self.function_registry.get_function_location(id)
    }

    /// Get the source location from a constant buffer id
    pub fn get_cbuffer_location(&self, id: ConstantBufferId) -> SourceLocation {
        assert!(id.0 < self.cbuffer_registry.len() as u32);
        self.cbuffer_registry[id.0 as usize].name.location
    }

    /// Assign slot numbers to resource bindings
    ///
    /// Slots are assigned within a single namespace.
    /// Only object types will receive a slot.
    pub fn assign_slot_numbers(mut self) -> Self {
        // Search definitions for used slot numbers
        fn search_definition(
            module: &Module,
            decl: &RootDefinition,
            used_values: &mut HashSet<u32>,
        ) {
            match decl {
                RootDefinition::Struct(_)
                | RootDefinition::StructTemplate(_)
                | RootDefinition::Enum(_)
                | RootDefinition::Function(_) => {}
                RootDefinition::ConstantBuffer(id) => {
                    let cb = &module.cbuffer_registry[id.0 as usize];
                    if let Some(LanguageBinding {
                        set: 0,
                        index: slot,
                    }) = cb.lang_binding
                    {
                        used_values.insert(slot);
                    }
                }
                RootDefinition::GlobalVariable(id) => {
                    let decl = &module.global_registry[id.0 as usize];
                    if let Some(LanguageBinding {
                        set: 0,
                        index: slot,
                    }) = decl.lang_slot
                    {
                        used_values.insert(slot);
                    }
                }
                RootDefinition::Namespace(_, decls) => {
                    for decl in decls {
                        search_definition(module, decl, used_values)
                    }
                }
            }
        }

        let mut used_values = HashSet::new();
        for decl in &self.root_definitions {
            search_definition(&self, decl, &mut used_values)
        }

        // Add slot numbers to definitions without them
        fn process_definition(
            module: &mut Module,
            decl: &mut RootDefinition,
            used_values: &HashSet<u32>,
            next_value: &mut u32,
        ) {
            match decl {
                RootDefinition::Struct(_)
                | RootDefinition::StructTemplate(_)
                | RootDefinition::Enum(_)
                | RootDefinition::Function(_) => {}
                RootDefinition::ConstantBuffer(id) => {
                    let cb = &mut module.cbuffer_registry[id.0 as usize];
                    if cb.lang_binding.is_none() {
                        let mut slot = *next_value;
                        while used_values.contains(&slot) {
                            slot += 1;
                        }
                        *next_value = slot + 1;

                        cb.lang_binding = Some(LanguageBinding {
                            set: 0,
                            index: slot,
                        });
                    }
                }
                RootDefinition::GlobalVariable(id) => {
                    let decl = &mut module.global_registry[id.0 as usize];
                    if decl.lang_slot.is_none() {
                        // Find the slot type that we are adding
                        let tyl = module.type_registry.get_type_layout(decl.type_id);
                        if tyl.clone().remove_modifier().is_object() {
                            let mut slot = *next_value;
                            while used_values.contains(&slot) {
                                slot += 1;
                            }
                            *next_value = slot + 1;

                            decl.lang_slot = Some(LanguageBinding {
                                set: 0,
                                index: slot,
                            });
                        }
                    }
                }
                RootDefinition::Namespace(_, decls) => {
                    for decl in decls {
                        process_definition(module, decl, used_values, next_value)
                    }
                }
            }
        }

        let mut next_value = 0;
        for decl in &mut self.root_definitions.clone() {
            process_definition(&mut self, decl, &used_values, &mut next_value)
        }

        self
    }

    /// Assign api binding locations to all global resources
    pub fn assign_api_bindings(mut self, params: AssignBindingsParams) -> Self {
        assert!(!self.flags.assigned_api_slots);
        self.flags.assigned_api_slots = true;
        self.flags.requires_buffer_address = params.support_buffer_address;
        self.flags.requires_vk_binding = !params.require_slot_type || params.support_buffer_address;

        fn process_definition(
            module: &mut Module,
            decl: &RootDefinition,
            params: &AssignBindingsParams,
            used_slots: &mut HashMap<u32, u32>,
            inline_size: &mut HashMap<u32, u32>,
        ) {
            match decl {
                RootDefinition::Struct(_)
                | RootDefinition::StructTemplate(_)
                | RootDefinition::Enum(_)
                | RootDefinition::Function(_) => {}
                RootDefinition::ConstantBuffer(id) => {
                    let cb = &mut module.cbuffer_registry[id.0 as usize];
                    assert_eq!(cb.api_binding, None);
                    if let Some(lang_slot) = cb.lang_binding {
                        let index = match used_slots.entry(lang_slot.set) {
                            std::collections::hash_map::Entry::Occupied(mut o) => {
                                let slot = *o.get();
                                *o.get_mut() += 1;
                                slot
                            }
                            std::collections::hash_map::Entry::Vacant(v) => {
                                v.insert(1);
                                0
                            }
                        };

                        cb.api_binding = Some(ApiBinding {
                            set: lang_slot.set,
                            location: ApiLocation::Index(index),
                            slot_type: if params.require_slot_type {
                                Some(RegisterType::B)
                            } else {
                                None
                            },
                        });
                    }
                }
                RootDefinition::GlobalVariable(id) => {
                    let decl = &mut module.global_registry[id.0 as usize];
                    assert_eq!(decl.api_slot, None);
                    if let Some(lang_slot) = decl.lang_slot {
                        let tyl = module.type_registry.get_type_layout(decl.type_id);
                        if params.support_buffer_address && tyl.is_buffer_address() {
                            let offset = match inline_size.entry(lang_slot.set) {
                                std::collections::hash_map::Entry::Occupied(mut o) => {
                                    let slot = *o.get();
                                    *o.get_mut() += 8;
                                    slot
                                }
                                std::collections::hash_map::Entry::Vacant(v) => {
                                    v.insert(8);
                                    0
                                }
                            };

                            decl.api_slot = Some(ApiBinding {
                                set: lang_slot.set,
                                location: ApiLocation::InlineConstant(offset),
                                slot_type: None,
                            });

                            assert_eq!(decl.storage_class, GlobalStorage::Extern);
                            decl.storage_class = GlobalStorage::Static;
                        } else {
                            let index = match used_slots.entry(lang_slot.set) {
                                std::collections::hash_map::Entry::Occupied(mut o) => {
                                    let slot = *o.get();
                                    *o.get_mut() += 1;
                                    slot
                                }
                                std::collections::hash_map::Entry::Vacant(v) => {
                                    v.insert(1);
                                    0
                                }
                            };

                            decl.api_slot = Some(ApiBinding {
                                set: lang_slot.set,
                                location: ApiLocation::Index(index),
                                slot_type: if params.require_slot_type {
                                    let tyl = module.type_registry.get_type_layout(decl.type_id);
                                    Some(
                                        if let TypeLayout::Object(ot) =
                                            &tyl.clone().remove_modifier()
                                        {
                                            ot.get_register_type()
                                        } else {
                                            panic!("Non-object type has a global resource binding");
                                        },
                                    )
                                } else {
                                    None
                                },
                            });
                        }
                    }
                }
                RootDefinition::Namespace(_, decls) => {
                    for decl in decls {
                        process_definition(module, decl, params, used_slots, inline_size);
                    }
                }
            }
        }

        let mut used_slots = HashMap::new();
        let mut inline_size = HashMap::new();
        for decl in &self.root_definitions.clone() {
            process_definition(&mut self, decl, &params, &mut used_slots, &mut inline_size);
        }

        // Make an inline constant buffer to store bindings that can be stored as constants
        assert!(self.inline_constant_buffers.is_empty());
        for (set, size) in inline_size {
            let binding = *used_slots.get(&set).unwrap_or(&0);
            self.inline_constant_buffers.push(InlineConstantBuffer {
                set,
                api_location: binding,
                size_in_bytes: size,
            });
        }

        // Ensure buffers are in order for consistency
        self.inline_constant_buffers.sort();

        self
    }
}

impl ScopedName {
    /// Create a scoped name with no scoping
    pub fn unscoped(name: String) -> Self {
        ScopedName(Vec::from([name]))
    }
}

impl Default for AssignBindingsParams {
    fn default() -> Self {
        AssignBindingsParams {
            require_slot_type: true,
            support_buffer_address: false,
        }
    }
}

impl std::fmt::Display for ScopedName {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let (last, scopes) = self.0.split_last().unwrap();
        for scope in scopes {
            write!(f, "{}::", scope)?;
        }
        write!(f, "{}", last)
    }
}

impl std::fmt::Debug for ScopedName {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self)
    }
}

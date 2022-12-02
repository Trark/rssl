use crate::*;
use std::collections::{HashMap, HashSet};

/// Represents a full parsed and type checked source file
#[derive(PartialEq, Clone, Default, Debug)]
pub struct Module {
    /// The original names of all global declarations with are now internal ids
    pub global_declarations: GlobalDeclarations,

    /// Container of all registered types
    pub type_registry: Vec<TypeLayout>,

    /// Container of all struct types
    pub struct_registry: Vec<StructDefinition>,

    /// Container of all struct template types
    pub struct_template_registry: Vec<StructTemplateDefinition>,

    /// Container of all functions
    pub function_registry: Vec<Option<FunctionDefinition>>,

    /// Container for names of all functions
    pub function_name_registry: Vec<FunctionNameDefinition>,

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
    ConstantBuffer(ConstantBuffer),
    GlobalVariable(GlobalVariable),
    Function(FunctionId),
    Namespace(String, Vec<RootDefinition>),
}

/// Map of declarations in the global scope
#[derive(PartialEq, Clone, Default, Debug)]
#[allow(clippy::derive_partial_eq_without_eq)]
pub struct GlobalDeclarations {
    pub globals: HashMap<GlobalId, String>,
    pub constants: HashMap<ConstantBufferId, String>,
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
    /// Assign slot numbers to resource bindings
    ///
    /// Slots are assigned within a single namespace.
    /// Only object types will receive a slot.
    pub fn assign_slot_numbers(mut self) -> Self {
        // Search definitions for used slot numbers
        fn search_definition(decl: &RootDefinition, used_values: &mut HashSet<u32>) {
            match decl {
                RootDefinition::Struct(_)
                | RootDefinition::StructTemplate(_)
                | RootDefinition::Function(_) => {}
                RootDefinition::ConstantBuffer(cb) => {
                    if let Some(LanguageBinding {
                        set: 0,
                        index: slot,
                    }) = cb.lang_binding
                    {
                        used_values.insert(slot);
                    }
                }
                RootDefinition::GlobalVariable(decl) => {
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
                        search_definition(decl, used_values)
                    }
                }
            }
        }

        let mut used_values = HashSet::new();
        for decl in &self.root_definitions {
            search_definition(decl, &mut used_values)
        }

        // Add slot numbers to definitions without them
        fn process_definition(
            decl: &mut RootDefinition,
            used_values: &HashSet<u32>,
            next_value: &mut u32,
        ) {
            match decl {
                RootDefinition::Struct(_)
                | RootDefinition::StructTemplate(_)
                | RootDefinition::Function(_) => {}
                RootDefinition::ConstantBuffer(cb) => {
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
                RootDefinition::GlobalVariable(decl) => {
                    if decl.lang_slot.is_none() {
                        // Find the slot type that we are adding
                        let is_object = matches!(decl.global_type.0 .0, TypeLayout::Object(_));

                        if is_object {
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
                        process_definition(decl, used_values, next_value)
                    }
                }
            }
        }

        let mut next_value = 0;
        for decl in &mut self.root_definitions {
            process_definition(decl, &used_values, &mut next_value)
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
            decl: &mut RootDefinition,
            params: &AssignBindingsParams,
            used_slots: &mut HashMap<u32, u32>,
            inline_size: &mut HashMap<u32, u32>,
        ) {
            match decl {
                RootDefinition::Struct(_)
                | RootDefinition::StructTemplate(_)
                | RootDefinition::Function(_) => {}
                RootDefinition::ConstantBuffer(cb) => {
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
                RootDefinition::GlobalVariable(decl) => {
                    assert_eq!(decl.api_slot, None);
                    if let Some(lang_slot) = decl.lang_slot {
                        if params.support_buffer_address
                            && decl.global_type.0 .0.is_buffer_address()
                        {
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

                            assert_eq!(decl.global_type.1, GlobalStorage::Extern);
                            decl.global_type.1 = GlobalStorage::Static;
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
                                    Some(if let TypeLayout::Object(ot) = &decl.global_type.0 .0 {
                                        match ot {
                                            ObjectType::Buffer(_)
                                            | ObjectType::ByteAddressBuffer
                                            | ObjectType::BufferAddress
                                            | ObjectType::StructuredBuffer(_)
                                            | ObjectType::Texture2D(_) => RegisterType::T,

                                            ObjectType::RWBuffer(_)
                                            | ObjectType::RWByteAddressBuffer
                                            | ObjectType::RWBufferAddress
                                            | ObjectType::RWStructuredBuffer(_)
                                            | ObjectType::RWTexture2D(_) => RegisterType::U,

                                            ObjectType::ConstantBuffer(_) => RegisterType::B,
                                        }
                                    } else {
                                        panic!("Non-object type has a global resource binding");
                                    })
                                } else {
                                    None
                                },
                            });
                        }
                    }
                }
                RootDefinition::Namespace(_, decls) => {
                    for decl in decls {
                        process_definition(decl, params, used_slots, inline_size);
                    }
                }
            }
        }

        let mut used_slots = HashMap::new();
        let mut inline_size = HashMap::new();
        for decl in &mut self.root_definitions {
            process_definition(decl, &params, &mut used_slots, &mut inline_size);
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

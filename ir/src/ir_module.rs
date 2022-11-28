use crate::*;
use std::collections::{HashMap, HashSet};

/// Represents a full parsed and type checked source file
#[derive(PartialEq, Debug, Clone)]
pub struct Module {
    /// The original names of all global declarations with are now internal ids
    pub global_declarations: GlobalDeclarations,
    /// The root definitions in the module
    pub root_definitions: Vec<RootDefinition>,
}

/// A single top level definition in an RSSL file
///
/// While this is called a "root" definition - it may be nested inside namespaces.
/// We still maintain namespace scoping and definition ordering as it makes exporting to HLSL easier.
#[derive(PartialEq, Debug, Clone)]
pub enum RootDefinition {
    Struct(StructDefinition),
    StructTemplate(StructTemplateDefinition),
    ConstantBuffer(ConstantBuffer),
    GlobalVariable(GlobalVariable),
    Function(FunctionDefinition),
    Namespace(String, Vec<RootDefinition>),
}

/// Map of declarations in the global scope
#[derive(PartialEq, Debug, Clone)]
#[allow(clippy::derive_partial_eq_without_eq)]
pub struct GlobalDeclarations {
    pub functions: HashMap<FunctionId, ScopedName>,
    pub globals: HashMap<GlobalId, String>,
    pub structs: HashMap<StructId, ScopedName>,
    pub struct_templates: HashMap<StructTemplateId, String>,
    pub constants: HashMap<ConstantBufferId, String>,
}

/// A name which may have namespace qualification
#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Clone)]
pub struct ScopedName(pub Vec<String>);

/// Parameters for [Module::assign_api_bindings]
pub struct AssignBindingsParams {
    /// Generate slot type information for bindings
    require_slot_type: bool,
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
                    }) = cb.lang_slot
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
                    if cb.lang_slot.is_none() {
                        let mut slot = *next_value;
                        while used_values.contains(&slot) {
                            slot += 1;
                        }
                        *next_value = slot + 1;

                        cb.lang_slot = Some(LanguageBinding {
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
        fn process_definition(decl: &mut RootDefinition, params: &AssignBindingsParams) {
            match decl {
                RootDefinition::Struct(_)
                | RootDefinition::StructTemplate(_)
                | RootDefinition::Function(_) => {}
                RootDefinition::ConstantBuffer(cb) => {
                    assert_eq!(cb.api_slot, None);
                    if let Some(lang_slot) = cb.lang_slot {
                        cb.api_slot = Some(ApiBinding {
                            set: lang_slot.set,
                            index: lang_slot.index,
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
                        decl.api_slot = Some(ApiBinding {
                            set: lang_slot.set,
                            index: lang_slot.index,
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
                RootDefinition::Namespace(_, decls) => {
                    for decl in decls {
                        process_definition(decl, params);
                    }
                }
            }
        }

        for decl in &mut self.root_definitions {
            process_definition(decl, &params);
        }

        self
    }
}

impl Default for AssignBindingsParams {
    fn default() -> Self {
        AssignBindingsParams {
            require_slot_type: true,
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

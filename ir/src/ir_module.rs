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

impl Module {
    /// Assign slot numbers to resource bindings
    ///
    /// Slots are assigned within a single namespace.
    /// Only object types will receive a slot.
    pub fn assign_slot_numbers(&mut self) {
        // Search definitions for used slot numbers
        fn search_definition(decl: &RootDefinition, used_values: &mut HashSet<u32>) {
            match decl {
                RootDefinition::Struct(_)
                | RootDefinition::StructTemplate(_)
                | RootDefinition::Function(_) => {}
                RootDefinition::ConstantBuffer(cb) => match cb.slot {
                    Some(GlobalSlot::ReadSlot(slot))
                    | Some(GlobalSlot::ReadWriteSlot(slot))
                    | Some(GlobalSlot::SamplerSlot(slot))
                    | Some(GlobalSlot::ConstantSlot(slot)) => {
                        used_values.insert(slot);
                    }
                    None => {}
                },
                RootDefinition::GlobalVariable(decl) => match decl.slot {
                    Some(GlobalSlot::ReadSlot(slot))
                    | Some(GlobalSlot::ReadWriteSlot(slot))
                    | Some(GlobalSlot::SamplerSlot(slot))
                    | Some(GlobalSlot::ConstantSlot(slot)) => {
                        used_values.insert(slot);
                    }
                    None => {}
                },
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
                    if cb.slot.is_none() {
                        let mut slot = *next_value;
                        while used_values.contains(&slot) {
                            slot += 1;
                        }
                        *next_value = slot + 1;

                        cb.slot = Some(GlobalSlot::ConstantSlot(slot));
                    }
                }
                RootDefinition::GlobalVariable(decl) => {
                    if decl.slot.is_none() {
                        // Find the slot type that we are adding
                        let slot_ty = match decl.global_type.0 .0 {
                            TypeLayout::Object(ObjectType::ConstantBuffer(_)) => {
                                Some(GlobalSlot::ConstantSlot(0))
                            }
                            TypeLayout::Object(ObjectType::ByteAddressBuffer)
                            | TypeLayout::Object(ObjectType::StructuredBuffer(_))
                            | TypeLayout::Object(ObjectType::Buffer(_))
                            | TypeLayout::Object(ObjectType::Texture2D(_)) => {
                                Some(GlobalSlot::ReadSlot(0))
                            }
                            TypeLayout::Object(ObjectType::RWByteAddressBuffer)
                            | TypeLayout::Object(ObjectType::RWStructuredBuffer(_))
                            | TypeLayout::Object(ObjectType::RWBuffer(_))
                            | TypeLayout::Object(ObjectType::RWTexture2D(_)) => {
                                Some(GlobalSlot::ReadWriteSlot(0))
                            }
                            _ => None,
                        };

                        if let Some(slot_ty) = slot_ty {
                            let mut slot = *next_value;
                            while used_values.contains(&slot) {
                                slot += 1;
                            }
                            *next_value = slot + 1;

                            decl.slot = Some(match slot_ty {
                                GlobalSlot::ReadSlot(_) => GlobalSlot::ReadSlot(slot),
                                GlobalSlot::ReadWriteSlot(_) => GlobalSlot::ReadWriteSlot(slot),
                                GlobalSlot::SamplerSlot(_) => GlobalSlot::SamplerSlot(slot),
                                GlobalSlot::ConstantSlot(_) => GlobalSlot::ConstantSlot(slot),
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

use crate::*;
use rssl_text::SourceLocation;
use std::collections::HashMap;

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

    /// Container of all registered local variables in the module
    pub variable_registry: VariableRegistry,

    /// Container of all the namespaces
    pub namespace_registry: NamespaceRegistry,

    /// The shader pipelines that can be built
    pub pipelines: Vec<PipelineDefinition>,

    /// The shader pipeline that is set as the entry point
    pub selected_pipeline: Option<usize>,

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
/// These definitions are used to maintain the source order of root symbols in the module.
/// These should not affect internal module consistency.
/// These also may not be the exported order if a different order is required.
#[derive(PartialEq, Debug, Clone)]
pub enum RootDefinition {
    Struct(StructId),
    StructTemplate(StructTemplateId),
    Enum(EnumId),
    ConstantBuffer(ConstantBufferId),
    GlobalVariable(GlobalId),
    FunctionDeclaration(FunctionId),
    Function(FunctionId),
}

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

    /// Count number of slots for object types based on how many fields we need to fill for a Metal argument buffer
    pub metal_slot_layout: bool,

    /// If static samplers need API slots
    pub static_samplers_have_slots: bool,
}

impl Module {
    /// Create a new module
    pub fn create() -> Self {
        let mut module = Module::default();
        intrinsic_data::add_intrinsics(&mut module);
        module
    }

    /// Get a string name from a type id
    pub fn get_type_name_short(&self, id: TypeId) -> String {
        let tyl = self.type_registry.get_type_layer(id);
        match tyl {
            TypeLayer::Void => "void".to_string(),
            TypeLayer::Scalar(st) => format!("{st:?}"),
            TypeLayer::Vector(ty, x) => format!("{}{}", self.get_type_name_short(ty), x),
            TypeLayer::Matrix(ty, x, y) => {
                format!("{}{}x{}", self.get_type_name_short(ty), x, y)
            }
            TypeLayer::Struct(sid) => self.get_struct_name(sid).to_string(),
            TypeLayer::Enum(id) => self.get_enum_name(id).to_string(),
            TypeLayer::Object(ot) => self.get_object_type_string(ot),
            TypeLayer::Array(ty, Some(len)) => {
                format!("{}[{len}]", self.get_type_name_short(ty))
            }
            TypeLayer::Array(ty, None) => {
                format!("{}[]", self.get_type_name_short(ty))
            }
            TypeLayer::Modifier(modifier, ty) => {
                format!("{:?}{}", modifier, self.get_type_name_short(ty))
            }
            TypeLayer::TemplateParam(id) => {
                let template_type = self.type_registry.get_template_type(id);
                match &template_type.name {
                    Some(name) => name.node.clone(),
                    None => format!("typename<{}>", template_type.positional_index),
                }
            }
            _ => format!("{tyl:?}"),
        }
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

    /// Get the name from an enum id
    pub fn get_enum_name(&self, id: EnumId) -> &str {
        &self.enum_registry.get_enum_definition(id).name.node
    }

    /// Get a string name from an intrinsic object type
    fn get_object_type_string(&self, object_type: ObjectType) -> String {
        use ObjectType::*;
        match object_type {
            Buffer(ty) => format!("Buffer<{}>", self.get_type_name_short(ty)),
            RWBuffer(ty) => format!("RWBuffer<{}>", self.get_type_name_short(ty)),
            ByteAddressBuffer => "ByteAddressBuffer".to_string(),
            RWByteAddressBuffer => "RWByteAddressBuffer".to_string(),
            BufferAddress => "BufferAddress".to_string(),
            RWBufferAddress => "RWBufferAddress".to_string(),
            StructuredBuffer(ty) => {
                format!("StructuredBuffer<{}>", self.get_type_name_short(ty))
            }
            RWStructuredBuffer(ty) => {
                format!("RWStructuredBuffer<{}>", self.get_type_name_short(ty))
            }
            Texture2D(ty) => format!("Texture2D<{}>", self.get_type_name_short(ty)),
            Texture2DMips(ty) => format!("Texture2D<{}>::Mips", self.get_type_name_short(ty)),
            Texture2DMipsSlice(ty) => {
                format!("Texture2D<{}>::MipsSlice", self.get_type_name_short(ty))
            }
            Texture2DArray(ty) => format!("Texture2DArray<{}>", self.get_type_name_short(ty)),
            Texture2DArrayMips(ty) => {
                format!("Texture2DArray<{}>::Mips", self.get_type_name_short(ty))
            }
            Texture2DArrayMipsSlice(ty) => {
                format!(
                    "Texture2DArray<{}>::MipsSlice",
                    self.get_type_name_short(ty)
                )
            }
            RWTexture2D(ty) => format!("RWTexture2D<{}>", self.get_type_name_short(ty)),
            RWTexture2DArray(ty) => format!("RWTexture2DArray<{}>", self.get_type_name_short(ty)),
            TextureCube(ty) => format!("TextureCube<{}>", self.get_type_name_short(ty)),
            TextureCubeArray(ty) => format!("TextureCubeArray<{}>", self.get_type_name_short(ty)),
            Texture3D(ty) => format!("Texture3D<{}>", self.get_type_name_short(ty)),
            Texture3DMips(ty) => format!("Texture3D<{}>::Mips", self.get_type_name_short(ty)),
            Texture3DMipsSlice(ty) => {
                format!("Texture3D<{}>::MipsSlice", self.get_type_name_short(ty))
            }
            RWTexture3D(ty) => format!("RWTexture3D<{}>", self.get_type_name_short(ty)),
            ConstantBuffer(ty) => format!("ConstantBuffer<{}>", self.get_type_name_short(ty)),
            SamplerState => "SamplerState".to_string(),
            SamplerComparisonState => "SamplerComparisonState".to_string(),
            TriangleStream(ty) => {
                format!("TriangleStream<{}>", self.get_type_name_short(ty))
            }
            RaytracingAccelerationStructure => "RaytracingAccelerationStructure".to_string(),
            RayQuery(v) => format!("RayQuery<{v}>"),
            RayDesc => "RayDesc".to_string(),
        }
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

    /// Select which pipeline we will build for
    pub fn select_pipeline(self, name: &str) -> Option<Self> {
        let mut selected = None;
        for (i, pipeline) in self.pipelines.iter().enumerate() {
            if pipeline.name.node == name {
                assert_eq!(selected, None);
                selected = Some(i);
            }
        }
        selected?;
        let mut output = self.clone();
        output.selected_pipeline = selected;
        Some(output)
    }

    /// Assign api binding locations to all global resources
    ///
    /// Api slots are assigned independent of language register assignments
    pub fn assign_api_bindings(mut self, params: &AssignBindingsParams) -> Self {
        assert!(!self.flags.assigned_api_slots);
        self.flags.assigned_api_slots = true;
        self.flags.requires_buffer_address = params.support_buffer_address;
        self.flags.requires_vk_binding = !params.require_slot_type || params.support_buffer_address;

        let default_set = match self.selected_pipeline {
            Some(index) => self.pipelines[index].default_bind_group_index,
            None => 0,
        };

        fn process_definition(
            module: &mut Module,
            decl: &RootDefinition,
            params: &AssignBindingsParams,
            used_slots: &mut HashMap<u32, u32>,
            inline_size: &mut HashMap<u32, u32>,
            default_set: u32,
        ) {
            match decl {
                RootDefinition::Struct(_)
                | RootDefinition::StructTemplate(_)
                | RootDefinition::Enum(_)
                | RootDefinition::FunctionDeclaration(_)
                | RootDefinition::Function(_) => {}
                RootDefinition::ConstantBuffer(id) => {
                    let cb = &mut module.cbuffer_registry[id.0 as usize];
                    let set = cb.lang_binding.set.unwrap_or(default_set);
                    assert_eq!(cb.api_binding, None);
                    {
                        let index = match used_slots.entry(set) {
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
                            set,
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
                    let set = decl.lang_slot.set.unwrap_or(default_set);

                    // If static samplers are implemented purely in shader source then do not give them slots
                    if decl.static_sampler.is_some() && !params.static_samplers_have_slots {
                        return;
                    }

                    // Remove outer layer of modifier
                    let unmodified_ty_id = module.type_registry.remove_modifier(decl.type_id);

                    // Attempt to remove array type - and extract the length
                    let (unmodified_ty_id, array_len) =
                        match module.type_registry.get_type_layer(unmodified_ty_id) {
                            // Arrays with known length
                            // Unsized arrays are ignored currently
                            TypeLayer::Array(inner, Some(len)) => {
                                // Remove inner layer of modifier
                                let unmodified = module.type_registry.remove_modifier(inner);

                                (unmodified, Some(len))
                            }
                            _ => (unmodified_ty_id, None),
                        };

                    // Get info for type layer after extracting outer shells
                    let unmodified_tyl = module.type_registry.get_type_layer(unmodified_ty_id);

                    let array_count = array_len.unwrap_or(1) as u32;
                    let slice_cost = match unmodified_tyl {
                        TypeLayer::Object(
                            ObjectType::ByteAddressBuffer
                            | ObjectType::RWByteAddressBuffer
                            | ObjectType::BufferAddress
                            | ObjectType::RWBufferAddress
                            | ObjectType::StructuredBuffer(_)
                            | ObjectType::RWStructuredBuffer(_),
                        ) if params.metal_slot_layout => 2,
                        _ => 1,
                    };
                    let slot_count = array_count * slice_cost;

                    assert_eq!(decl.api_slot, None);
                    if unmodified_tyl.is_object() {
                        if params.support_buffer_address
                            && module.type_registry.is_buffer_address(decl.type_id)
                        {
                            let offset = match inline_size.entry(set) {
                                std::collections::hash_map::Entry::Occupied(mut o) => {
                                    let slot = *o.get();
                                    *o.get_mut() += 8 * slot_count;
                                    slot
                                }
                                std::collections::hash_map::Entry::Vacant(v) => {
                                    v.insert(8 * slot_count);
                                    0
                                }
                            };

                            decl.api_slot = Some(ApiBinding {
                                set,
                                location: ApiLocation::InlineConstant(offset),
                                slot_type: None,
                            });

                            assert_eq!(decl.storage_class, GlobalStorage::Extern);
                            decl.storage_class = GlobalStorage::Static;
                        } else {
                            let index = match used_slots.entry(set) {
                                std::collections::hash_map::Entry::Occupied(mut o) => {
                                    let slot = *o.get();
                                    *o.get_mut() += slot_count;
                                    slot
                                }
                                std::collections::hash_map::Entry::Vacant(v) => {
                                    v.insert(slot_count);
                                    0
                                }
                            };

                            decl.api_slot = Some(ApiBinding {
                                set,
                                location: ApiLocation::Index(index),
                                slot_type: if params.require_slot_type {
                                    Some(if let TypeLayer::Object(ot) = unmodified_tyl {
                                        ot.get_register_type()
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
            }
        }

        let mut used_slots = HashMap::new();
        let mut inline_size = HashMap::new();
        for decl in &self.root_definitions.clone() {
            process_definition(
                &mut self,
                decl,
                params,
                &mut used_slots,
                &mut inline_size,
                default_set,
            );
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
            metal_slot_layout: false,
            static_samplers_have_slots: true,
        }
    }
}

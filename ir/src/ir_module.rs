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

    /// The shader pipelines that can be built
    pub pipelines: Vec<PipelineDefinition>,

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
    FunctionDeclaration(FunctionId),
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
                self.type_registry.get_template_type(id).name.node.clone()
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

    /// Assign api binding locations to all global resources
    ///
    /// Api slots are assigned independent of language register assignments
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
                | RootDefinition::FunctionDeclaration(_)
                | RootDefinition::Function(_) => {}
                RootDefinition::ConstantBuffer(id) => {
                    let cb = &mut module.cbuffer_registry[id.0 as usize];
                    assert_eq!(cb.api_binding, None);
                    {
                        let index = match used_slots.entry(cb.lang_binding.set) {
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
                            set: cb.lang_binding.set,
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
                    let unmodified_ty_id = module.type_registry.remove_modifier(decl.type_id);
                    let unmodified_tyl = module.type_registry.get_type_layer(unmodified_ty_id);
                    assert_eq!(decl.api_slot, None);
                    if unmodified_tyl.is_object() {
                        if params.support_buffer_address
                            && module.type_registry.is_buffer_address(decl.type_id)
                        {
                            let offset = match inline_size.entry(decl.lang_slot.set) {
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
                                set: decl.lang_slot.set,
                                location: ApiLocation::InlineConstant(offset),
                                slot_type: None,
                            });

                            assert_eq!(decl.storage_class, GlobalStorage::Extern);
                            decl.storage_class = GlobalStorage::Static;
                        } else {
                            let index = match used_slots.entry(decl.lang_slot.set) {
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
                                set: decl.lang_slot.set,
                                location: ApiLocation::Index(index),
                                slot_type: if params.require_slot_type {
                                    let unmodified_id =
                                        module.type_registry.remove_modifier(decl.type_id);
                                    let tyl = module.type_registry.get_type_layer(unmodified_id);
                                    Some(if let TypeLayer::Object(ot) = tyl {
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
            write!(f, "{scope}::")?;
        }
        write!(f, "{last}")
    }
}

impl std::fmt::Debug for ScopedName {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{self}")
    }
}

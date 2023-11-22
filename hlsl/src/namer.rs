use rssl_ir as ir;
use std::collections::{HashMap, HashSet};

/// Map of generated names for each symbol
#[derive(Debug)]
pub struct NameMap {
    names: HashMap<NameSymbol, NameString>,
}

/// Any kind of named symbol
#[derive(Debug, Eq, PartialEq, Hash, Copy, Clone)]
pub enum NameSymbol {
    Struct(ir::StructId),
    Enum(ir::EnumId),
    GlobalVariable(ir::GlobalId),
    Function(ir::FunctionId),
    Namespace(ir::NamespaceId),
}

/// A name which may have namespace qualification
pub struct ScopedName(pub Vec<String>);

#[derive(Debug)]
struct NameString {
    namespace: Option<ir::NamespaceId>,
    name: String,
}

impl NameMap {
    /// Construct a name map from the symbols within a module
    pub fn build(module: &ir::Module) -> NameMap {
        let mut name_map = NameMap {
            names: HashMap::new(),
        };

        let mut scopes: HashMap<Option<ir::NamespaceId>, HashMap<String, Vec<NameSymbol>>> =
            HashMap::new();

        scopes.insert(None, HashMap::new());

        for i in 0..module.namespace_registry.get_namespace_count() {
            let id = ir::NamespaceId(i);

            let parent = module.namespace_registry.get_namespace_parent(id);
            let name = module.namespace_registry.get_namespace_name(id).to_string();

            // Insert the name into the parent scope
            let name_vec = scopes.get_mut(&parent).unwrap().entry(name).or_default();
            name_vec.push(NameSymbol::Namespace(id));

            // Add namespace as a scope
            scopes.insert(Some(id), HashMap::new());
        }

        for i in 0..module.struct_registry.len() {
            let id = ir::StructId(i as u32);
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
            let id = ir::EnumId(i);
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
            let id = ir::GlobalId(i as u32);
            let def = &module.global_registry[id.0 as usize];

            // Do not assign names to intrinsics
            if def.is_intrinsic {
                // All intrinsic values should have names that are reserved
                assert!(
                    RESERVED_NAMES.contains(&def.name.node.as_str()),
                    "{}",
                    def.name.node
                );
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

        for i in 0..module.function_registry.get_function_count() {
            let id = ir::FunctionId(i);

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
        let mut reserved_names = HashSet::new();
        for name in RESERVED_NAMES {
            reserved_names.insert(name.to_string());
        }

        for scope in &scopes {
            // Record used names within the current scope
            // Names may be reused in different namespaces
            let mut used_names = reserved_names.clone();

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

impl std::fmt::Display for ScopedName {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let (last, scopes) = self.0.split_last().unwrap();
        // Do not write the starting ::  as it makes the output too ugly without ability to elide it
        for scope in scopes {
            write!(f, "{scope}::")?;
        }
        write!(f, "{last}")
    }
}

/// List of names we want to avoid in the generated code
const RESERVED_NAMES: &[&str] = &[
    "abs",
    "acos",
    "all",
    "AllMemoryBarrier",
    "AllMemoryBarrierWithGroupSync",
    "and",
    "any",
    "asdouble",
    "asfloat",
    "asin",
    "asint",
    "asuint",
    "atan",
    "atan2",
    "auto",
    "break",
    "Buffer",
    "ByteAddressBuffer",
    "CANDIDATE_NON_OPAQUE_TRIANGLE",
    "CANDIDATE_PROCEDURAL_PRIMITIVE",
    "case",
    "catch",
    "cbuffer",
    "ceil",
    "char",
    "clamp",
    "class",
    "column_major",
    "COMMITTED_NOTHING",
    "COMMITTED_PROCEDURAL_PRIMITIVE_HIT",
    "COMMITTED_TRIANGLE_HIT",
    "const",
    "const_cast",
    "ConstantBuffer",
    "constexpr",
    "continue",
    "cos",
    "cosh",
    "countbits",
    "cross",
    "ddx",
    "ddx_coarse",
    "ddx_fine",
    "ddy",
    "ddy_coarse",
    "ddy_fine",
    "decltype",
    "default",
    "delete",
    "DeviceMemoryBarrier",
    "DeviceMemoryBarrierWithGroupSync",
    "discard",
    "DispatchMesh",
    "distance",
    "do",
    "dot",
    "double",
    "dynamic_cast",
    "else",
    "enum",
    "exp",
    "exp2",
    "explicit",
    "extern",
    "f16tof32",
    "f32tof16",
    "false",
    "firstbithigh",
    "firstbitlow",
    "float",
    "float16_t",
    "floor",
    "fmod",
    "for",
    "frac",
    "friend",
    "goto",
    "GroupMemoryBarrier",
    "GroupMemoryBarrierWithGroupSync",
    "groupshared",
    "half",
    "if",
    "in",
    "inline",
    "inout",
    "int",
    "int64_t",
    "InterlockedAdd",
    "InterlockedAnd",
    "InterlockedCompareExchange",
    "InterlockedCompareStore",
    "InterlockedExchange",
    "InterlockedMax",
    "InterlockedMin",
    "InterlockedOr",
    "InterlockedXor",
    "isfinite",
    "isinf",
    "isnan",
    "length",
    "lerp",
    "log",
    "log10",
    "log2",
    "long",
    "matrix",
    "max",
    "min",
    "modf",
    "mul",
    "mutable",
    "namespace",
    "new",
    "NonUniformResourceIndex",
    "normalize",
    "operator",
    "or",
    "out",
    "packoffset",
    "pow",
    "private",
    "protected",
    "public",
    "QuadReadAcrossDiagonal",
    "QuadReadAcrossX",
    "QuadReadAcrossY",
    "QuadReadLaneAt",
    "RAY_FLAG_ACCEPT_FIRST_HIT_AND_END_SEARCH",
    "RAY_FLAG_CULL_BACK_FACING_TRIANGLES",
    "RAY_FLAG_CULL_FRONT_FACING_TRIANGLES",
    "RAY_FLAG_CULL_NON_OPAQUE",
    "RAY_FLAG_CULL_OPAQUE",
    "RAY_FLAG_FORCE_NON_OPAQUE",
    "RAY_FLAG_FORCE_OPAQUE",
    "RAY_FLAG_NONE",
    "RAY_FLAG_SKIP_CLOSEST_HIT_SHADER",
    "RAY_FLAG_SKIP_PROCEDURAL_PRIMITIVES",
    "RAY_FLAG_SKIP_TRIANGLES",
    "RayDesc",
    "RayQuery",
    "RaytracingAccelerationStructure",
    "rcp",
    "reflect",
    "refract",
    "register",
    "reinterpret_cast",
    "return",
    "reversebits",
    "round",
    "row_major",
    "rsqrt",
    "RWBuffer",
    "RWByteAddressBuffer",
    "RWStructuredBuffer",
    "RWTexture2D",
    "RWTexture2DArray",
    "RWTexture3D",
    "SamplerComparisonState",
    "SamplerState,",
    "saturate",
    "select",
    "SetMeshOutputCounts",
    "short",
    "sign",
    "signed",
    "sin",
    "sincos",
    "sinh",
    "sizeof",
    "smoothstep",
    "snorm",
    "sqrt",
    "static",
    "static_cast",
    "step",
    "struct",
    "StructuredBuffer",
    "switch",
    "tan",
    "tanh",
    "template",
    "Texture2D",
    "Texture2DArray",
    "Texture3D",
    "TextureCube",
    "TextureCubeArray",
    "this",
    "throw",
    "transpose",
    "TriangleStream",
    "true",
    "trunc",
    "try",
    "typedef",
    "typename",
    "uint",
    "uint64_t",
    "union",
    "unorm",
    "unsigned",
    "using",
    "vector",
    "virtual",
    "void",
    "volatile",
    "WaveActiveAllEqual",
    "WaveActiveAllTrue",
    "WaveActiveAnyTrue",
    "WaveActiveBallot",
    "WaveActiveBitAnd",
    "WaveActiveBitOr",
    "WaveActiveBitXor",
    "WaveActiveCountBits",
    "WaveActiveMax",
    "WaveActiveMin",
    "WaveActiveProduct",
    "WaveActiveSum",
    "WaveGetLaneCount",
    "WaveGetLaneIndex",
    "WaveIsFirstLane",
    "WavePrefixCountBits",
    "WavePrefixProduct",
    "WavePrefixSum",
    "WaveReadLaneAt",
    "WaveReadLaneFirst",
    "while",
];

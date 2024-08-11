use std::collections::{HashMap, HashSet};

use rssl_ast as ast;
use rssl_ir as ir;
use rssl_text::{Located, SourceLocation};

use super::{metal_lib_identifier, metal_raytracing_identifier, GenerateError};
use crate::names::HELPER_NAMESPACE_NAME;

/// Represents a helper function that is generated to implement intrinsic operations
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum IntrinsicHelper {
    GetDimensions(GetDimensionsHelper),
    Load(LoadHelper),
    Store(Dim),
    Sample(SampleHelper),
    AddressLoad,
    AddressStore,
    AddressAtomic(BufferAtomicOp),
    AddressGetDimensions,
    StructuredBufferGetDimensions,
    StructuredBufferLoad,
    StructuredBufferStore,
    /// Turn a scalar or non-4 component vector into a 4 component vector - not caring about higher channels
    Extend(u32),
    /// Transform a uint coordinate to an int coordinate
    MakeSigned,
    /// Transform a uint coordinate to an int coordinate and add a zero on the end
    MakeSignedPushZero,
    /// Call set_index for each vertex in a primitive
    MeshOutputSetIndices(ir::OutputTopology),
    /// Setup intersection_params for a raytracing call
    IntersectionParams,
}

/// Represents a helper struct that is generated to implement intrinsic operations
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(clippy::enum_variant_names)]
pub enum IntrinsicObject {
    ByteAddressBuffer,
    RWByteAddressBuffer,
    StructuredBuffer,
    RWStructuredBuffer,
}

/// Generate the function name required to call the helper
pub fn get_intrinsic_helper_name(intrinsic: IntrinsicHelper) -> &'static str {
    match intrinsic {
        IntrinsicHelper::GetDimensions(_) => "GetDimensions",
        IntrinsicHelper::Load(_) => "Load",
        IntrinsicHelper::Store(_) => "Store",
        IntrinsicHelper::Sample(config) => {
            if config.has_lod {
                "SampleLevel"
            } else {
                "Sample"
            }
        }
        IntrinsicHelper::AddressLoad => "Load",
        IntrinsicHelper::AddressStore => "Store",
        IntrinsicHelper::AddressAtomic(op) => op.get_helper_name(),
        IntrinsicHelper::AddressGetDimensions => "GetDimensions",
        IntrinsicHelper::StructuredBufferLoad => "Load",
        IntrinsicHelper::StructuredBufferStore => "Store",
        IntrinsicHelper::StructuredBufferGetDimensions => "GetDimensions",
        IntrinsicHelper::Extend(_) => "extend",
        IntrinsicHelper::MakeSigned => "make_signed",
        IntrinsicHelper::MakeSignedPushZero => "make_signed_push_0",
        IntrinsicHelper::MeshOutputSetIndices(_) => "set_indices",
        IntrinsicHelper::IntersectionParams => "intersection_params",
    }
}

impl IntrinsicObject {
    /// Get the generated name of the object
    pub fn get_name(&self) -> &'static str {
        match self {
            IntrinsicObject::ByteAddressBuffer => "ByteAddressBuffer",
            IntrinsicObject::RWByteAddressBuffer => "RWByteAddressBuffer",
            IntrinsicObject::StructuredBuffer => "StructuredBuffer",
            IntrinsicObject::RWStructuredBuffer => "RWStructuredBuffer",
        }
    }

    /// Get the generated name of the object inside the helper namespace
    pub fn get_scoped_name(&self) -> ast::ScopedIdentifier {
        ast::ScopedIdentifier {
            base: ast::ScopedIdentifierBase::Relative,
            identifiers: Vec::from([
                Located::none(String::from(HELPER_NAMESPACE_NAME)),
                Located::none(String::from(self.get_name())),
            ]),
        }
    }
}

/// Generate the definitions for all helpers
pub fn generate_helpers(
    required_helpers: HashMap<Option<IntrinsicObject>, HashSet<IntrinsicHelper>>,
) -> Result<Option<ast::RootDefinition>, GenerateError> {
    let mut definitions = Vec::new();

    let mut objects = Vec::from_iter(required_helpers);
    objects.sort_by(|(key_lhs, _), (key_rhs, _)| std::cmp::Ord::cmp(key_lhs, key_rhs));

    for (object, helpers) in objects {
        let mut ordered = Vec::from_iter(helpers);
        ordered.sort();

        let mut functions = Vec::new();
        for helper in ordered {
            functions.push(generate_helper(helper)?);
        }

        if let Some(object) = object {
            let template_args = if matches!(
                object,
                IntrinsicObject::StructuredBuffer | IntrinsicObject::RWStructuredBuffer
            ) {
                Vec::from([ast::TemplateParam::Type(ast::TemplateTypeParam {
                    name: Some(Located::none(String::from("T"))),
                    default: None,
                })])
            } else {
                Vec::new()
            };

            let member_vars = generate_members(object);
            let mut members = member_vars
                .into_iter()
                .map(ast::StructEntry::Variable)
                .collect::<Vec<_>>();
            members.extend(functions.into_iter().map(ast::StructEntry::Method));

            definitions.push(ast::RootDefinition::Struct(ast::StructDefinition {
                name: Located::none(String::from(object.get_name())),
                base_types: Vec::new(),
                template_params: ast::TemplateParamList(template_args),
                members,
            }));
        } else {
            definitions.extend(functions.into_iter().map(ast::RootDefinition::Function));
        }
    }

    if definitions.is_empty() {
        Ok(None)
    } else {
        Ok(Some(ast::RootDefinition::Namespace(
            Located::none(String::from(HELPER_NAMESPACE_NAME)),
            definitions,
        )))
    }
}

/// Generate the definition for a single helper
fn generate_helper(helper: IntrinsicHelper) -> Result<ast::FunctionDefinition, GenerateError> {
    use IntrinsicHelper::*;
    match helper {
        GetDimensions(config) => Ok(build_get_dimensions(config)?),
        Load(config) => Ok(build_texture_load(config)?),
        Store(dim) => Ok(build_texture_store(dim)?),
        Sample(config) => Ok(build_texture_sample(config)?),
        AddressLoad => Ok(build_address_load()?),
        AddressStore => Ok(build_address_store()?),
        AddressAtomic(op) => Ok(build_address_atomic(op)?),
        AddressGetDimensions => Ok(build_address_get_dimensions()?),
        StructuredBufferLoad => Ok(build_structured_buffer_load()?),
        StructuredBufferStore => Ok(build_structured_buffer_store()?),
        StructuredBufferGetDimensions => Ok(build_structured_buffer_get_dimensions()?),
        Extend(count) => Ok(build_vector_extend(count)?),
        MakeSigned => Ok(build_unsign(false)?),
        MakeSignedPushZero => Ok(build_unsign(true)?),
        MeshOutputSetIndices(topology) => Ok(build_mesh_output_set_indices(topology)?),
        IntersectionParams => Ok(build_intersection_params()?),
    }
}

/// Generate the member variables for an intrinsic object
fn generate_members(object: IntrinsicObject) -> Vec<ast::StructMember> {
    use IntrinsicObject::*;
    match object {
        ByteAddressBuffer | RWByteAddressBuffer => {
            let mut ty = ast::Type::from("uint8_t");
            if !matches!(object, RWByteAddressBuffer) {
                ty.modifiers
                    .prepend(Located::none(ast::TypeModifier::Const));
            }
            ty.modifiers
                .prepend(Located::none(ast::TypeModifier::AddressSpace(
                    ast::AddressSpace::Device,
                )));

            Vec::from([
                ast::StructMember {
                    ty,
                    defs: Vec::from([ast::InitDeclarator {
                        declarator: ast::Declarator::Pointer(ast::PointerDeclarator {
                            attributes: Vec::new(),
                            qualifiers: ast::TypeModifierSet::default(),
                            inner: Box::new(ast::Declarator::Identifier(
                                ast::ScopedIdentifier::trivial("address"),
                                Vec::new(),
                            )),
                        }),
                        location_annotations: Vec::new(),
                        init: None,
                    }]),
                    attributes: Vec::new(),
                },
                ast::StructMember {
                    ty: ast::Type::from("uint64_t"),
                    defs: Vec::from([ast::InitDeclarator {
                        declarator: ast::Declarator::Identifier(
                            ast::ScopedIdentifier::trivial("size"),
                            Vec::new(),
                        ),
                        location_annotations: Vec::new(),
                        init: None,
                    }]),
                    attributes: Vec::new(),
                },
            ])
        }
        StructuredBuffer | RWStructuredBuffer => {
            let mut ty = ast::Type::from("T");
            if !matches!(object, RWStructuredBuffer) {
                ty.modifiers
                    .prepend(Located::none(ast::TypeModifier::Const));
            }
            ty.modifiers
                .prepend(Located::none(ast::TypeModifier::AddressSpace(
                    ast::AddressSpace::Device,
                )));

            Vec::from([
                ast::StructMember {
                    ty,
                    defs: Vec::from([ast::InitDeclarator {
                        declarator: ast::Declarator::Pointer(ast::PointerDeclarator {
                            attributes: Vec::new(),
                            qualifiers: ast::TypeModifierSet::default(),
                            inner: Box::new(ast::Declarator::Identifier(
                                ast::ScopedIdentifier::trivial("address"),
                                Vec::new(),
                            )),
                        }),
                        location_annotations: Vec::new(),
                        init: None,
                    }]),
                    attributes: Vec::new(),
                },
                ast::StructMember {
                    ty: ast::Type::from("uint64_t"),
                    defs: Vec::from([ast::InitDeclarator {
                        declarator: ast::Declarator::Identifier(
                            ast::ScopedIdentifier::trivial("size"),
                            Vec::new(),
                        ),
                        location_annotations: Vec::new(),
                        init: None,
                    }]),
                    attributes: Vec::new(),
                },
            ])
        }
    }
}

/// Dimension of a texture object type
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Dim {
    Tex2D,
    Tex2DArray,
    TexCube,
    TexCubeArray,
    Tex3D,
    TexelBuffer,
}

impl Dim {
    fn get_type_name(&self) -> &'static str {
        match self {
            Dim::Tex2D => "texture2d",
            Dim::Tex2DArray => "texture2d_array",
            Dim::TexCube => "texturecube",
            Dim::TexCubeArray => "texturecube_array",
            Dim::Tex3D => "texture3d",
            Dim::TexelBuffer => "texture_buffer",
        }
    }
}

/// Create a type for a vec
fn build_vec(ty: &str, n: u64) -> ast::Type {
    ast::Type::from_layout(ast::TypeLayout(
        metal_lib_identifier("vec"),
        Vec::from([
            ast::ExpressionOrType::Type(ast::TypeId::from(ty)),
            ast::ExpressionOrType::Expression(Located::none(ast::Expression::Literal(
                ast::Literal::IntUntyped(n),
            ))),
        ])
        .into_boxed_slice(),
    ))
}

/// Create a type for a texture object
fn build_texture(name: &'static str, component: &'static str, read_write: bool) -> ast::Type {
    let mut type_args = Vec::from([ast::ExpressionOrType::Type(ast::TypeId::from(component))]);

    if read_write {
        let access = ast::Expression::Identifier(ast::ScopedIdentifier {
            base: ast::ScopedIdentifierBase::Relative,
            identifiers: Vec::from([
                Located::none(String::from("metal")),
                Located::none(String::from("access")),
                Located::none(String::from("read_write")),
            ]),
        });
        type_args.push(ast::ExpressionOrType::Expression(Located::none(access)));
    }

    ast::Type::from_layout(ast::TypeLayout(
        ast::ScopedIdentifier {
            base: ast::ScopedIdentifierBase::Relative,
            identifiers: Vec::from([
                Located::none(String::from("metal")),
                Located::none(String::from(name)),
            ]),
        },
        type_args.into_boxed_slice(),
    ))
}

/// Create a type for a sampler object
fn build_sampler() -> ast::Type {
    ast::Type::from(metal_lib_identifier("sampler"))
}

/// Build a trivial function parameter
fn build_param(param_type: ast::Type, name: &str) -> ast::FunctionParam {
    ast::FunctionParam {
        param_type,
        declarator: ast::Declarator::Identifier(ast::ScopedIdentifier::trivial(name), Vec::new()),
        location_annotations: Vec::new(),
        default_expr: None,
    }
}

/// Build a function out parameter
fn build_out_param(mut param_type: ast::Type, name: &str) -> ast::FunctionParam {
    param_type
        .modifiers
        .prepend(Located::none(ast::TypeModifier::AddressSpace(
            ast::AddressSpace::Thread,
        )));
    ast::FunctionParam {
        param_type,
        declarator: ast::Declarator::Reference(ast::ReferenceDeclarator {
            attributes: Vec::new(),
            inner: Box::new(ast::Declarator::Identifier(
                ast::ScopedIdentifier::trivial(name),
                Vec::new(),
            )),
        }),
        location_annotations: Vec::new(),
        default_expr: None,
    }
}

/// Build a simple object.member expr node
fn build_expr_member(object: &str, member: &str) -> Located<ast::Expression> {
    Located::none(ast::Expression::Member(
        Box::new(Located::none(ast::Expression::Identifier(
            ast::ScopedIdentifier::trivial(object),
        ))),
        ast::ScopedIdentifier::trivial(member),
    ))
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct GetDimensionsHelper {
    pub dim: Dim,
    pub read_write: bool,
    pub has_mip_args: bool,
}

/// Create a definition for various dimension fetching methods
fn build_get_dimensions(
    config: GetDimensionsHelper,
) -> Result<ast::FunctionDefinition, GenerateError> {
    let has_mips = config.has_mip_args;

    let has_width = matches!(config.dim, Dim::Tex2D | Dim::Tex2DArray | Dim::Tex3D);
    let has_height = matches!(config.dim, Dim::Tex2D | Dim::Tex2DArray | Dim::Tex3D);
    let has_elements = matches!(config.dim, Dim::Tex2DArray);
    let has_depth = matches!(config.dim, Dim::Tex3D);

    let mut params = Vec::new();
    let mut body = Vec::new();

    params.push(build_param(
        build_texture(config.dim.get_type_name(), "T", config.read_write),
        "texture",
    ));

    let mip_args = if has_mips {
        params.push(build_param(ast::Type::trivial("uint"), "mipLevel"));

        Vec::from([Located::none(ast::Expression::Identifier(
            ast::ScopedIdentifier::trivial("mipLevel"),
        ))])
    } else {
        Vec::new()
    };

    if has_width {
        params.push(build_out_param(ast::Type::trivial("uint"), "width"));

        body.push(ast::Statement {
            kind: ast::StatementKind::Expression(ast::Expression::BinaryOperation(
                ast::BinOp::Assignment,
                Box::new(Located::none(ast::Expression::Identifier(
                    ast::ScopedIdentifier::trivial("width"),
                ))),
                Box::new(Located::none(ast::Expression::Call(
                    Box::new(build_expr_member("texture", "get_width")),
                    Vec::new(),
                    mip_args.clone(),
                ))),
            )),
            location: SourceLocation::UNKNOWN,
            attributes: Vec::new(),
        });
    }

    if has_height {
        params.push(build_out_param(ast::Type::trivial("uint"), "height"));

        body.push(ast::Statement {
            kind: ast::StatementKind::Expression(ast::Expression::BinaryOperation(
                ast::BinOp::Assignment,
                Box::new(Located::none(ast::Expression::Identifier(
                    ast::ScopedIdentifier::trivial("height"),
                ))),
                Box::new(Located::none(ast::Expression::Call(
                    Box::new(build_expr_member("texture", "get_height")),
                    Vec::new(),
                    mip_args.clone(),
                ))),
            )),
            location: SourceLocation::UNKNOWN,
            attributes: Vec::new(),
        });
    }

    if has_elements {
        params.push(build_out_param(ast::Type::trivial("uint"), "elements"));

        body.push(ast::Statement {
            kind: ast::StatementKind::Expression(ast::Expression::BinaryOperation(
                ast::BinOp::Assignment,
                Box::new(Located::none(ast::Expression::Identifier(
                    ast::ScopedIdentifier::trivial("elements"),
                ))),
                Box::new(Located::none(ast::Expression::Call(
                    Box::new(build_expr_member("texture", "get_array_size")),
                    Vec::new(),
                    Vec::new(),
                ))),
            )),
            location: SourceLocation::UNKNOWN,
            attributes: Vec::new(),
        });
    }

    if has_depth {
        params.push(build_out_param(ast::Type::trivial("uint"), "depth"));

        body.push(ast::Statement {
            kind: ast::StatementKind::Expression(ast::Expression::BinaryOperation(
                ast::BinOp::Assignment,
                Box::new(Located::none(ast::Expression::Identifier(
                    ast::ScopedIdentifier::trivial("depth"),
                ))),
                Box::new(Located::none(ast::Expression::Call(
                    Box::new(build_expr_member("texture", "get_depth")),
                    Vec::new(),
                    mip_args,
                ))),
            )),
            location: SourceLocation::UNKNOWN,
            attributes: Vec::new(),
        });
    }

    if has_mips {
        params.push(build_out_param(
            ast::Type::trivial("uint"),
            "numberOfLevels",
        ));

        body.push(ast::Statement {
            kind: ast::StatementKind::Expression(ast::Expression::BinaryOperation(
                ast::BinOp::Assignment,
                Box::new(Located::none(ast::Expression::Identifier(
                    ast::ScopedIdentifier::trivial("numberOfLevels"),
                ))),
                Box::new(Located::none(ast::Expression::Call(
                    Box::new(build_expr_member("texture", "get_num_mip_levels")),
                    Vec::new(),
                    Vec::new(),
                ))),
            )),
            location: SourceLocation::UNKNOWN,
            attributes: Vec::new(),
        });
    }

    Ok(ast::FunctionDefinition {
        name: Located::none(String::from("GetDimensions")),
        returntype: ast::FunctionReturn {
            return_type: ast::Type::trivial("void"),
            location_annotations: Vec::new(),
        },
        template_params: ast::TemplateParamList(Vec::from([ast::TemplateParam::Type(
            ast::TemplateTypeParam {
                name: Some(Located::none(String::from("T"))),
                default: None,
            },
        )])),
        params,
        is_const: false,
        is_volatile: false,
        body: Some(body),
        attributes: Vec::new(),
    })
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct LoadHelper {
    pub dim: Dim,
    pub read_write: bool,
    pub has_offset: bool,
    pub has_status: bool,
}

/// Create a definition for various texture load methods
fn build_texture_load(config: LoadHelper) -> Result<ast::FunctionDefinition, GenerateError> {
    let num_dims_base = match config.dim {
        Dim::TexelBuffer => 1,
        Dim::Tex2D | Dim::Tex2DArray => 2,
        Dim::Tex3D => 3,
        Dim::TexCube | Dim::TexCubeArray => panic!("Cubes are not supported in Load"),
    };

    let has_mips = !config.read_write && config.dim != Dim::TexelBuffer;
    let has_array_slices = matches!(config.dim, Dim::Tex2DArray);

    let num_dims = num_dims_base + has_mips as usize + has_array_slices as usize;

    let location_type = match num_dims {
        1 => "int",
        2 => "int2",
        3 => "int3",
        4 => "int4",
        _ => panic!(),
    };

    let offset_type = match num_dims_base {
        1 => {
            assert!(!config.has_offset);
            "void"
        }
        2 => "int2",
        3 => "int3",
        _ => panic!(),
    };

    let mut params = Vec::new();
    params.push(build_param(
        build_texture(config.dim.get_type_name(), "T", config.read_write),
        "texture",
    ));
    params.push(build_param(ast::Type::trivial(location_type), "location"));
    if config.has_offset {
        params.push(build_param(ast::Type::trivial(offset_type), "offset"));
    }
    if config.has_status {
        params.push(build_out_param(ast::Type::trivial("uint"), "status"));
    }

    let mut coord_args = Vec::new();
    let make_coord_arg = |c| {
        if config.has_offset {
            Located::none(ast::Expression::BinaryOperation(
                ast::BinOp::Add,
                Box::new(build_expr_member("location", c)),
                Box::new(build_expr_member("offset", c)),
            ))
        } else {
            build_expr_member("location", c)
        }
    };
    if num_dims_base == 1 {
        coord_args.push(Located::none(ast::Expression::Identifier(
            ast::ScopedIdentifier::trivial("location"),
        )));
    } else {
        coord_args.push(make_coord_arg("x"));
        coord_args.push(make_coord_arg("y"));
        if num_dims_base >= 3 {
            coord_args.push(make_coord_arg("z"));
        }
    }

    let coord_type = match num_dims_base {
        1 => "uint",
        2 => "uint2",
        3 => "uint3",
        _ => panic!(),
    };

    let mut load_args = Vec::from([Located::none(ast::Expression::Call(
        Box::new(Located::none(ast::Expression::Identifier(
            ast::ScopedIdentifier::trivial(coord_type),
        ))),
        Vec::new(),
        coord_args,
    ))]);
    if num_dims_base == 2 && num_dims_base < num_dims {
        load_args.push(build_expr_member("location", "z"));
    }
    if (num_dims_base == 3 && num_dims_base < num_dims) || (num_dims_base + 1 < num_dims) {
        load_args.push(build_expr_member("location", "w"));
    }

    let load_expr = Located::none(ast::Expression::Call(
        Box::new(build_expr_member(
            "texture",
            if config.has_status {
                "sparse_read"
            } else {
                "read"
            },
        )),
        Vec::new(),
        load_args,
    ));

    let body = if config.has_status {
        Vec::from([
            ast::Statement {
                kind: ast::StatementKind::Var(ast::VarDef {
                    local_type: ast::Type::from_layout(ast::TypeLayout(
                        metal_lib_identifier("sparse_color"),
                        Vec::from([ast::ExpressionOrType::Type(ast::TypeId::from(build_vec(
                            "T", 4,
                        )))])
                        .into_boxed_slice(),
                    )),
                    defs: Vec::from([ast::InitDeclarator {
                        declarator: ast::Declarator::Identifier(
                            ast::ScopedIdentifier::trivial("color"),
                            Vec::new(),
                        ),
                        location_annotations: Vec::new(),
                        init: Some(ast::Initializer::Expression(load_expr)),
                    }]),
                }),
                location: SourceLocation::UNKNOWN,
                attributes: Vec::new(),
            },
            ast::Statement {
                kind: ast::StatementKind::Expression(ast::Expression::BinaryOperation(
                    ast::BinOp::Assignment,
                    Box::new(Located::none(ast::Expression::Identifier(
                        ast::ScopedIdentifier::trivial("status"),
                    ))),
                    Box::new(Located::none(ast::Expression::Call(
                        Box::new(build_expr_member("color", "resident")),
                        Vec::new(),
                        Vec::new(),
                    ))),
                )),
                location: SourceLocation::UNKNOWN,
                attributes: Vec::new(),
            },
            ast::Statement {
                kind: ast::StatementKind::Return(Some(Located::none(ast::Expression::Call(
                    Box::new(build_expr_member("color", "value")),
                    Vec::new(),
                    Vec::new(),
                )))),
                location: SourceLocation::UNKNOWN,
                attributes: Vec::new(),
            },
        ])
    } else {
        Vec::from([ast::Statement {
            kind: ast::StatementKind::Return(Some(load_expr)),
            location: SourceLocation::UNKNOWN,
            attributes: Vec::new(),
        }])
    };

    Ok(ast::FunctionDefinition {
        name: Located::none(String::from("Load")),
        returntype: ast::FunctionReturn {
            return_type: build_vec("T", 4),
            location_annotations: Vec::new(),
        },
        template_params: ast::TemplateParamList(Vec::from([ast::TemplateParam::Type(
            ast::TemplateTypeParam {
                name: Some(Located::none(String::from("T"))),
                default: None,
            },
        )])),
        params,
        is_const: false,
        is_volatile: false,
        body: Some(body),
        attributes: Vec::new(),
    })
}

/// Create a definition for various texture store methods
fn build_texture_store(dim: Dim) -> Result<ast::FunctionDefinition, GenerateError> {
    let location_type = match dim {
        Dim::TexelBuffer => "uint",
        Dim::Tex2D => "uint2",
        Dim::Tex2DArray | Dim::Tex3D => "uint3",
        _ => unreachable!(),
    };

    let params = Vec::from([
        build_param(build_texture(dim.get_type_name(), "T", true), "texture"),
        build_param(ast::Type::trivial(location_type), "location"),
        build_param(build_vec("T", 4), "value"),
    ]);

    let mut load_args = Vec::new();
    load_args.push(Located::none(ast::Expression::Identifier(
        ast::ScopedIdentifier::trivial("value"),
    )));
    match dim {
        Dim::Tex2D | Dim::Tex3D | Dim::TexelBuffer => {
            load_args.push(Located::none(ast::Expression::Identifier(
                ast::ScopedIdentifier::trivial("location"),
            )));
        }
        Dim::Tex2DArray => {
            load_args.push(build_expr_member("location", "xy"));
            load_args.push(build_expr_member("location", "z"));
        }
        Dim::TexCube | Dim::TexCubeArray => unreachable!(),
    }

    let load_expr = ast::Expression::Call(
        Box::new(build_expr_member("texture", "write")),
        Vec::new(),
        load_args,
    );

    let body = Vec::from([
        ast::Statement {
            kind: ast::StatementKind::Expression(load_expr),
            location: SourceLocation::UNKNOWN,
            attributes: Vec::new(),
        },
        ast::Statement {
            kind: ast::StatementKind::Return(Some(Located::none(ast::Expression::Identifier(
                ast::ScopedIdentifier::trivial("value"),
            )))),
            location: SourceLocation::UNKNOWN,
            attributes: Vec::new(),
        },
    ]);

    Ok(ast::FunctionDefinition {
        name: Located::none(String::from("Store")),
        returntype: ast::FunctionReturn {
            return_type: build_vec("T", 4),
            location_annotations: Vec::new(),
        },
        template_params: ast::TemplateParamList(Vec::from([ast::TemplateParam::Type(
            ast::TemplateTypeParam {
                name: Some(Located::none(String::from("T"))),
                default: None,
            },
        )])),
        params,
        is_const: false,
        is_volatile: false,
        body: Some(body),
        attributes: Vec::new(),
    })
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SampleHelper {
    pub dim: Dim,
    pub has_lod: bool,
    pub has_offset: bool,
    pub has_clamp: bool,
    pub has_status: bool,
}

impl SampleHelper {
    fn is_array(&self) -> bool {
        matches!(self.dim, Dim::Tex2DArray | Dim::TexCubeArray)
    }

    fn is_3d(&self) -> bool {
        matches!(self.dim, Dim::Tex3D)
    }

    fn has_offset(&self) -> bool {
        self.has_offset
    }

    fn has_clamp(&self) -> bool {
        self.has_clamp
    }

    fn has_status(&self) -> bool {
        self.has_status
    }
}

/// Create a definition for various texture sample methods
fn build_texture_sample(config: SampleHelper) -> Result<ast::FunctionDefinition, GenerateError> {
    let coord_type = match config.dim {
        Dim::Tex2D => "float2",
        Dim::Tex2DArray | Dim::TexCube | Dim::Tex3D => "float3",
        Dim::TexCubeArray => "float4",
        Dim::TexelBuffer => panic!("Texel buffers do not support sampling"),
    };

    let offset_type = match config.is_3d() {
        false => "int2",
        true => "int3",
    };

    let mut params = Vec::new();
    params.push(build_param(
        build_texture(config.dim.get_type_name(), "T", false),
        "texture",
    ));
    params.push(build_param(build_sampler(), "s"));
    params.push(build_param(ast::Type::trivial(coord_type), "coord"));
    if config.has_lod {
        params.push(build_param(ast::Type::trivial("float"), "lod"));
    }
    if config.has_offset() {
        params.push(build_param(ast::Type::trivial(offset_type), "offset"));
    }
    if config.has_clamp() {
        params.push(build_param(ast::Type::trivial("float"), "clamp"));
    }
    if config.has_status() {
        params.push(build_out_param(ast::Type::trivial("uint"), "status"));
    }

    let mut sample_args = Vec::new();
    sample_args.push(Located::none(ast::Expression::Identifier(
        ast::ScopedIdentifier::trivial("s"),
    )));
    if config.is_array() {
        let swizzle = if config.dim == Dim::TexCubeArray {
            "xyz"
        } else {
            "xy"
        };
        sample_args.push(build_expr_member("coord", swizzle));
    } else {
        sample_args.push(Located::none(ast::Expression::Identifier(
            ast::ScopedIdentifier::trivial("coord"),
        )));
    }
    if config.is_array() {
        sample_args.push(Located::none(ast::Expression::Call(
            Box::new(Located::none(ast::Expression::Identifier(
                ast::ScopedIdentifier::trivial("uint"),
            ))),
            Vec::new(),
            Vec::from([build_expr_member("coord", "z")]),
        )));
    }
    if config.has_lod {
        sample_args.push(Located::none(ast::Expression::Call(
            Box::new(Located::none(ast::Expression::Identifier(
                metal_lib_identifier("level"),
            ))),
            Vec::new(),
            Vec::from([Located::none(ast::Expression::Identifier(
                ast::ScopedIdentifier::trivial("lod"),
            ))]),
        )));
    }
    if config.has_clamp() {
        sample_args.push(Located::none(ast::Expression::Call(
            Box::new(Located::none(ast::Expression::Identifier(
                metal_lib_identifier("min_lod_clamp"),
            ))),
            Vec::new(),
            Vec::from([Located::none(ast::Expression::Identifier(
                ast::ScopedIdentifier::trivial("clamp"),
            ))]),
        )));
    }
    if config.has_offset() {
        sample_args.push(Located::none(ast::Expression::Identifier(
            ast::ScopedIdentifier::trivial("offset"),
        )));
    }

    let load_expr = Located::none(ast::Expression::Call(
        Box::new(build_expr_member(
            "texture",
            if config.has_status() {
                "sparse_sample"
            } else {
                "sample"
            },
        )),
        Vec::new(),
        sample_args,
    ));

    let body = if config.has_status() {
        Vec::from([
            ast::Statement {
                kind: ast::StatementKind::Var(ast::VarDef {
                    local_type: ast::Type::from_layout(ast::TypeLayout(
                        metal_lib_identifier("sparse_color"),
                        Vec::from([ast::ExpressionOrType::Type(ast::TypeId::from(build_vec(
                            "T", 4,
                        )))])
                        .into_boxed_slice(),
                    )),
                    defs: Vec::from([ast::InitDeclarator {
                        declarator: ast::Declarator::Identifier(
                            ast::ScopedIdentifier::trivial("color"),
                            Vec::new(),
                        ),
                        location_annotations: Vec::new(),
                        init: Some(ast::Initializer::Expression(load_expr)),
                    }]),
                }),
                location: SourceLocation::UNKNOWN,
                attributes: Vec::new(),
            },
            ast::Statement {
                kind: ast::StatementKind::Expression(ast::Expression::BinaryOperation(
                    ast::BinOp::Assignment,
                    Box::new(Located::none(ast::Expression::Identifier(
                        ast::ScopedIdentifier::trivial("status"),
                    ))),
                    Box::new(Located::none(ast::Expression::Call(
                        Box::new(build_expr_member("color", "resident")),
                        Vec::new(),
                        Vec::new(),
                    ))),
                )),
                location: SourceLocation::UNKNOWN,
                attributes: Vec::new(),
            },
            ast::Statement {
                kind: ast::StatementKind::Return(Some(Located::none(ast::Expression::Call(
                    Box::new(build_expr_member("color", "value")),
                    Vec::new(),
                    Vec::new(),
                )))),
                location: SourceLocation::UNKNOWN,
                attributes: Vec::new(),
            },
        ])
    } else {
        Vec::from([ast::Statement {
            kind: ast::StatementKind::Return(Some(load_expr)),
            location: SourceLocation::UNKNOWN,
            attributes: Vec::new(),
        }])
    };

    let name = if config.has_lod {
        "SampleLevel"
    } else {
        "Sample"
    };

    Ok(ast::FunctionDefinition {
        name: Located::none(String::from(name)),
        returntype: ast::FunctionReturn {
            return_type: build_vec("T", 4),
            location_annotations: Vec::new(),
        },
        template_params: ast::TemplateParamList(Vec::from([ast::TemplateParam::Type(
            ast::TemplateTypeParam {
                name: Some(Located::none(String::from("T"))),
                default: None,
            },
        )])),
        params,
        is_const: false,
        is_volatile: false,
        body: Some(body),
        attributes: Vec::new(),
    })
}

/// Build a Load / Load2 / Load3 / Load4 / Load<T> for a buffer
fn build_address_load() -> Result<ast::FunctionDefinition, GenerateError> {
    let params = Vec::from([ast::FunctionParam {
        param_type: ast::Type::from("uint"),
        declarator: ast::Declarator::Identifier(
            ast::ScopedIdentifier::trivial("offset"),
            Vec::new(),
        ),
        location_annotations: Vec::new(),
        default_expr: None,
    }]);

    let target = {
        let mut target = ast::TypeId::from("T");
        target
            .base
            .modifiers
            .prepend(Located::none(ast::TypeModifier::Const));
        target
            .base
            .modifiers
            .prepend(Located::none(ast::TypeModifier::AddressSpace(
                ast::AddressSpace::Device,
            )));
        target.abstract_declarator = ast::Declarator::Pointer(ast::PointerDeclarator {
            attributes: Vec::new(),
            qualifiers: ast::TypeModifierSet::new(),
            inner: Box::new(target.abstract_declarator),
        });
        target
    };

    let address = ast::Expression::BinaryOperation(
        ast::BinOp::Add,
        Box::new(Located::none(ast::Expression::Identifier(
            ast::ScopedIdentifier::trivial("address"),
        ))),
        Box::new(Located::none(ast::Expression::Identifier(
            ast::ScopedIdentifier::trivial("offset"),
        ))),
    );

    let typed_address = ast::Expression::Call(
        Box::new(Located::none(ast::Expression::Identifier(
            ast::ScopedIdentifier::trivial("reinterpret_cast"),
        ))),
        Vec::from([ast::ExpressionOrType::Type(target)]),
        Vec::from([Located::none(address)]),
    );

    let deref = ast::Expression::UnaryOperation(
        ast::UnaryOp::Dereference,
        Box::new(Located::none(typed_address)),
    );

    let protect_cond = build_address_out_of_bounds_condition();
    let default_value = build_out_of_bounds_default_value();
    let protected = ast::Expression::TernaryConditional(
        Box::new(Located::none(protect_cond)),
        Box::new(Located::none(deref)),
        Box::new(Located::none(default_value)),
    );

    Ok(ast::FunctionDefinition {
        name: Located::none(String::from("Load")),
        returntype: ast::FunctionReturn {
            return_type: ast::Type::from("T"),
            location_annotations: Vec::new(),
        },
        template_params: ast::TemplateParamList(Vec::from([ast::TemplateParam::Type(
            ast::TemplateTypeParam {
                name: Some(Located::none(String::from("T"))),
                default: None,
            },
        )])),
        params,
        is_const: true,
        is_volatile: false,
        body: Some(Vec::from([ast::Statement {
            kind: ast::StatementKind::Return(Some(Located::none(protected))),
            location: SourceLocation::UNKNOWN,
            attributes: Vec::new(),
        }])),
        attributes: Vec::new(),
    })
}

/// Build a Store / Store2 / Store3 / Store4 for a byte buffer
fn build_address_store() -> Result<ast::FunctionDefinition, GenerateError> {
    let params = Vec::from([
        ast::FunctionParam {
            param_type: ast::Type::from("uint"),
            declarator: ast::Declarator::Identifier(
                ast::ScopedIdentifier::trivial("offset"),
                Vec::new(),
            ),
            location_annotations: Vec::new(),
            default_expr: None,
        },
        ast::FunctionParam {
            param_type: ast::Type::from("T"),
            declarator: ast::Declarator::Identifier(
                ast::ScopedIdentifier::trivial("value"),
                Vec::new(),
            ),
            location_annotations: Vec::new(),
            default_expr: None,
        },
    ]);

    let target = {
        let mut target = ast::TypeId::from("T");
        target
            .base
            .modifiers
            .prepend(Located::none(ast::TypeModifier::AddressSpace(
                ast::AddressSpace::Device,
            )));
        target.abstract_declarator = ast::Declarator::Pointer(ast::PointerDeclarator {
            attributes: Vec::new(),
            qualifiers: ast::TypeModifierSet::new(),
            inner: Box::new(target.abstract_declarator),
        });
        target
    };

    let address = ast::Expression::BinaryOperation(
        ast::BinOp::Add,
        Box::new(Located::none(ast::Expression::Identifier(
            ast::ScopedIdentifier::trivial("address"),
        ))),
        Box::new(Located::none(ast::Expression::Identifier(
            ast::ScopedIdentifier::trivial("offset"),
        ))),
    );

    let typed_address = ast::Expression::Call(
        Box::new(Located::none(ast::Expression::Identifier(
            ast::ScopedIdentifier::trivial("reinterpret_cast"),
        ))),
        Vec::from([ast::ExpressionOrType::Type(target)]),
        Vec::from([Located::none(address)]),
    );

    let typed_reference = ast::Expression::UnaryOperation(
        ast::UnaryOp::Dereference,
        Box::new(Located::none(typed_address)),
    );

    let assign = ast::Expression::BinaryOperation(
        ast::BinOp::Assignment,
        Box::new(Located::none(typed_reference)),
        Box::new(Located::none(ast::Expression::Identifier(
            ast::ScopedIdentifier::trivial("value"),
        ))),
    );

    let protect_cond = build_address_out_of_bounds_condition();

    Ok(ast::FunctionDefinition {
        name: Located::none(String::from("Store")),
        returntype: ast::FunctionReturn {
            return_type: ast::Type::from("void"),
            location_annotations: Vec::new(),
        },
        template_params: ast::TemplateParamList(Vec::from([ast::TemplateParam::Type(
            ast::TemplateTypeParam {
                name: Some(Located::none(String::from("T"))),
                default: None,
            },
        )])),
        params,
        is_const: true,
        is_volatile: false,
        body: Some(Vec::from([build_if_block(
            protect_cond,
            Vec::from([ast::Statement {
                kind: ast::StatementKind::Expression(assign),
                location: SourceLocation::UNKNOWN,
                attributes: Vec::new(),
            }]),
        )])),
        attributes: Vec::new(),
    })
}

fn build_address_out_of_bounds_condition() -> ast::Expression {
    ast::Expression::BinaryOperation(
        ast::BinOp::LessEqual,
        Box::new(Located::none(ast::Expression::BinaryOperation(
            ast::BinOp::Add,
            Box::new(Located::none(ast::Expression::Identifier(
                ast::ScopedIdentifier::trivial("offset"),
            ))),
            Box::new(Located::none(ast::Expression::SizeOf(Box::new(
                ast::ExpressionOrType::Type(ast::TypeId::from("T")),
            )))),
        ))),
        Box::new(Located::none(ast::Expression::Call(
            Box::new(Located::none(ast::Expression::Identifier(
                ast::ScopedIdentifier::trivial("static_cast"),
            ))),
            Vec::from([ast::ExpressionOrType::Type(ast::TypeId::from("uint"))]),
            Vec::from([Located::none(ast::Expression::Identifier(
                ast::ScopedIdentifier::trivial("size"),
            ))]),
        ))),
    )
}

fn build_out_of_bounds_default_value() -> ast::Expression {
    // We would ideally want to initialise to all zero bits but we instead use value initialization
    ast::Expression::BracedInit(Box::new(ast::TypeId::from("T")), Vec::new())
}

fn build_if_block(cond: ast::Expression, statements: Vec<ast::Statement>) -> ast::Statement {
    ast::Statement {
        kind: ast::StatementKind::If(
            Located::none(cond),
            Box::new(ast::Statement {
                kind: ast::StatementKind::Block(statements),
                location: SourceLocation::UNKNOWN,
                attributes: Vec::new(),
            }),
        ),
        location: SourceLocation::UNKNOWN,
        attributes: Vec::new(),
    }
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum BufferAtomicOp {
    Add,
    And,
    Exchange,
    Max,
    Min,
    Or,
    Xor,
}

impl BufferAtomicOp {
    /// Get the name for the generated function
    pub fn get_helper_name(&self) -> &'static str {
        match self {
            BufferAtomicOp::Add => "InterlockedAdd",
            BufferAtomicOp::And => "InterlockedAnd",
            BufferAtomicOp::Exchange => "InterlockedExchange",
            BufferAtomicOp::Max => "InterlockedMax",
            BufferAtomicOp::Min => "InterlockedMin",
            BufferAtomicOp::Or => "InterlockedOr",
            BufferAtomicOp::Xor => "InterlockedXor",
        }
    }

    /// Get the name for the intrinsic function we will call
    pub fn get_intrinsic_name(&self) -> &'static str {
        match self {
            BufferAtomicOp::Add => "atomic_fetch_add_explicit",
            BufferAtomicOp::And => "atomic_fetch_and_explicit",
            BufferAtomicOp::Exchange => "atomic_exchange_explicit",
            BufferAtomicOp::Max => "atomic_fetch_max_explicit",
            BufferAtomicOp::Min => "atomic_fetch_min_explicit",
            BufferAtomicOp::Or => "atomic_fetch_or_explicit",
            BufferAtomicOp::Xor => "atomic_fetch_xor_explicit",
        }
    }
}

/// Build a helper for an Interlocked operation for a byte buffer
fn build_address_atomic(op: BufferAtomicOp) -> Result<ast::FunctionDefinition, GenerateError> {
    let params = Vec::from([
        ast::FunctionParam {
            param_type: ast::Type::from("uint"),
            declarator: ast::Declarator::Identifier(
                ast::ScopedIdentifier::trivial("dest"),
                Vec::new(),
            ),
            location_annotations: Vec::new(),
            default_expr: None,
        },
        ast::FunctionParam {
            param_type: ast::Type::from("uint"),
            declarator: ast::Declarator::Identifier(
                ast::ScopedIdentifier::trivial("value"),
                Vec::new(),
            ),
            location_annotations: Vec::new(),
            default_expr: None,
        },
        ast::FunctionParam {
            param_type: {
                let mut ty = ast::Type::from("uint");
                ty.modifiers
                    .prepend(Located::none(ast::TypeModifier::AddressSpace(
                        ast::AddressSpace::Thread,
                    )));
                ty
            },
            declarator: ast::Declarator::Reference(ast::ReferenceDeclarator {
                attributes: Vec::new(),
                inner: Box::new(ast::Declarator::Identifier(
                    ast::ScopedIdentifier::trivial("original_value"),
                    Vec::new(),
                )),
            }),
            location_annotations: Vec::new(),
            default_expr: None,
        },
    ]);

    let address = ast::Expression::BinaryOperation(
        ast::BinOp::Add,
        Box::new(Located::none(ast::Expression::Identifier(
            ast::ScopedIdentifier::trivial("address"),
        ))),
        Box::new(Located::none(ast::Expression::Identifier(
            ast::ScopedIdentifier::trivial("dest"),
        ))),
    );

    let typed_address = ast::Expression::Call(
        Box::new(Located::none(ast::Expression::Identifier(
            ast::ScopedIdentifier::trivial("reinterpret_cast"),
        ))),
        Vec::from([ast::ExpressionOrType::Type(ast::TypeId {
            base: ast::Type {
                layout: ast::TypeLayout(
                    metal_lib_identifier("atomic"),
                    Vec::from([ast::ExpressionOrType::Type(ast::TypeId::from("uint"))])
                        .into_boxed_slice(),
                ),
                modifiers: ast::TypeModifierSet::from(&[Located::none(
                    ast::TypeModifier::AddressSpace(ast::AddressSpace::Device),
                )]),
                location: SourceLocation::UNKNOWN,
            },
            abstract_declarator: ast::Declarator::Pointer(ast::PointerDeclarator {
                attributes: Vec::new(),
                qualifiers: ast::TypeModifierSet::default(),
                inner: Box::new(ast::Declarator::Empty),
            }),
        })]),
        Vec::from([Located::none(address)]),
    );

    let atomic_call = ast::Expression::BinaryOperation(
        ast::BinOp::Assignment,
        Box::new(Located::none(ast::Expression::Identifier(
            ast::ScopedIdentifier::trivial("original_value"),
        ))),
        Box::new(Located::none(ast::Expression::Call(
            Box::new(Located::none(ast::Expression::Identifier(
                ast::ScopedIdentifier::trivial(op.get_intrinsic_name()),
            ))),
            Vec::new(),
            Vec::from([
                Located::none(typed_address),
                Located::none(ast::Expression::Identifier(ast::ScopedIdentifier::trivial(
                    "value",
                ))),
                Located::none(ast::Expression::Identifier(ast::ScopedIdentifier {
                    base: ast::ScopedIdentifierBase::Relative,
                    identifiers: Vec::from([
                        Located::none(String::from("metal")),
                        Located::none(String::from("memory_order")),
                        Located::none(String::from("memory_order_relaxed")),
                    ]),
                })),
            ]),
        ))),
    );

    Ok(ast::FunctionDefinition {
        name: Located::none(String::from(op.get_helper_name())),
        returntype: ast::FunctionReturn {
            return_type: ast::Type::from("void"),
            location_annotations: Vec::new(),
        },
        template_params: ast::TemplateParamList(Vec::new()),
        params,
        is_const: true,
        is_volatile: false,
        body: Some(Vec::from([ast::Statement {
            kind: ast::StatementKind::Expression(atomic_call),
            location: SourceLocation::UNKNOWN,
            attributes: Vec::new(),
        }])),
        attributes: Vec::new(),
    })
}

/// Create a definition for byte buffer GetDimensions
fn build_address_get_dimensions() -> Result<ast::FunctionDefinition, GenerateError> {
    let mut params = Vec::new();
    let mut body = Vec::new();

    params.push(build_out_param(ast::Type::trivial("uint"), "dim"));
    body.push(ast::Statement {
        kind: ast::StatementKind::Expression(ast::Expression::BinaryOperation(
            ast::BinOp::Assignment,
            Box::new(Located::none(ast::Expression::Identifier(
                ast::ScopedIdentifier::trivial("dim"),
            ))),
            Box::new(Located::none(ast::Expression::Call(
                Box::new(Located::none(ast::Expression::Identifier(
                    ast::ScopedIdentifier::trivial("static_cast"),
                ))),
                Vec::from([ast::ExpressionOrType::Type(ast::TypeId::from("uint"))]),
                Vec::from([Located::none(ast::Expression::Identifier(
                    ast::ScopedIdentifier::trivial("size"),
                ))]),
            ))),
        )),
        location: SourceLocation::UNKNOWN,
        attributes: Vec::new(),
    });

    Ok(ast::FunctionDefinition {
        name: Located::none(String::from("GetDimensions")),
        returntype: ast::FunctionReturn {
            return_type: ast::Type::trivial("void"),
            location_annotations: Vec::new(),
        },
        template_params: ast::TemplateParamList(Vec::new()),
        params,
        is_const: true,
        is_volatile: false,
        body: Some(body),
        attributes: Vec::new(),
    })
}

/// Build a Load for a StructuredBuffer / RWStructuredBuffer
fn build_structured_buffer_load() -> Result<ast::FunctionDefinition, GenerateError> {
    let params = Vec::from([ast::FunctionParam {
        param_type: ast::Type::from("int"),
        declarator: ast::Declarator::Identifier(
            ast::ScopedIdentifier::trivial("location"),
            Vec::new(),
        ),
        location_annotations: Vec::new(),
        default_expr: None,
    }]);

    let access = ast::Expression::ArraySubscript(
        Box::new(Located::none(ast::Expression::Identifier(
            ast::ScopedIdentifier::trivial("address"),
        ))),
        Box::new(Located::none(ast::Expression::Identifier(
            ast::ScopedIdentifier::trivial("location"),
        ))),
    );

    let protect_cond = build_structured_buffer_out_of_bounds_condition();
    let default_value = build_out_of_bounds_default_value();
    let protected = ast::Expression::TernaryConditional(
        Box::new(Located::none(protect_cond)),
        Box::new(Located::none(access)),
        Box::new(Located::none(default_value)),
    );

    Ok(ast::FunctionDefinition {
        name: Located::none(String::from("Load")),
        returntype: ast::FunctionReturn {
            return_type: ast::Type::from("T"),
            location_annotations: Vec::new(),
        },
        template_params: ast::TemplateParamList(Vec::new()),
        params,
        is_const: true,
        is_volatile: false,
        body: Some(Vec::from([ast::Statement {
            kind: ast::StatementKind::Return(Some(Located::none(protected))),
            location: SourceLocation::UNKNOWN,
            attributes: Vec::new(),
        }])),
        attributes: Vec::new(),
    })
}

/// Build a Store for a StructuredBuffer / RWStructuredBuffer
fn build_structured_buffer_store() -> Result<ast::FunctionDefinition, GenerateError> {
    let params = Vec::from([
        ast::FunctionParam {
            param_type: ast::Type::from("uint"),
            declarator: ast::Declarator::Identifier(
                ast::ScopedIdentifier::trivial("location"),
                Vec::new(),
            ),
            location_annotations: Vec::new(),
            default_expr: None,
        },
        ast::FunctionParam {
            param_type: ast::Type::from("T"),
            declarator: ast::Declarator::Identifier(
                ast::ScopedIdentifier::trivial("value"),
                Vec::new(),
            ),
            location_annotations: Vec::new(),
            default_expr: None,
        },
    ]);

    let access = ast::Expression::ArraySubscript(
        Box::new(Located::none(ast::Expression::Identifier(
            ast::ScopedIdentifier::trivial("address"),
        ))),
        Box::new(Located::none(ast::Expression::Identifier(
            ast::ScopedIdentifier::trivial("location"),
        ))),
    );

    let assign = ast::Expression::BinaryOperation(
        ast::BinOp::Assignment,
        Box::new(Located::none(access)),
        Box::new(Located::none(ast::Expression::Identifier(
            ast::ScopedIdentifier::trivial("value"),
        ))),
    );

    let protect_cond = build_structured_buffer_out_of_bounds_condition();
    let protected = ast::Expression::TernaryConditional(
        Box::new(Located::none(protect_cond)),
        Box::new(Located::none(assign)),
        Box::new(Located::none(ast::Expression::Identifier(
            ast::ScopedIdentifier::trivial("value"),
        ))),
    );

    Ok(ast::FunctionDefinition {
        name: Located::none(String::from("Store")),
        returntype: ast::FunctionReturn {
            return_type: ast::Type::from("T"),
            location_annotations: Vec::new(),
        },
        template_params: ast::TemplateParamList(Vec::new()),
        params,
        is_const: true,
        is_volatile: false,
        body: Some(Vec::from([ast::Statement {
            kind: ast::StatementKind::Return(Some(Located::none(protected))),
            location: SourceLocation::UNKNOWN,
            attributes: Vec::new(),
        }])),
        attributes: Vec::new(),
    })
}

/// Create a definition for StructuredBuffer / RWStructuredBuffer GetDimensions
fn build_structured_buffer_get_dimensions() -> Result<ast::FunctionDefinition, GenerateError> {
    let mut params = Vec::new();
    let mut body = Vec::new();

    params.push(build_out_param(ast::Type::trivial("uint"), "numStructs"));
    body.push(ast::Statement {
        kind: ast::StatementKind::Expression(ast::Expression::BinaryOperation(
            ast::BinOp::Assignment,
            Box::new(Located::none(ast::Expression::Identifier(
                ast::ScopedIdentifier::trivial("numStructs"),
            ))),
            Box::new(Located::none(ast::Expression::Call(
                Box::new(Located::none(ast::Expression::Identifier(
                    ast::ScopedIdentifier::trivial("static_cast"),
                ))),
                Vec::from([ast::ExpressionOrType::Type(ast::TypeId::from("uint"))]),
                Vec::from([Located::none(ast::Expression::Identifier(
                    ast::ScopedIdentifier::trivial("size"),
                ))]),
            ))),
        )),
        location: SourceLocation::UNKNOWN,
        attributes: Vec::new(),
    });

    params.push(build_out_param(ast::Type::trivial("uint"), "stride"));
    body.push(ast::Statement {
        kind: ast::StatementKind::Expression(ast::Expression::BinaryOperation(
            ast::BinOp::Assignment,
            Box::new(Located::none(ast::Expression::Identifier(
                ast::ScopedIdentifier::trivial("stride"),
            ))),
            Box::new(Located::none(ast::Expression::Call(
                Box::new(Located::none(ast::Expression::Identifier(
                    ast::ScopedIdentifier::trivial("sizeof"),
                ))),
                Vec::new(),
                Vec::from([Located::none(ast::Expression::Identifier(
                    ast::ScopedIdentifier::trivial("T"),
                ))]),
            ))),
        )),
        location: SourceLocation::UNKNOWN,
        attributes: Vec::new(),
    });

    Ok(ast::FunctionDefinition {
        name: Located::none(String::from("GetDimensions")),
        returntype: ast::FunctionReturn {
            return_type: ast::Type::trivial("void"),
            location_annotations: Vec::new(),
        },
        template_params: ast::TemplateParamList(Vec::new()),
        params,
        is_const: true,
        is_volatile: false,
        body: Some(body),
        attributes: Vec::new(),
    })
}

fn build_structured_buffer_out_of_bounds_condition() -> ast::Expression {
    ast::Expression::BinaryOperation(
        ast::BinOp::LessThan,
        Box::new(Located::none(ast::Expression::Identifier(
            ast::ScopedIdentifier::trivial("location"),
        ))),
        Box::new(Located::none(ast::Expression::Identifier(
            ast::ScopedIdentifier::trivial("size"),
        ))),
    )
}

/// Build a helper function to turn a scalar or smaller vector into a 4 component vector
fn build_vector_extend(count: u32) -> Result<ast::FunctionDefinition, GenerateError> {
    let mut params = Vec::new();
    let mut body = Vec::new();

    let value_type = if count == 1 {
        ast::Type::trivial("T")
    } else {
        build_vec("T", count as u64)
    };

    let constructor_args = if count == 1 {
        Vec::from([
            Located::none(ast::Expression::Identifier(ast::ScopedIdentifier::trivial(
                "value",
            ))),
            Located::none(ast::Expression::Literal(ast::Literal::IntUntyped(0))),
            Located::none(ast::Expression::Literal(ast::Literal::IntUntyped(0))),
            Located::none(ast::Expression::Literal(ast::Literal::IntUntyped(0))),
        ])
    } else {
        Vec::from([
            Located::none(ast::Expression::Identifier(ast::ScopedIdentifier::trivial(
                "value",
            ))),
            Located::none(ast::Expression::Literal(ast::Literal::IntUntyped(0))),
        ])
    };

    params.push(build_param(value_type, "value"));
    body.push(ast::Statement {
        kind: ast::StatementKind::Return(Some(Located::none(ast::Expression::Call(
            Box::new(Located::none(ast::Expression::Identifier(
                metal_lib_identifier("vec"),
            ))),
            Vec::from([
                ast::ExpressionOrType::Type(ast::TypeId::from("T")),
                ast::ExpressionOrType::Expression(Located::none(ast::Expression::Literal(
                    ast::Literal::IntUntyped(4),
                ))),
            ]),
            constructor_args,
        )))),
        location: SourceLocation::UNKNOWN,
        attributes: Vec::new(),
    });

    Ok(ast::FunctionDefinition {
        name: Located::none(String::from("extend")),
        returntype: ast::FunctionReturn {
            return_type: build_vec("T", 4),
            location_annotations: Vec::new(),
        },
        template_params: ast::TemplateParamList(Vec::from([ast::TemplateParam::Type(
            ast::TemplateTypeParam {
                name: Some(Located::none(String::from("T"))),
                default: None,
            },
        )])),
        params,
        is_const: false,
        is_volatile: false,
        body: Some(body),
        attributes: Vec::new(),
    })
}

/// Build a helper function to turn a coordinate without a mip index to a coordinate with a mip index of zero - and also int -> uint convert
fn build_unsign(push_zero: bool) -> Result<ast::FunctionDefinition, GenerateError> {
    let mut params = Vec::new();
    let mut body = Vec::new();

    let make_n = || {
        ast::ExpressionOrType::Expression(Located::none(ast::Expression::Identifier(
            ast::ScopedIdentifier::trivial("N"),
        )))
    };

    let make_n1 = || {
        ast::ExpressionOrType::Expression(Located::none(ast::Expression::BinaryOperation(
            ast::BinOp::Add,
            Box::new(Located::none(ast::Expression::Identifier(
                ast::ScopedIdentifier::trivial("N"),
            ))),
            Box::new(Located::none(ast::Expression::Literal(
                ast::Literal::IntUntyped(1),
            ))),
        )))
    };

    let make_int = || ast::ExpressionOrType::Type(ast::TypeId::from("int"));
    let make_uint = || ast::ExpressionOrType::Type(ast::TypeId::from("uint"));

    params.push(build_param(
        ast::Type::from_layout(ast::TypeLayout(
            metal_lib_identifier("vec"),
            Vec::from([make_uint(), make_n()]).into_boxed_slice(),
        )),
        "coord",
    ));

    let uint_to_int = ast::Expression::Call(
        Box::new(Located::none(ast::Expression::Identifier(
            metal_lib_identifier("vec"),
        ))),
        Vec::from([make_int(), make_n()]),
        Vec::from([Located::none(ast::Expression::Identifier(
            ast::ScopedIdentifier::trivial("coord"),
        ))]),
    );

    let extend = if push_zero {
        ast::Expression::Call(
            Box::new(Located::none(ast::Expression::Identifier(
                metal_lib_identifier("vec"),
            ))),
            Vec::from([make_int(), make_n1()]),
            Vec::from([
                Located::none(uint_to_int),
                Located::none(ast::Expression::Literal(ast::Literal::IntUnsigned32(0))),
            ]),
        )
    } else {
        uint_to_int
    };

    body.push(ast::Statement {
        kind: ast::StatementKind::Return(Some(Located::none(extend))),
        location: SourceLocation::UNKNOWN,
        attributes: Vec::new(),
    });

    Ok(ast::FunctionDefinition {
        name: Located::none(String::from(if push_zero {
            "make_signed_push_0"
        } else {
            "make_signed"
        })),
        returntype: ast::FunctionReturn {
            return_type: ast::Type::from_layout(ast::TypeLayout(
                metal_lib_identifier("vec"),
                Vec::from([make_int(), if push_zero { make_n1() } else { make_n() }])
                    .into_boxed_slice(),
            )),
            location_annotations: Vec::new(),
        },
        template_params: ast::TemplateParamList(Vec::from([ast::TemplateParam::Value(
            ast::TemplateValueParam {
                name: Some(Located::none(String::from("N"))),
                default: None,
                value_type: ast::Type::from("size_t"),
            },
        )])),
        params,
        is_const: false,
        is_volatile: false,
        body: Some(body),
        attributes: Vec::new(),
    })
}

/// Build a helper function to invoke set_index multiple times
fn build_mesh_output_set_indices(
    topology: ir::OutputTopology,
) -> Result<ast::FunctionDefinition, GenerateError> {
    let mut params = Vec::new();
    let mut body = Vec::new();

    let n = match topology {
        ir::OutputTopology::Point => 1,
        ir::OutputTopology::Line => 2,
        ir::OutputTopology::Triangle => 3,
    };

    params.push(build_param(ast::Type::from("Mesh"), "mesh"));
    params.push(build_param(ast::Type::from("uint"), "index"));
    params.push(build_param(
        ast::Type::from(format!("uint{}", n).as_str()),
        "indices",
    ));

    for i in 0..n {
        body.push(ast::Statement {
            kind: ast::StatementKind::Expression(ast::Expression::Call(
                Box::new(Located::none(ast::Expression::Member(
                    Box::new(Located::none(ast::Expression::Identifier(
                        ast::ScopedIdentifier::trivial("mesh"),
                    ))),
                    ast::ScopedIdentifier::trivial("set_index"),
                ))),
                Vec::new(),
                Vec::from([
                    Located::none(ast::Expression::BinaryOperation(
                        ast::BinOp::Add,
                        Box::new(Located::none(ast::Expression::BinaryOperation(
                            ast::BinOp::Multiply,
                            Box::new(Located::none(ast::Expression::Literal(
                                ast::Literal::IntUnsigned32(3),
                            ))),
                            Box::new(Located::none(ast::Expression::Identifier(
                                ast::ScopedIdentifier::trivial("index"),
                            ))),
                        ))),
                        Box::new(Located::none(ast::Expression::Literal(
                            ast::Literal::IntUnsigned32(i as u64),
                        ))),
                    )),
                    Located::none(ast::Expression::Member(
                        Box::new(Located::none(ast::Expression::Identifier(
                            ast::ScopedIdentifier::trivial("indices"),
                        ))),
                        ast::ScopedIdentifier::trivial(match i {
                            0 => "x",
                            1 => "y",
                            2 => "z",
                            _ => unreachable!(),
                        }),
                    )),
                ]),
            )),
            location: SourceLocation::UNKNOWN,
            attributes: Vec::new(),
        });
    }

    Ok(ast::FunctionDefinition {
        name: Located::none(String::from("set_indices")),
        returntype: ast::FunctionReturn {
            return_type: ast::Type::from_layout(ast::TypeLayout::trivial("void")),
            location_annotations: Vec::new(),
        },
        template_params: ast::TemplateParamList(Vec::from([ast::TemplateParam::Type(
            ast::TemplateTypeParam {
                name: Some(Located::none(String::from("Mesh"))),
                default: None,
            },
        )])),
        params,
        is_const: false,
        is_volatile: false,
        body: Some(body),
        attributes: Vec::new(),
    })
}

/// Build a helper function to create intersection_params
fn build_intersection_params() -> Result<ast::FunctionDefinition, GenerateError> {
    let params_ty = metal_raytracing_identifier("intersection_params");
    let params = Vec::from([ast::FunctionParam {
        param_type: ast::Type::from("uint"),
        declarator: ast::Declarator::Identifier(
            ast::ScopedIdentifier::trivial("flags"),
            Vec::new(),
        ),
        location_annotations: Vec::new(),
        default_expr: None,
    }]);
    fn enum_identifier(ty: &str, val: &str) -> ast::Expression {
        ast::Expression::Identifier(ast::ScopedIdentifier {
            // metal is a reserved name so we can drop the leading ::
            base: ast::ScopedIdentifierBase::Relative,
            identifiers: Vec::from([
                Located::none(String::from("metal")),
                Located::none(String::from("raytracing")),
                Located::none(String::from(ty)),
                Located::none(String::from(val)),
            ]),
        })
    }
    fn flag_cond(bit: u32, left: ast::Expression, right: ast::Expression) -> ast::Expression {
        ast::Expression::TernaryConditional(
            Box::new(Located::none(ast::Expression::BinaryOperation(
                ast::BinOp::BitwiseAnd,
                Box::new(Located::none(ast::Expression::Identifier(
                    ast::ScopedIdentifier::trivial("flags"),
                ))),
                Box::new(Located::none(ast::Expression::Literal(
                    ast::Literal::IntUnsigned32(bit as u64),
                ))),
            ))),
            Box::new(Located::none(left)),
            Box::new(Located::none(right)),
        )
    }
    let body = Vec::from([
        ast::Statement {
            kind: ast::StatementKind::Var(ast::VarDef {
                local_type: ast::Type::from(params_ty.clone()),
                defs: Vec::from([ast::InitDeclarator {
                    declarator: ast::Declarator::from(Located::none("params")),
                    location_annotations: Vec::new(),
                    init: None,
                }]),
            }),
            location: SourceLocation::UNKNOWN,
            attributes: Vec::new(),
        },
        ast::Statement {
            kind: ast::StatementKind::Expression(ast::Expression::Call(
                Box::new(build_expr_member("params", "force_opacity")),
                Vec::new(),
                Vec::from([Located::none(flag_cond(
                    0x01,
                    enum_identifier("forced_opacity", "opaque"),
                    flag_cond(
                        0x02,
                        enum_identifier("forced_opacity", "non_opaque"),
                        enum_identifier("forced_opacity", "none"),
                    ),
                ))]),
            )),
            location: SourceLocation::UNKNOWN,
            attributes: Vec::new(),
        },
        ast::Statement {
            kind: ast::StatementKind::Expression(ast::Expression::Call(
                Box::new(build_expr_member("params", "accept_any_intersection")),
                Vec::new(),
                Vec::from([Located::none(ast::Expression::BinaryOperation(
                    ast::BinOp::BitwiseAnd,
                    Box::new(Located::none(ast::Expression::Identifier(
                        ast::ScopedIdentifier::trivial("flags"),
                    ))),
                    Box::new(Located::none(ast::Expression::Literal(
                        ast::Literal::IntUnsigned32(0x04),
                    ))),
                ))]),
            )),
            location: SourceLocation::UNKNOWN,
            attributes: Vec::new(),
        },
        ast::Statement {
            kind: ast::StatementKind::Expression(ast::Expression::Call(
                Box::new(build_expr_member("params", "set_triangle_cull_mode")),
                Vec::new(),
                Vec::from([Located::none(flag_cond(
                    0x10,
                    enum_identifier("triangle_cull_mode", "back"),
                    flag_cond(
                        0x20,
                        enum_identifier("triangle_cull_mode", "front"),
                        enum_identifier("triangle_cull_mode", "none"),
                    ),
                ))]),
            )),
            location: SourceLocation::UNKNOWN,
            attributes: Vec::new(),
        },
        ast::Statement {
            kind: ast::StatementKind::Expression(ast::Expression::Call(
                Box::new(build_expr_member("params", "set_opacity_cull_mode")),
                Vec::new(),
                Vec::from([Located::none(flag_cond(
                    0x40,
                    enum_identifier("opacity_cull_mode", "opaque"),
                    flag_cond(
                        0x80,
                        enum_identifier("opacity_cull_mode", "non_opaque"),
                        enum_identifier("opacity_cull_mode", "none"),
                    ),
                ))]),
            )),
            location: SourceLocation::UNKNOWN,
            attributes: Vec::new(),
        },
        ast::Statement {
            kind: ast::StatementKind::Expression(ast::Expression::Call(
                Box::new(build_expr_member("params", "set_geometry_cull_mode")),
                Vec::new(),
                Vec::from([Located::none(ast::Expression::BinaryOperation(
                    ast::BinOp::BitwiseOr,
                    Box::new(Located::none(flag_cond(
                        0x100,
                        enum_identifier("geometry_cull_mode", "triangle"),
                        enum_identifier("geometry_cull_mode", "none"),
                    ))),
                    Box::new(Located::none(flag_cond(
                        0x200,
                        enum_identifier("geometry_cull_mode", "bounding_box"),
                        enum_identifier("geometry_cull_mode", "none"),
                    ))),
                ))]),
            )),
            location: SourceLocation::UNKNOWN,
            attributes: Vec::new(),
        },
        ast::Statement {
            kind: ast::StatementKind::Return(Some(Located::none(ast::Expression::Identifier(
                ast::ScopedIdentifier::trivial("params"),
            )))),
            location: SourceLocation::UNKNOWN,
            attributes: Vec::new(),
        },
    ]);
    Ok(ast::FunctionDefinition {
        name: Located::none(String::from("intersection_params")),
        returntype: ast::FunctionReturn {
            return_type: ast::Type::from(params_ty),
            location_annotations: Vec::new(),
        },
        template_params: ast::TemplateParamList(Vec::new()),
        params,
        is_const: false,
        is_volatile: false,
        body: Some(body),
        attributes: Vec::new(),
    })
}

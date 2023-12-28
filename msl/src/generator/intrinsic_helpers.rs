use std::collections::{HashMap, HashSet};

use rssl_ast as ast;
use rssl_text::{Located, SourceLocation};

use super::{metal_lib_identifier, GenerateError};
use crate::names::HELPER_NAMESPACE_NAME;

/// Represents a helper function that is generated to implement intrinsic operations
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum IntrinsicHelper {
    GetDimensions(GetDimensionsHelper),
    Load(LoadHelper),
    Sample(SampleHelper),
    AddressLoad,
    AddressStore,
    AddressAtomic(BufferAtomicOp),
}

/// Represents a helper struct that is generated to implement intrinsic operations
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum IntrinsicObject {
    ByteAddressBuffer,
    RWByteAddressBuffer,
}

/// Generate the function name required to call the helper
pub fn get_intrinsic_helper_name(intrinsic: IntrinsicHelper) -> &'static str {
    match intrinsic {
        IntrinsicHelper::GetDimensions(_) => "GetDimensions",
        IntrinsicHelper::Load(_) => "Load",
        IntrinsicHelper::Sample(_) => "Sample",
        IntrinsicHelper::AddressLoad => "Load",
        IntrinsicHelper::AddressStore => "Store",
        IntrinsicHelper::AddressAtomic(op) => op.get_helper_name(),
    }
}

impl IntrinsicObject {
    /// Get the generated name of the object
    pub fn get_name(&self) -> &'static str {
        match self {
            IntrinsicObject::ByteAddressBuffer => "ByteAddressBuffer",
            IntrinsicObject::RWByteAddressBuffer => "RWByteAddressBuffer",
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
            let member_vars = generate_members(object);
            let mut members = member_vars
                .into_iter()
                .map(ast::StructEntry::Variable)
                .collect::<Vec<_>>();
            members.extend(functions.into_iter().map(ast::StructEntry::Method));

            definitions.push(ast::RootDefinition::Struct(ast::StructDefinition {
                name: Located::none(String::from(object.get_name())),
                base_types: Vec::new(),
                template_params: ast::TemplateParamList(Vec::new()),
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
        Sample(config) => Ok(build_texture_sample(config)?),
        AddressLoad => Ok(build_address_load()?),
        AddressStore => Ok(build_address_store()?),
        AddressAtomic(op) => Ok(build_address_atomic(op)?),
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

            Vec::from([ast::StructMember {
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
            }])
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
}

/// Create a definition for various dimension fetching methods
fn build_get_dimensions(
    config: GetDimensionsHelper,
) -> Result<ast::FunctionDefinition, GenerateError> {
    let has_mips = !config.read_write;

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

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SampleHelper {
    pub dim: Dim,
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

    Ok(ast::FunctionDefinition {
        name: Located::none(String::from("Sample")),
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
            kind: ast::StatementKind::Return(Some(Located::none(deref))),
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

    Ok(ast::FunctionDefinition {
        name: Located::none(String::from("Store")),
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
            kind: ast::StatementKind::Expression(assign),
            location: SourceLocation::UNKNOWN,
            attributes: Vec::new(),
        }])),
        attributes: Vec::new(),
    })
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

use std::collections::HashSet;

use rssl_ast as ast;
use rssl_text::{Located, SourceLocation};

use super::{metal_lib_identifier, GenerateError};

/// Represents a helper function that is generated to implement intrinsic operations
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum IntrinsicHelper {
    Texture2DLoad,
    Texture2DLoadOffset,
    Texture2DLoadOffsetStatus,
    Texture2DArrayLoad,
    Texture2DArrayLoadOffset,
    Texture2DArrayLoadOffsetStatus,
    RWTexture2DLoad,
    RWTexture2DLoadStatus,
    RWTexture2DArrayLoad,
    RWTexture2DArrayLoadStatus,
    Texture3DLoad,
    Texture3DLoadOffset,
    Texture3DLoadOffsetStatus,
    RWTexture3DLoad,
    RWTexture3DLoadStatus,

    Sample(SampleHelper),
}

/// Generate the function name required to call the helper
pub fn get_intrinsic_helper_name(intrinsic: IntrinsicHelper) -> &'static str {
    match intrinsic {
        IntrinsicHelper::Texture2DLoad => "Load",
        IntrinsicHelper::Texture2DLoadOffset => "Load",
        IntrinsicHelper::Texture2DLoadOffsetStatus => "Load",
        IntrinsicHelper::Texture2DArrayLoad => "Load",
        IntrinsicHelper::Texture2DArrayLoadOffset => "Load",
        IntrinsicHelper::Texture2DArrayLoadOffsetStatus => "Load",
        IntrinsicHelper::RWTexture2DLoad => "Load",
        IntrinsicHelper::RWTexture2DLoadStatus => "Load",
        IntrinsicHelper::RWTexture2DArrayLoad => "Load",
        IntrinsicHelper::RWTexture2DArrayLoadStatus => "Load",
        IntrinsicHelper::Texture3DLoad => "Load",
        IntrinsicHelper::Texture3DLoadOffset => "Load",
        IntrinsicHelper::Texture3DLoadOffsetStatus => "Load",
        IntrinsicHelper::RWTexture3DLoad => "Load",
        IntrinsicHelper::RWTexture3DLoadStatus => "Load",

        IntrinsicHelper::Sample(_) => "Sample",
    }
}

/// Generate the definitions for all helpers
pub fn generate_helpers(
    required_helpers: HashSet<IntrinsicHelper>,
) -> Result<Option<ast::RootDefinition>, GenerateError> {
    let mut definitions = Vec::new();

    let mut ordered = Vec::from_iter(required_helpers);
    ordered.sort();

    for helper in ordered {
        definitions.push(generate_helper(helper)?);
    }

    if definitions.is_empty() {
        Ok(None)
    } else {
        Ok(Some(ast::RootDefinition::Namespace(
            Located::none(String::from("helper")),
            definitions,
        )))
    }
}

/// Generate the definition for a single helper
fn generate_helper(helper: IntrinsicHelper) -> Result<ast::RootDefinition, GenerateError> {
    use IntrinsicHelper::*;
    match helper {
        Texture2DLoad => Ok(build_texture_load(Dim::Tex2D, false, false, false)?),
        Texture2DLoadOffset => Ok(build_texture_load(Dim::Tex2D, false, true, false)?),
        Texture2DLoadOffsetStatus => Ok(build_texture_load(Dim::Tex2D, false, true, true)?),
        Texture2DArrayLoad => Ok(build_texture_load(Dim::Tex2DArray, false, false, false)?),
        Texture2DArrayLoadOffset => Ok(build_texture_load(Dim::Tex2DArray, false, true, false)?),
        Texture2DArrayLoadOffsetStatus => {
            Ok(build_texture_load(Dim::Tex2DArray, false, true, true)?)
        }
        RWTexture2DLoad => Ok(build_texture_load(Dim::Tex2D, true, false, false)?),
        RWTexture2DLoadStatus => Ok(build_texture_load(Dim::Tex2D, true, false, true)?),
        RWTexture2DArrayLoad => Ok(build_texture_load(Dim::Tex2DArray, true, false, false)?),
        RWTexture2DArrayLoadStatus => Ok(build_texture_load(Dim::Tex2DArray, true, false, true)?),
        Texture3DLoad => Ok(build_texture_load(Dim::Tex3D, false, false, false)?),
        Texture3DLoadOffset => Ok(build_texture_load(Dim::Tex3D, false, true, false)?),
        Texture3DLoadOffsetStatus => Ok(build_texture_load(Dim::Tex3D, false, true, true)?),
        RWTexture3DLoad => Ok(build_texture_load(Dim::Tex3D, true, false, false)?),
        RWTexture3DLoadStatus => Ok(build_texture_load(Dim::Tex3D, true, false, true)?),
        Sample(config) => Ok(build_texture_sample(config)?),
    }
}

/// Dimension of a texture object type
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Dim {
    Tex2D,
    Tex2DArray,
    Tex3D,
}

/// Create a type for a vec
fn build_vec(ty: &str, n: u64) -> ast::Type {
    ast::Type::from_layout(ast::TypeLayout(
        metal_lib_identifier("vec"),
        Vec::from([
            ast::ExpressionOrType::Type(ast::Type::trivial(ty)),
            ast::ExpressionOrType::Expression(Located::none(ast::Expression::Literal(
                ast::Literal::IntUntyped(n),
            ))),
        ])
        .into_boxed_slice(),
    ))
}

/// Create a type for a texture object
fn build_texture(name: &'static str, component: &'static str, read_write: bool) -> ast::Type {
    let mut type_args = Vec::from([ast::ExpressionOrType::Type(ast::Type::trivial(component))]);

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

/// Build a function parameter for a sparse status result
fn build_status_param() -> ast::FunctionParam {
    ast::FunctionParam {
        param_type: ast::Type {
            layout: ast::TypeLayout::trivial("uint"),
            modifiers: ast::TypeModifierSet {
                modifiers: Vec::from([Located::none(ast::TypeModifier::AddressSpace(
                    ast::AddressSpace::Thread,
                ))]),
            },
            location: SourceLocation::UNKNOWN,
        },
        declarator: ast::Declarator::Reference(ast::ReferenceDeclarator {
            attributes: Vec::new(),
            inner: Box::new(ast::Declarator::Identifier(
                ast::ScopedIdentifier::trivial("status"),
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

/// Create a definition for various texture load methods
fn build_texture_load(
    dim: Dim,
    read_write: bool,
    has_offset: bool,
    has_status: bool,
) -> Result<ast::RootDefinition, GenerateError> {
    let array_or_3d = matches!(dim, Dim::Tex2DArray | Dim::Tex3D);
    let need_z = matches!(dim, Dim::Tex3D);

    let location_type = match (array_or_3d, read_write) {
        (false, false) => "int3",
        (false, true) => "int2",
        (true, false) => "int4",
        (true, true) => "int3",
    };

    let texture_type = match dim {
        Dim::Tex2D => "texture2d",
        Dim::Tex2DArray => "texture2d_array",
        Dim::Tex3D => "texture3d",
    };

    let offset_type = match need_z {
        false => "int2",
        true => "int3",
    };

    let mut params = Vec::new();
    params.push(build_param(
        build_texture(texture_type, "T", read_write),
        "texture",
    ));
    params.push(build_param(ast::Type::trivial(location_type), "location"));
    if has_offset {
        params.push(build_param(ast::Type::trivial(offset_type), "offset"));
    }
    if has_status {
        params.push(build_status_param());
    }

    let mut coord_args = Vec::new();
    let make_coord_arg = |c| {
        if has_offset {
            Located::none(ast::Expression::BinaryOperation(
                ast::BinOp::Add,
                Box::new(build_expr_member("location", c)),
                Box::new(build_expr_member("offset", c)),
            ))
        } else {
            build_expr_member("location", c)
        }
    };
    coord_args.push(make_coord_arg("x"));
    coord_args.push(make_coord_arg("y"));
    if need_z {
        coord_args.push(make_coord_arg("z"));
    }

    let coord_type = match need_z {
        false => "uint2",
        true => "uint3",
    };

    let mut load_args = Vec::from([Located::none(ast::Expression::Call(
        Box::new(Located::none(ast::Expression::Identifier(
            ast::ScopedIdentifier::trivial(coord_type),
        ))),
        Vec::new(),
        coord_args,
    ))]);
    if !need_z && (!read_write || array_or_3d) {
        load_args.push(build_expr_member("location", "z"));
    }
    if !read_write && array_or_3d {
        load_args.push(build_expr_member("location", "w"));
    }

    let load_expr = Located::none(ast::Expression::Call(
        Box::new(build_expr_member(
            "texture",
            if has_status { "sparse_read" } else { "read" },
        )),
        Vec::new(),
        load_args,
    ));

    let body = if has_status {
        Vec::from([
            ast::Statement {
                kind: ast::StatementKind::Var(ast::VarDef {
                    local_type: ast::Type::from_layout(ast::TypeLayout(
                        metal_lib_identifier("sparse_color"),
                        Vec::from([ast::ExpressionOrType::Type(build_vec("T", 4))])
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

    Ok(ast::RootDefinition::Function(ast::FunctionDefinition {
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
        body: Some(body),
        attributes: Vec::new(),
    }))
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
        matches!(self.dim, Dim::Tex2DArray)
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
fn build_texture_sample(config: SampleHelper) -> Result<ast::RootDefinition, GenerateError> {
    let coord_type = match config.is_array() || config.is_3d() {
        false => "float2",
        true => "float3",
    };

    let texture_type = match config.dim {
        Dim::Tex2D => "texture2d",
        Dim::Tex2DArray => "texture2d_array",
        Dim::Tex3D => "texture3d",
    };

    let offset_type = match config.is_3d() {
        false => "int2",
        true => "int3",
    };

    let mut params = Vec::new();
    params.push(build_param(
        build_texture(texture_type, "T", false),
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
        params.push(build_status_param());
    }

    let mut coord_args = Vec::new();
    coord_args.push(build_expr_member("coord", "x"));
    coord_args.push(build_expr_member("coord", "y"));
    if config.is_3d() {
        coord_args.push(build_expr_member("coord", "z"));
    }

    let coord_type = match config.is_3d() {
        false => "float2",
        true => "float3",
    };

    let mut sample_args = Vec::new();
    sample_args.push(Located::none(ast::Expression::Identifier(
        ast::ScopedIdentifier::trivial("s"),
    )));
    sample_args.push(Located::none(ast::Expression::Call(
        Box::new(Located::none(ast::Expression::Identifier(
            ast::ScopedIdentifier::trivial(coord_type),
        ))),
        Vec::new(),
        coord_args,
    )));
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
                        Vec::from([ast::ExpressionOrType::Type(build_vec("T", 4))])
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

    Ok(ast::RootDefinition::Function(ast::FunctionDefinition {
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
        body: Some(body),
        attributes: Vec::new(),
    }))
}

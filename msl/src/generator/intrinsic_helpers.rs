use std::collections::HashSet;

use rssl_ast as ast;
use rssl_text::{Located, SourceLocation};

use super::{metal_lib_identifier, GenerateError};

#[allow(clippy::enum_variant_names)]
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum IntrinsicHelper {
    Texture2DArrayLoad,
    Texture2DArrayLoadOffset,
    Texture2DArrayLoadOffsetStatus,
}

pub fn get_intrinsic_helper_name(intrinsic: IntrinsicHelper) -> &'static str {
    match intrinsic {
        IntrinsicHelper::Texture2DArrayLoad => "Load",
        IntrinsicHelper::Texture2DArrayLoadOffset => "Load",
        IntrinsicHelper::Texture2DArrayLoadOffsetStatus => "Load",
    }
}

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

fn generate_helper(helper: IntrinsicHelper) -> Result<ast::RootDefinition, GenerateError> {
    match helper {
        IntrinsicHelper::Texture2DArrayLoad
        | IntrinsicHelper::Texture2DArrayLoadOffset
        | IntrinsicHelper::Texture2DArrayLoadOffsetStatus => {
            let has_offset = matches!(
                helper,
                IntrinsicHelper::Texture2DArrayLoadOffset
                    | IntrinsicHelper::Texture2DArrayLoadOffsetStatus
            );
            let has_status = matches!(helper, IntrinsicHelper::Texture2DArrayLoadOffsetStatus);
            let mut params = Vec::from([
                ast::FunctionParam {
                    param_type: ast::Type::from_layout(ast::TypeLayout(
                        metal_lib_identifier("texture2d_array"),
                        Vec::from([ast::ExpressionOrType::Type(ast::Type::trivial("T"))])
                            .into_boxed_slice(),
                    )),
                    declarator: ast::Declarator::Identifier(
                        ast::ScopedIdentifier::trivial("texture"),
                        Vec::new(),
                    ),
                    location_annotations: Vec::new(),
                    default_expr: None,
                },
                ast::FunctionParam {
                    param_type: ast::Type::trivial("int4"),
                    declarator: ast::Declarator::Identifier(
                        ast::ScopedIdentifier::trivial("location"),
                        Vec::new(),
                    ),
                    location_annotations: Vec::new(),
                    default_expr: None,
                },
            ]);
            if has_offset {
                params.push(ast::FunctionParam {
                    param_type: ast::Type::trivial("int2"),
                    declarator: ast::Declarator::Identifier(
                        ast::ScopedIdentifier::trivial("offset"),
                        Vec::new(),
                    ),
                    location_annotations: Vec::new(),
                    default_expr: None,
                });
            }
            if has_status {
                params.push(ast::FunctionParam {
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
                });
            }
            let load_expr = Located::none(ast::Expression::Call(
                Box::new(generate_expr_member(
                    "texture",
                    if has_status { "sparse_read" } else { "read" },
                )),
                Vec::new(),
                Vec::from([
                    Located::none(ast::Expression::Call(
                        Box::new(Located::none(ast::Expression::Identifier(
                            ast::ScopedIdentifier::trivial("uint2"),
                        ))),
                        Vec::new(),
                        Vec::from([
                            if has_offset {
                                Located::none(ast::Expression::BinaryOperation(
                                    ast::BinOp::Add,
                                    Box::new(generate_expr_member("location", "x")),
                                    Box::new(generate_expr_member("offset", "x")),
                                ))
                            } else {
                                generate_expr_member("location", "x")
                            },
                            if has_offset {
                                Located::none(ast::Expression::BinaryOperation(
                                    ast::BinOp::Add,
                                    Box::new(generate_expr_member("location", "y")),
                                    Box::new(generate_expr_member("offset", "y")),
                                ))
                            } else {
                                generate_expr_member("location", "y")
                            },
                        ]),
                    )),
                    generate_expr_member("location", "z"),
                    generate_expr_member("location", "w"),
                ]),
            ));
            let vect4 = ast::Type::from_layout(ast::TypeLayout(
                metal_lib_identifier("vec"),
                Vec::from([
                    ast::ExpressionOrType::Type(ast::Type::trivial("T")),
                    ast::ExpressionOrType::Expression(Located::none(ast::Expression::Literal(
                        ast::Literal::IntUntyped(4),
                    ))),
                ])
                .into_boxed_slice(),
            ));
            let body = if has_status {
                Vec::from([
                    ast::Statement {
                        kind: ast::StatementKind::Var(ast::VarDef {
                            local_type: ast::Type::from_layout(ast::TypeLayout(
                                metal_lib_identifier("sparse_color"),
                                Vec::from([ast::ExpressionOrType::Type(vect4.clone())])
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
                                Box::new(generate_expr_member("color", "resident")),
                                Vec::new(),
                                Vec::new(),
                            ))),
                        )),
                        location: SourceLocation::UNKNOWN,
                        attributes: Vec::new(),
                    },
                    ast::Statement {
                        kind: ast::StatementKind::Return(Some(Located::none(
                            ast::Expression::Call(
                                Box::new(generate_expr_member("color", "value")),
                                Vec::new(),
                                Vec::new(),
                            ),
                        ))),
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
                    return_type: vect4,
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
    }
}

fn generate_expr_member(object: &str, member: &str) -> Located<ast::Expression> {
    Located::none(ast::Expression::Member(
        Box::new(Located::none(ast::Expression::Identifier(
            ast::ScopedIdentifier::trivial(object),
        ))),
        ast::ScopedIdentifier::trivial(member),
    ))
}

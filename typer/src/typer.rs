use rssl_ast as ast;
use rssl_ir as ir;

mod declarations;
mod enums;
mod errors;
mod expressions;
mod functions;
mod globals;
mod pipelines;
mod scopes;
mod statements;
mod structs;
mod types;

pub use errors::{TyperError, TyperExternalError};

use errors::TyperResult;
use rssl_text::tokens::*;
use scopes::{Context, ParserSymbolResolver};
use types::{parse_type_for_usage, TypePosition};

/// Convert input into internal typed representation
pub fn parse(tokens: Vec<LexToken>) -> Result<ir::Module, TyperExternalError> {
    let mut context = Context::new();

    if let Err(err) = parse_internal(tokens, &mut context) {
        return Err(TyperExternalError(err, context));
    };

    Ok(context.module)
}

/// Internal version of parse when the context is created
fn parse_internal(tokens: Vec<LexToken>, context: &mut Context) -> TyperResult<()> {
    use rssl_parser::{Parser, ParserItem};
    let mut parser = Parser::new(tokens);

    loop {
        match parser.parse_item(&ParserSymbolResolver::new(context)) {
            Ok(ParserItem::Definition(item)) => {
                let mut def_ir = parse_rootdefinition(&item, context)?;
                assert!(context.get_next_template_params().is_none());
                context.module.root_definitions.append(&mut def_ir);
            }
            Ok(ParserItem::Template) => {
                let scope = context.push_scope();
                let mut template_params = Vec::new();
                loop {
                    match parser.parse_template_parameter(&ParserSymbolResolver::new(context)) {
                        Ok(Some(template_param)) => {
                            let param = match template_param {
                                ast::TemplateParam::Type(ty_param) => {
                                    if ty_param.default.is_some() {
                                        todo!("default template arguments not implemented");
                                    }
                                    let id = context.module.type_registry.register_template_type(
                                        ty_param.name.clone(),
                                        template_params.len() as u32,
                                    );
                                    if ty_param.name.is_some() {
                                        context.insert_template_type(id)?;
                                    }
                                    ir::TemplateParam::Type(id)
                                }
                                ast::TemplateParam::Value(ty_param) => {
                                    if ty_param.default.is_some() {
                                        todo!("default template arguments not implemented");
                                    }
                                    // TODO: Ensure allowed type modifiers are as expected
                                    let ty = parse_type_for_usage(
                                        &ty_param.value_type,
                                        TypePosition::Free,
                                        context,
                                    )?;
                                    let id = context
                                        .module
                                        .variable_registry
                                        .register_template_value(ir::TemplateParamValue {
                                            name: ty_param.name.clone(),
                                            type_id: ty,
                                            positional_index: template_params.len() as u32,
                                        });
                                    if ty_param.name.is_some() {
                                        context.insert_template_value(id)?;
                                    }
                                    ir::TemplateParam::Value(id)
                                }
                            };
                            template_params.push(param);
                        }
                        Ok(None) => break,
                        Err(err) => return Err(TyperError::ParseError(err)),
                    }
                }
                context.set_next_template_params(scope, template_params);
            }
            Ok(ParserItem::NamespaceEnter(name)) => {
                assert!(context.get_next_template_params().is_none());
                context.enter_namespace(&name)?;
            }
            Ok(ParserItem::NamespaceExit) => {
                assert!(context.get_next_template_params().is_none());
                context.exit_namespace();
            }
            Ok(ParserItem::Empty) => {
                assert!(context.get_next_template_params().is_none());
            }
            Ok(ParserItem::EndOfFile) => {
                assert!(context.is_at_root());
                assert!(context.get_next_template_params().is_none());
                return Ok(());
            }
            Err(err) => return Err(TyperError::ParseError(err)),
        }
    }
}

/// Type check a single root definition
fn parse_rootdefinition(
    ast: &ast::RootDefinition,
    context: &mut Context,
) -> TyperResult<Vec<ir::RootDefinition>> {
    match ast {
        ast::RootDefinition::Struct(ref sd) => {
            let def = structs::parse_rootdefinition_struct(sd, context)?;
            Ok(Vec::from([def]))
        }
        ast::RootDefinition::Enum(ref ed) => {
            let def = enums::parse_rootdefinition_enum(ed, context)?;
            Ok(Vec::from([def]))
        }
        ast::RootDefinition::Typedef(ref td) => {
            types::parse_rootdefinition_typedef(td, context)?;
            Ok(Vec::new())
        }
        ast::RootDefinition::ConstantBuffer(ref cb) => {
            let def = globals::parse_rootdefinition_constantbuffer(cb, context)?;
            Ok(Vec::from([def]))
        }
        ast::RootDefinition::GlobalVariable(ref gv) => {
            globals::parse_rootdefinition_globalvariable(gv, context)
        }
        ast::RootDefinition::Function(ref fd) => {
            let def = functions::parse_rootdefinition_function(fd, context)?;
            Ok(Vec::from([def]))
        }
        ast::RootDefinition::Namespace(name, contents) => {
            context.enter_namespace(name)?;
            let mut ir_defs = Vec::new();
            for ast_def in contents {
                ir_defs.extend(parse_rootdefinition(ast_def, context)?);
            }
            context.exit_namespace();
            Ok(ir_defs)
        }
        ast::RootDefinition::Pipeline(ref def) => {
            pipelines::parse_pipeline(def, context)?;
            Ok(Vec::new())
        }
    }
}

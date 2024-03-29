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
use scopes::Context;

/// Convert abstract syntax tree into internal typed representation
pub fn type_check(ast: &ast::Module) -> Result<ir::Module, TyperExternalError> {
    let mut context = Context::new();

    if let Err(err) = type_check_internal(ast, &mut context) {
        return Err(TyperExternalError(err, context));
    };

    Ok(context.module)
}

/// Internal version of type_check when the context is created
fn type_check_internal(ast: &ast::Module, context: &mut Context) -> TyperResult<()> {
    // Convert each root definition in order
    for def in &ast.root_definitions {
        let mut def_ir = parse_rootdefinition(def, context)?;
        context.module.root_definitions.append(&mut def_ir);
    }

    assert!(context.is_at_root());
    Ok(())
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

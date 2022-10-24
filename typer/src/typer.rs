use rssl_ast as ast;
use rssl_ir as ir;

mod errors;
mod expressions;
mod functions;
mod globals;
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

    match type_check_internal(ast, &mut context) {
        Ok(module) => Ok(module),
        Err(err) => Err(TyperExternalError(err, context)),
    }
}

/// Internal version of type_check when the context is created
fn type_check_internal(ast: &ast::Module, context: &mut Context) -> TyperResult<ir::Module> {
    // Convert each root definition in order
    let mut root_definitions = vec![];
    for def in &ast.root_definitions {
        let mut def_ir = parse_rootdefinition(def, context)?;
        root_definitions.append(&mut def_ir);
    }

    assert!(context.is_at_root());

    // Remember the names of non-local definitions at the module level
    let global_declarations = context.gather_global_names();

    Ok(ir::Module {
        root_definitions,
        global_declarations,
    })
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
        ast::RootDefinition::Enum(_) => todo!(),
        ast::RootDefinition::SamplerState => todo!(),
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
            context.enter_namespace(name);
            let mut ir_defs = Vec::new();
            for ast_def in contents {
                ir_defs.extend(parse_rootdefinition(ast_def, context)?);
            }
            context.exit_namespace();
            let ns = ir::RootDefinition::Namespace(name.clone(), ir_defs);
            Ok(Vec::from([ns]))
        }
    }
}

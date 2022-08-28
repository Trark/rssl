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

pub use errors::TyperError;

use errors::TyperResult;
use scopes::Context;

/// Convert abstract syntax tree into internal typed representation
pub fn type_check(ast: &ast::Module) -> TyperResult<ir::Module> {
    let mut context = Context::new();

    // Convert each root definition in order
    let mut root_definitions = vec![];
    for def in &ast.root_definitions {
        let mut def_ir = parse_rootdefinition(def, &mut context)?;
        root_definitions.append(&mut def_ir);
    }

    // Remember the function names at the module level
    let global_declarations = context.gather_global_names(&root_definitions);

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
        ast::RootDefinition::Enum(_) => unimplemented!(),
        ast::RootDefinition::SamplerState => unimplemented!(),
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
    }
}

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

    // Remember the function names at the module level
    let global_declarations = gather_global_names(&root_definitions, context);

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

/// Make a name map from a set of root definitions
fn gather_global_names(
    root_definitions: &[ir::RootDefinition],
    context: &Context,
) -> ir::GlobalDeclarations {
    use std::collections::HashMap;

    let mut decls = ir::GlobalDeclarations {
        functions: HashMap::new(),
        globals: HashMap::new(),
        structs: HashMap::new(),
        constants: HashMap::new(),
    };

    for def in root_definitions {
        match def {
            ir::RootDefinition::Struct(ref sd) => match context.get_struct_name(&sd.id) {
                Some(name) => {
                    decls.structs.insert(sd.id, name.to_string());
                }
                None => {
                    panic!("struct name does not exist");
                }
            },
            ir::RootDefinition::ConstantBuffer(ref cb) => match context.get_cbuffer_name(&cb.id) {
                Some(name) => {
                    decls.constants.insert(cb.id, name.to_string());
                }
                None => {
                    panic!("constant buffer name does not exist");
                }
            },
            ir::RootDefinition::GlobalVariable(ref gv) => match context.get_global_name(&gv.id) {
                Some(name) => {
                    decls.globals.insert(gv.id, name.to_string());
                }
                None => {
                    panic!("global variable name does not exist");
                }
            },
            ir::RootDefinition::Function(ref func) => match context.get_function_name(&func.id) {
                Some(name) => {
                    decls.functions.insert(func.id, name.to_string());
                }
                None => {
                    panic!("function name does not exist");
                }
            },
        }
    }

    decls
}

use crate::ast_enums::EnumDefinition;
use crate::ast_functions::FunctionDefinition;
use crate::ast_globals::{ConstantBuffer, GlobalVariable};
use crate::ast_structs::StructDefinition;

/// Represents a full parsed source file - which is a list of [RootDefinitions][RootDefinition].
#[derive(PartialEq, Debug, Clone)]
pub struct Module {
    pub root_definitions: Vec<RootDefinition>,
}

/// A single top level definition in an RSSL file
///
/// While this is called a "root" definition - it may be nested inside namespaces.
#[derive(PartialEq, Debug, Clone)]
pub enum RootDefinition {
    Struct(StructDefinition),
    Enum(EnumDefinition),
    ConstantBuffer(ConstantBuffer),
    GlobalVariable(GlobalVariable),
    Function(FunctionDefinition),
    Namespace(String, Vec<RootDefinition>),
}

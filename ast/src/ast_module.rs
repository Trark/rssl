use crate::ast_enums::EnumDefinition;
use crate::ast_functions::FunctionDefinition;
use crate::ast_globals::{ConstantBuffer, GlobalVariable};
use crate::ast_structs::StructDefinition;

#[derive(PartialEq, Debug, Clone)]
pub struct Module {
    pub root_definitions: Vec<RootDefinition>,
}

#[derive(PartialEq, Debug, Clone)]
pub enum RootDefinition {
    Struct(StructDefinition),
    Enum(EnumDefinition),
    SamplerState,
    ConstantBuffer(ConstantBuffer),
    GlobalVariable(GlobalVariable),
    Function(FunctionDefinition),
    Namespace(String, Vec<RootDefinition>),
}

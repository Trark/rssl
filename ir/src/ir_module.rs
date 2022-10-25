use crate::*;
use std::collections::HashMap;

#[derive(PartialEq, Debug, Clone)]
pub struct Module {
    pub global_declarations: GlobalDeclarations,
    pub root_definitions: Vec<RootDefinition>,
}

#[derive(PartialEq, Debug, Clone)]
pub enum RootDefinition {
    Struct(StructDefinition),
    StructTemplate(StructTemplateDefinition),
    ConstantBuffer(ConstantBuffer),
    GlobalVariable(GlobalVariable),
    Function(FunctionDefinition),
    Namespace(String, Vec<RootDefinition>),
}

/// Map of declarations in the global scope
#[derive(PartialEq, Debug, Clone)]
pub struct GlobalDeclarations {
    pub functions: HashMap<FunctionId, String>,
    pub globals: HashMap<GlobalId, String>,
    pub structs: HashMap<StructId, ScopedName>,
    pub struct_templates: HashMap<StructTemplateId, String>,
    pub constants: HashMap<ConstantBufferId, String>,
}

/// A name which may have namespace qualification
#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Clone)]
pub struct ScopedName(pub Vec<String>);

impl std::fmt::Display for ScopedName {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let (last, scopes) = self.0.split_last().unwrap();
        for scope in scopes {
            write!(f, "{}::", scope)?;
        }
        write!(f, "{}", last)
    }
}

impl std::fmt::Debug for ScopedName {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self)
    }
}

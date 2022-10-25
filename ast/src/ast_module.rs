use crate::ast_enums::EnumDefinition;
use crate::ast_functions::FunctionDefinition;
use crate::ast_globals::{ConstantBuffer, GlobalVariable};
use crate::ast_structs::StructDefinition;
use rssl_text::Located;

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

/// A name which may have namespace qualification
#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Clone)]
pub struct ScopedName(pub Vec<Located<String>>);

impl ScopedName {
    /// Make a name from a single part and without location information
    pub fn trivial(name: &str) -> Self {
        ScopedName(Vec::from([Located::none(name.to_string())]))
    }

    /// Remove location information from a scoped name
    pub fn unlocate(mut self) -> ScopedName {
        for name in &mut self.0 {
            name.location = rssl_text::SourceLocation::UNKNOWN;
        }
        self
    }
}

impl From<Located<&str>> for ScopedName {
    fn from(unscoped_name: Located<&str>) -> Self {
        ScopedName(Vec::from([Located::new(
            unscoped_name.node.to_string(),
            unscoped_name.location,
        )]))
    }
}

impl std::fmt::Display for ScopedName {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let (last, scopes) = self.0.split_last().unwrap();
        for scope in scopes {
            write!(f, "{:?} :: ", scope)?;
        }
        write!(f, "{:?}", last)
    }
}

impl std::fmt::Debug for ScopedName {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self)
    }
}

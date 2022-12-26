use crate::ast_statements::{Attribute, Statement, VariableBind};
use crate::ast_types::*;
use rssl_text::Located;

/// A definition for a function in RSSL
#[derive(PartialEq, Debug, Clone)]
pub struct FunctionDefinition {
    pub name: Located<String>,
    pub returntype: FunctionReturn,
    pub template_params: TemplateParamList,
    pub params: Vec<FunctionParam>,
    pub body: Vec<Statement>,
    pub attributes: Vec<Attribute>,
}

/// The type of a function return value
#[derive(PartialEq, Debug, Clone)]
pub struct FunctionReturn {
    pub return_type: Type,
    pub semantic: Option<Semantic>,
}

/// A function parameter definition
#[derive(PartialEq, Debug, Clone)]
pub struct FunctionParam {
    pub name: Located<String>,
    pub param_type: Type,
    pub bind: VariableBind,
    pub semantic: Option<Semantic>,
}

impl From<Type> for FunctionReturn {
    fn from(ty: Type) -> FunctionReturn {
        FunctionReturn {
            return_type: ty,
            semantic: None,
        }
    }
}

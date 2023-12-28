use crate::ast_declarations::{Declarator, LocationAnnotation};
use crate::ast_expressions::Expression;
use crate::ast_statements::{Attribute, Statement};
use crate::ast_types::*;
use rssl_text::Located;

/// A definition for a function in RSSL
#[derive(PartialEq, Debug, Clone)]
pub struct FunctionDefinition {
    pub name: Located<String>,
    pub returntype: FunctionReturn,
    pub template_params: TemplateParamList,
    pub params: Vec<FunctionParam>,
    pub is_const: bool,
    pub is_volatile: bool,
    pub body: Option<Vec<Statement>>,
    pub attributes: Vec<Attribute>,
}

/// The type of a function return value
#[derive(PartialEq, Debug, Clone)]
pub struct FunctionReturn {
    pub return_type: Type,
    pub location_annotations: Vec<LocationAnnotation>,
}

/// A function parameter definition
#[derive(PartialEq, Debug, Clone)]
pub struct FunctionParam {
    pub param_type: Type,
    pub declarator: Declarator,
    pub location_annotations: Vec<LocationAnnotation>,
    pub default_expr: Option<Expression>,
}

impl From<Type> for FunctionReturn {
    fn from(ty: Type) -> FunctionReturn {
        FunctionReturn {
            return_type: ty,
            location_annotations: Vec::new(),
        }
    }
}

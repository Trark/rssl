use crate::ast_expressions::*;
use crate::ast_statements::{Statement, VariableBind};
use crate::ast_types::{TemplateParamList, Type};
use crate::primitive_types::*;
use rssl_text::Located;

/// A definition for a function in RSSL
#[derive(PartialEq, Debug, Clone)]
pub struct FunctionDefinition {
    pub name: Located<String>,
    pub returntype: FunctionReturn,
    pub template_params: TemplateParamList,
    pub params: Vec<FunctionParam>,
    pub body: Vec<Statement>,
    pub attributes: Vec<FunctionAttribute>,
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
    pub param_type: ParamType,
    pub bind: VariableBind,
    pub semantic: Option<Semantic>,
}

/// The type of any parameter declaration
#[derive(PartialEq, Debug, Clone)]
pub struct ParamType(
    pub Type,
    pub InputModifier,
    pub Option<InterpolationModifier>,
);

/// An attribute that is applied to a function
#[derive(PartialEq, Debug, Clone)]
pub enum FunctionAttribute {
    NumThreads(
        Located<Expression>,
        Located<Expression>,
        Located<Expression>,
    ),
}

impl From<Type> for FunctionReturn {
    fn from(ty: Type) -> FunctionReturn {
        FunctionReturn {
            return_type: ty,
            semantic: None,
        }
    }
}

impl From<Type> for ParamType {
    fn from(ty: Type) -> ParamType {
        ParamType(ty, InputModifier::default(), None)
    }
}

impl FunctionAttribute {
    pub fn numthreads(x: u64, y: u64, z: u64) -> FunctionAttribute {
        let x_node = Located::none(Expression::Literal(Literal::UntypedInt(x)));
        let y_node = Located::none(Expression::Literal(Literal::UntypedInt(y)));
        let z_node = Located::none(Expression::Literal(Literal::UntypedInt(z)));
        FunctionAttribute::NumThreads(x_node, y_node, z_node)
    }
}

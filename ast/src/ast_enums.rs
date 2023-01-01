use crate::ast_expressions::Expression;
use rssl_text::Located;

/// A definition for an enum in RSSL
#[derive(PartialEq, Debug, Clone)]
pub struct EnumDefinition {
    pub name: Located<String>,
    pub values: Vec<EnumValue>,
}

/// A definition for a value inside an enum
#[derive(PartialEq, Debug, Clone)]
pub struct EnumValue {
    pub name: Located<String>,
    pub value: Option<Located<Expression>>,
}

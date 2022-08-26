use crate::ast_expressions::Expression;
use rssl_text::Located;

#[derive(PartialEq, Debug, Clone)]
pub struct EnumDefinition {
    pub name: String,
    pub values: Vec<EnumValue>,
}

#[derive(PartialEq, Debug, Clone)]
pub struct EnumValue {
    pub name: String,
    pub value: Option<Located<Expression>>,
}

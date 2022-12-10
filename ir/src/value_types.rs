use crate::*;

/// Value type for subexpressions. Doesn't appear in ir tree, but used for
/// reasoning about intermediates
#[derive(PartialEq, Eq, Debug, Clone)]
pub enum ValueType {
    Lvalue,
    Rvalue,
}

/// Type for value intermediates. Doesn't appear in ir tree, but used for
/// reasoning about intermediates. Doesn't include function intermediates.
#[derive(PartialEq, Eq, Debug, Clone)]
pub struct ExpressionType(pub TypeId, pub ValueType);

impl From<InputModifier> for ValueType {
    fn from(im: InputModifier) -> Self {
        match im {
            InputModifier::In => ValueType::Rvalue,
            InputModifier::InOut | InputModifier::Out => ValueType::Lvalue,
        }
    }
}

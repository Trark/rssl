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
pub struct ExpressionType(pub Type, pub ValueType);

/// Trait for turning a type into an [ExpressionType]
pub trait ToExpressionType {
    fn to_lvalue(self) -> ExpressionType;
    fn to_rvalue(self) -> ExpressionType;
}

impl ToExpressionType for Type {
    fn to_lvalue(self) -> ExpressionType {
        ExpressionType(self, ValueType::Lvalue)
    }
    fn to_rvalue(self) -> ExpressionType {
        ExpressionType(self, ValueType::Rvalue)
    }
}

impl<'a> ToExpressionType for &'a Type {
    fn to_lvalue(self) -> ExpressionType {
        self.clone().to_lvalue()
    }
    fn to_rvalue(self) -> ExpressionType {
        self.clone().to_rvalue()
    }
}

impl From<ParamType> for ExpressionType {
    fn from(ty: ParamType) -> Self {
        let vt = match ty.1 {
            InputModifier::In => ValueType::Rvalue,
            InputModifier::InOut | InputModifier::Out => ValueType::Lvalue,
        };
        ExpressionType(ty.0, vt)
    }
}

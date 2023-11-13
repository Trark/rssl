use crate::*;

/// Value type for subexpressions. Doesn't appear in ir tree, but used for
/// reasoning about intermediates
#[derive(PartialEq, Eq, Debug, Copy, Clone)]
pub enum ValueType {
    Lvalue,
    Rvalue,
}

/// Type for value intermediates. Doesn't appear in ir tree, but used for
/// reasoning about intermediates. Doesn't include function intermediates.
#[derive(PartialEq, Eq, Debug, Copy, Clone)]
pub struct ExpressionType(pub TypeId, pub ValueType);

impl From<InputModifier> for ValueType {
    fn from(im: InputModifier) -> Self {
        match im {
            InputModifier::In => ValueType::Rvalue,
            InputModifier::InOut | InputModifier::Out => ValueType::Lvalue,
        }
    }
}

/// Reduce value type of a swizzle if it uses the same slot multiple times
pub fn get_swizzle_value_type(swizzle: &Vec<SwizzleSlot>, vt: ValueType) -> ValueType {
    for i in 0..swizzle.len() {
        for j in 0..i {
            if swizzle[i] == swizzle[j] {
                return ValueType::Rvalue;
            }
        }
    }
    vt
}

/// Reduce value type of a matrix swizzle if it uses the same slot multiple times
pub fn get_matrix_swizzle_value_type(swizzle: &Vec<MatrixSwizzleSlot>, vt: ValueType) -> ValueType {
    for i in 0..swizzle.len() {
        for j in 0..i {
            if swizzle[i] == swizzle[j] {
                return ValueType::Rvalue;
            }
        }
    }
    vt
}

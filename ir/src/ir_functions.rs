use crate::*;

/// A definition for a function in RSSL
#[derive(PartialEq, Debug, Clone)]
pub struct FunctionDefinition {
    pub id: FunctionId,
    pub returntype: FunctionReturn,
    pub params: Vec<FunctionParam>,
    pub scope_block: ScopeBlock,
    pub attributes: Vec<FunctionAttribute>,
}

/// The type of a function return value
#[derive(PartialEq, Eq, Debug, Clone)]
pub struct FunctionReturn {
    pub return_type: Type,
    pub semantic: Option<Semantic>,
}

/// A function parameter definition
#[derive(PartialEq, Debug, Clone)]
#[allow(clippy::derive_partial_eq_without_eq)]
pub struct FunctionParam {
    pub id: VariableId,
    pub param_type: ParamType,
    pub semantic: Option<Semantic>,
}

/// The type of any parameter declaration
#[derive(PartialEq, Eq, Clone)]
pub struct ParamType(
    pub Type,
    pub InputModifier,
    pub Option<InterpolationModifier>,
);

/// An attribute that is applied to a function
#[derive(PartialEq, Debug, Clone)]
pub enum FunctionAttribute {
    NumThreads(Expression, Expression, Expression),
}

impl From<Type> for ParamType {
    fn from(ty: Type) -> ParamType {
        ParamType(ty, InputModifier::default(), None)
    }
}

impl std::fmt::Debug for ParamType {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match &self.2 {
            None => write!(f, "{:?} {:?}", self.1, self.0),
            Some(m) => write!(f, "{:?} {:?} {:?}", self.1, m, self.0),
        }
    }
}

impl FunctionAttribute {
    pub fn numthreads(x: u64, y: u64, z: u64) -> FunctionAttribute {
        let x_node = Expression::Literal(Literal::UntypedInt(x));
        let y_node = Expression::Literal(Literal::UntypedInt(y));
        let z_node = Expression::Literal(Literal::UntypedInt(z));
        FunctionAttribute::NumThreads(x_node, y_node, z_node)
    }
}

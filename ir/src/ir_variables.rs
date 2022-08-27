/// Id to function (in global scope)
#[derive(PartialEq, Eq, Hash, PartialOrd, Ord, Debug, Clone, Copy)]
pub struct FunctionId(pub u32);

/// Id to constant buffer
#[derive(PartialEq, Eq, Hash, PartialOrd, Ord, Debug, Clone, Copy)]
pub struct ConstantBufferId(pub u32);

/// Id to a global variable
#[derive(PartialEq, Eq, Hash, PartialOrd, Ord, Debug, Clone, Copy)]
pub struct GlobalId(pub u32);

/// Id to variable in current scope
#[derive(PartialEq, Eq, Hash, PartialOrd, Ord, Debug, Clone, Copy)]
pub struct VariableId(pub u32);

/// Number of scope levels to go up
#[derive(PartialEq, Eq, Hash, PartialOrd, Ord, Debug, Clone, Copy)]
pub struct ScopeRef(pub u32);

/// Reference to a variable, combining both id and scope level
#[derive(PartialEq, Eq, Hash, PartialOrd, Ord, Debug, Clone, Copy)]
pub struct VariableRef(pub VariableId, pub ScopeRef);

impl VariableRef {
    pub fn raw(id: u32, scope: u32) -> VariableRef {
        VariableRef(VariableId(id), ScopeRef(scope))
    }
}

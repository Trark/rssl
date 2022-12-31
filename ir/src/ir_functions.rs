use crate::*;
use rssl_text::{Located, SourceLocation};

/// Container of all registered functions
#[derive(PartialEq, Clone, Default, Debug)]
pub struct FunctionRegistry {
    signatures: Vec<FunctionSignature>,
    names: Vec<FunctionNameDefinition>,
    implementations: Vec<Option<FunctionImplementation>>,
    intrinsic_data: Vec<Option<Intrinsic>>,
    template_instantiation_data: Vec<Option<FunctionTemplateInstantiation>>,
}

/// Function name information
#[derive(PartialEq, Eq, Debug, Clone)]
pub struct FunctionNameDefinition {
    pub name: Located<String>,
    pub full_name: ScopedName,
}

/// Describes the signature for a function
#[derive(PartialEq, Eq, Debug, Clone)]
pub struct FunctionSignature {
    pub return_type: FunctionReturn,
    pub template_params: TemplateParamCount,
    pub param_types: Vec<ParamType>,
}

/// A definition for a function in RSSL
#[derive(PartialEq, Debug, Clone)]
pub struct FunctionImplementation {
    pub params: Vec<FunctionParam>,
    pub scope_block: ScopeBlock,
    pub attributes: Vec<FunctionAttribute>,
}

/// Function data specific to template instantiations
#[derive(PartialEq, Eq, Debug, Clone)]
pub struct FunctionTemplateInstantiation {
    /// A link to the parent function this was derived from
    pub parent_id: FunctionId,

    /// The template arguments used to create this instantiation
    pub template_args: Vec<TypeOrConstant>,
}

/// The type of a function return value
#[derive(PartialEq, Eq, Debug, Clone)]
pub struct FunctionReturn {
    pub return_type: TypeId,
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
    pub TypeId,
    pub InputModifier,
    pub Option<InterpolationModifier>,
    pub bool,
);

/// An attribute that is applied to a function
#[derive(PartialEq, Debug, Clone)]
pub enum FunctionAttribute {
    NumThreads(Expression, Expression, Expression),
    WaveSize(Expression),
    OutputTopology(String),
}

impl FunctionRegistry {
    /// Register a new function with the module
    pub fn register_function(
        &mut self,
        name_def: FunctionNameDefinition,
        signature: FunctionSignature,
    ) -> FunctionId {
        let id = FunctionId(self.names.len() as u32);

        // Store the signature for the function
        self.signatures.push(signature);

        // Store the names for the function
        self.names.push(name_def);

        // Default the implementation block to the missing state
        self.implementations.push(None);
        self.intrinsic_data.push(None);
        self.template_instantiation_data.push(None);

        id
    }

    /// Set the function implementation for a registered function declaration
    pub fn set_implementation(&mut self, id: FunctionId, implementation: FunctionImplementation) {
        assert_eq!(self.implementations[id.0 as usize], None);
        assert_eq!(self.intrinsic_data[id.0 as usize], None);
        self.implementations[id.0 as usize] = Some(implementation);
    }

    /// Set the function as an intrinsic
    pub fn set_intrinsic_data(&mut self, id: FunctionId, intrinsic: Intrinsic) {
        assert_eq!(self.implementations[id.0 as usize], None);
        assert_eq!(self.intrinsic_data[id.0 as usize], None);
        self.intrinsic_data[id.0 as usize] = Some(intrinsic);
    }

    /// Set the instantiation data for a function template instantiation
    pub fn set_template_instantiation_data(
        &mut self,
        id: FunctionId,
        instantiation_data: FunctionTemplateInstantiation,
    ) {
        assert_eq!(self.implementations[id.0 as usize], None);
        assert_eq!(self.template_instantiation_data[id.0 as usize], None);
        self.template_instantiation_data[id.0 as usize] = Some(instantiation_data);
    }

    /// Get the total number of registered functions
    pub fn get_function_count(&self) -> u32 {
        self.names.len() as u32
    }

    /// Get the signature from a function id
    pub fn get_function_signature(&self, id: FunctionId) -> &FunctionSignature {
        &self.signatures[id.0 as usize]
    }

    /// Get the name from a function id
    pub fn get_function_name(&self, id: FunctionId) -> &str {
        &self.names[id.0 as usize].name.node
    }

    /// Get the name definition from a function id
    pub fn get_function_name_definition(&self, id: FunctionId) -> &FunctionNameDefinition {
        &self.names[id.0 as usize]
    }

    /// Get the source location from a function id
    pub fn get_function_location(&self, id: FunctionId) -> SourceLocation {
        self.names[id.0 as usize].name.location
    }

    /// Get the implementation from a function id
    pub fn get_function_implementation(&self, id: FunctionId) -> &Option<FunctionImplementation> {
        &self.implementations[id.0 as usize]
    }

    /// Get the implementation from a function id
    pub fn get_intrinsic_data(&self, id: FunctionId) -> &Option<Intrinsic> {
        &self.intrinsic_data[id.0 as usize]
    }

    /// Get the template instantiation data from a function id
    pub fn get_template_instantiation_data(
        &self,
        id: FunctionId,
    ) -> &Option<FunctionTemplateInstantiation> {
        &self.template_instantiation_data[id.0 as usize]
    }
}

impl From<TypeId> for ParamType {
    fn from(id: TypeId) -> ParamType {
        ParamType(id, InputModifier::default(), None, false)
    }
}

impl std::fmt::Debug for ParamType {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let precise_str = if self.3 { " precise" } else { "" };
        match &self.2 {
            None => write!(f, "{:?}{} {:?}", self.1, precise_str, self.0),
            Some(m) => write!(f, "{:?}{} {:?} {:?}", self.1, precise_str, m, self.0),
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

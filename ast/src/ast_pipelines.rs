use crate::ast_expressions::Expression;
use rssl_text::Located;

/// A definition for a shader pipeline
#[derive(PartialEq, Debug, Clone)]
pub struct PipelineDefinition {
    /// The name of the pipeline
    pub name: Located<String>,

    /// The set of properties that define the pipeline
    pub properties: Vec<PipelineProperty>,
}

/// A single property describing the layout of a pipeline
#[derive(PartialEq, Debug, Clone)]
pub struct PipelineProperty {
    /// The name of a property for a pipeline
    pub property: Located<String>,

    /// The value or set of values bound
    pub value: Located<PipelinePropertyValue>,
}

/// Value bound to a property
#[derive(PartialEq, Debug, Clone)]
pub enum PipelinePropertyValue {
    /// An expression which describes a value bound to a property
    Single(Expression),
    /// A set of sub properties
    Aggregate(Vec<PipelineProperty>),
}

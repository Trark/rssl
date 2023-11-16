use crate::{
    Attribute, Expression, Initializer, PackOffset, Register, ScopedIdentifier, Semantic,
    TypeModifierSet,
};
use rssl_text::Located;

#[derive(PartialEq, Debug, Clone)]
pub struct InitDeclarator {
    pub declarator: Declarator,
    pub location_annotations: Vec<LocationAnnotation>,
    pub init: Option<Initializer>,
}

#[derive(PartialEq, Debug, Clone)]
pub enum Declarator {
    /// Represents end of declarator chain when there is no name
    Empty,

    /// Unqualified id or qualified id
    Identifier(ScopedIdentifier, Vec<Attribute>),

    /// Pointer declarator
    Pointer(PointerDeclarator),

    /// Array declarator
    Array(ArrayDeclarator),
}

#[derive(PartialEq, Debug, Clone)]
pub struct PointerDeclarator {
    pub attributes: Vec<Attribute>,
    pub qualifiers: TypeModifierSet,
    pub inner: Box<Declarator>,
}

#[derive(PartialEq, Debug, Clone)]
pub struct ArrayDeclarator {
    pub inner: Box<Declarator>,
    pub array_size: Option<Located<Expression>>,
    pub attributes: Vec<Attribute>,
}

/// Any annotation that occurs after a variable declarator that specifies how it will be linked to runtime / other shader stages
#[derive(PartialEq, Debug, Clone)]
pub enum LocationAnnotation {
    Semantic(Semantic),
    PackOffset(PackOffset),
    Register(Register),
}

impl From<Located<&str>> for Declarator {
    fn from(unscoped_name: Located<&str>) -> Self {
        Declarator::Identifier(ScopedIdentifier::from(unscoped_name), Vec::new())
    }
}

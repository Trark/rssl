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

    /// `*` declarator
    Pointer(PointerDeclarator),

    /// `&` declarator
    Reference(ReferenceDeclarator),

    /// `[]` declarator
    Array(ArrayDeclarator),
}

#[derive(PartialEq, Debug, Clone)]
pub struct PointerDeclarator {
    pub attributes: Vec<Attribute>,
    pub qualifiers: TypeModifierSet,
    pub inner: Box<Declarator>,
}

#[derive(PartialEq, Debug, Clone)]
pub struct ReferenceDeclarator {
    pub attributes: Vec<Attribute>,
    pub inner: Box<Declarator>,
}

#[derive(PartialEq, Debug, Clone)]
pub struct ArrayDeclarator {
    pub inner: Box<Declarator>,
    pub array_size: Option<Box<Located<Expression>>>,
    pub attributes: Vec<Attribute>,
}

/// Any annotation that occurs after a variable declarator that specifies how it will be linked to runtime / other shader stages
#[derive(PartialEq, Debug, Clone)]
pub enum LocationAnnotation {
    Semantic(Semantic),
    PackOffset(PackOffset),
    Register(Register),
}

impl Declarator {
    /// Return if the declarator does not have an identifier
    pub fn is_abstract(&self) -> bool {
        match self {
            Declarator::Empty => true,
            Declarator::Identifier(_, _) => false,
            Declarator::Pointer(PointerDeclarator { inner, .. }) => inner.is_abstract(),
            Declarator::Reference(ReferenceDeclarator { inner, .. }) => inner.is_abstract(),
            Declarator::Array(ArrayDeclarator { inner, .. }) => inner.is_abstract(),
        }
    }

    /// Add a declarator at the end of the chain
    pub fn insert_base<F: FnOnce(Declarator) -> Declarator>(self, f: F) -> Self {
        match self {
            Declarator::Empty | Declarator::Identifier(_, _) => f(self),
            Declarator::Pointer(PointerDeclarator {
                attributes,
                qualifiers,
                inner,
            }) => Declarator::Pointer(PointerDeclarator {
                attributes,
                qualifiers,
                inner: Box::new(inner.insert_base(f)),
            }),
            Declarator::Reference(ReferenceDeclarator { attributes, inner }) => {
                Declarator::Reference(ReferenceDeclarator {
                    attributes,
                    inner: Box::new(inner.insert_base(f)),
                })
            }
            Declarator::Array(ArrayDeclarator {
                inner,
                array_size,
                attributes,
            }) => Declarator::Array(ArrayDeclarator {
                inner: Box::new(inner.insert_base(f)),
                array_size,
                attributes,
            }),
        }
    }
}

impl From<Located<&str>> for Declarator {
    fn from(unscoped_name: Located<&str>) -> Self {
        Declarator::Identifier(ScopedIdentifier::from(unscoped_name), Vec::new())
    }
}

use crate::primitive_types::*;
use crate::ExpressionOrType;
use crate::ScopedIdentifier;
use rssl_text::{Locate, Located, SourceLocation};

/// A full type name reference
///
/// As AST does not have full type information this may trigger ambiguous parse branches in the tree.
#[derive(PartialEq, Clone)]
pub struct Type {
    pub layout: TypeLayout,
    pub modifier: TypeModifier,
    pub location: SourceLocation,
}

/// A type name reference without modifiers
#[derive(PartialEq, Clone)]
pub struct TypeLayout(pub ScopedIdentifier, pub Box<[ExpressionOrType]>);

/// Template parameters
#[derive(PartialEq, Eq, Debug, Clone)]
pub struct TemplateParamList(pub Vec<Located<String>>);

impl Type {
    /// Make a type from a single name
    pub fn trivial(name: &str) -> Self {
        Type::from_layout(TypeLayout::trivial(name))
    }

    /// Make a type from a layout and without modifiers
    pub fn from_layout(layout: TypeLayout) -> Self {
        let location = layout.0.identifiers[0].location;
        Type {
            layout,
            modifier: TypeModifier::new(),
            location,
        }
    }

    /// Make a type froom a single located name and template arguments
    pub fn with_template_types(name: Located<&str>, types: &[ExpressionOrType]) -> Self {
        Type::from_layout(TypeLayout::with_template_types(name, types))
    }

    /// Add const to the type modifier
    pub fn to_const(mut self) -> Self {
        self.modifier.is_const = true;
        self
    }
}

impl TypeLayout {
    /// Make a type layout from a single name
    pub fn trivial(name: &str) -> Self {
        TypeLayout(ScopedIdentifier::trivial(name), Default::default())
    }

    /// Make a type layout froom a single located name and template arguments
    pub fn with_template_types(name: Located<&str>, types: &[ExpressionOrType]) -> Self {
        TypeLayout(name.into(), types.to_vec().into_boxed_slice())
    }
}

impl Locate for Type {
    fn get_location(&self) -> SourceLocation {
        self.location
    }
}

impl From<&str> for Type {
    fn from(name: &str) -> Self {
        Located::none(name).into()
    }
}

impl From<Located<&str>> for Type {
    fn from(name: Located<&str>) -> Self {
        let location = name.location;
        Type {
            layout: name.into(),
            modifier: Default::default(),
            location,
        }
    }
}

impl From<&str> for TypeLayout {
    fn from(name: &str) -> Self {
        Located::none(name).into()
    }
}

impl From<Located<&str>> for TypeLayout {
    fn from(name: Located<&str>) -> Self {
        TypeLayout(name.into(), Default::default())
    }
}

impl std::fmt::Debug for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(
            f,
            "{:?}{:?} @ {:?}",
            self.modifier,
            self.layout,
            self.location.get_raw()
        )
    }
}

impl std::fmt::Display for TypeLayout {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.0)?;
        if let Some((last, non_last)) = self.1.split_last() {
            write!(f, "<")?;
            for arg in non_last {
                write!(f, "{:?}, ", arg)?;
            }
            write!(f, "{:?}", last)?;
            write!(f, ">")?;
        }
        Ok(())
    }
}

impl std::fmt::Debug for TypeLayout {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{:?}", self.0)?;
        if let Some((last, non_last)) = self.1.split_last() {
            write!(f, "<")?;
            for arg in non_last {
                write!(f, "{:?}, ", arg)?;
            }
            write!(f, "{:?}", last)?;
            write!(f, ">")?;
        }
        Ok(())
    }
}

//! # RSSL - Intermediate Representation
//!
//! The IR library contains all the definitions for the typed internal representation for RSSL.
//! The root is a [Module] instance.

pub(crate) mod intrinsic_data;
mod intrinsics;
mod ir_expressions;
mod ir_functions;
mod ir_globals;
mod ir_module;
mod ir_statements;
mod ir_structs;
mod ir_types;
mod ir_variables;

pub use rssl_ast::{
    GlobalStorage, InputModifier, InterpolationModifier, Literal, LocalStorage, PackOffset,
    PackSubOffset, RegisterType, RowOrder, Semantic, TypeModifier,
};

pub use intrinsics::*;
pub use ir_expressions::*;
pub use ir_functions::*;
pub use ir_globals::*;
pub use ir_module::*;
pub use ir_statements::*;
pub use ir_structs::*;
pub use ir_types::*;
pub use ir_variables::*;

mod value_types;
pub use value_types::*;

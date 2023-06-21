use super::errors::ErrorType;
use super::errors::{ToErrorType, TyperError, TyperResult};
use super::expressions::{UnresolvedFunction, VariableExpression};
use super::functions::{parse_function_body, ApplyTemplates};
use super::structs::build_struct_from_template;
use ir::ExpressionType;
use rssl_ast as ast;
use rssl_ir as ir;
use rssl_text::*;
use std::collections::hash_map::Entry;
use std::collections::HashMap;

/// Stores all ids for types and variables by a module
#[derive(Debug, Clone)]
pub struct Context {
    pub module: ir::Module,

    scopes: Vec<ScopeData>,
    current_scope: ScopeIndex,

    function_to_scope: HashMap<ir::FunctionId, ScopeIndex>,
    struct_template_data: Vec<StructTemplateData>,
}

pub type ScopeIndex = usize;

pub enum StructMemberValue {
    Variable(ir::TypeId),
    Method(Vec<ir::FunctionId>),
}

#[derive(Debug, Clone)]
struct StructTemplateData {
    scope: ScopeIndex,
    instantiations: HashMap<Vec<ir::TypeOrConstant>, ir::StructId>,
}

#[derive(Debug, Clone)]
struct ScopeData {
    parent_scope: usize,
    scope_name: Option<String>,

    variables: VariableBlock,

    symbols: HashMap<String, Vec<ScopeSymbol>>,

    namespace: Option<ir::NamespaceId>,
    owning_struct: Option<ir::StructId>,
    owning_enum: Option<ir::EnumId>,
    function_return_type: Option<ir::TypeId>,
}

/// Kind of any symbol that may be queried with a link to the full definition
#[derive(Debug, Clone)]
enum ScopeSymbol {
    Function(ir::FunctionId),
    Type(ir::TypeId),
    Constant(ir::Constant),
    ConstantBuffer(ir::ConstantBufferId),
    ConstantBufferMember(ir::ConstantBufferMemberId),
    GlobalVariable(ir::GlobalId),
    EnumValue(ir::EnumValueId),
    EnumValueUntyped(ir::EnumValueId),
    TemplateType(ir::TemplateTypeId),
    TemplateValue(ir::TemplateValueId),
    Namespace(ScopeIndex),
    EnumScope(ScopeIndex),
}

impl Context {
    /// Create a new instance to store type and variable context
    pub fn new() -> Self {
        let mut context = Context {
            module: ir::Module::create(),
            scopes: Vec::from([ScopeData {
                parent_scope: usize::MAX,
                scope_name: None,
                variables: VariableBlock::new(),
                symbols: HashMap::with_capacity(1024),
                namespace: None,
                owning_struct: None,
                owning_enum: None,
                function_return_type: None,
            }]),
            current_scope: 0,
            function_to_scope: HashMap::new(),
            struct_template_data: Vec::new(),
        };

        // For each builtin global value
        for i in 0..context.module.global_registry.len() {
            let id = ir::GlobalId(i as u32);
            let def = &context.module.global_registry[id.0 as usize];

            // Insert into global scope
            if context.scopes[context.current_scope]
                .symbols
                .insert(
                    def.name.to_string(),
                    Vec::from([ScopeSymbol::GlobalVariable(id)]),
                )
                .is_some()
            {
                panic!("duplicate builtin");
            }
        }

        // For each intrinsic function
        for i in 0..context.module.function_registry.get_function_count() {
            let id = ir::FunctionId(i);

            assert!(context
                .module
                .function_registry
                .get_intrinsic_data(id)
                .is_some());

            // Register the function into the root scope
            context
                .insert_function_in_scope(context.current_scope, id)
                .unwrap();
        }

        context
    }

    /// Returns if we are currently at the root scope
    pub fn is_at_root(&self) -> bool {
        self.current_scope == 0
    }

    /// Make a new scope
    fn make_scope(&mut self, parent: ScopeIndex) -> ScopeIndex {
        self.scopes.push(ScopeData {
            parent_scope: parent,
            scope_name: None,
            variables: VariableBlock::new(),
            symbols: HashMap::with_capacity(1024),
            namespace: None,
            owning_struct: None,
            owning_enum: None,
            function_return_type: None,
        });
        self.scopes.len() - 1
    }

    /// Add a new scope
    pub fn push_scope(&mut self) -> ScopeIndex {
        self.current_scope = self.make_scope(self.current_scope);
        self.current_scope
    }

    /// Add a new scope with a name
    pub fn push_scope_with_name(&mut self, name: &str) -> ScopeIndex {
        self.current_scope = self.push_scope();
        self.scopes[self.current_scope].scope_name = Some(name.to_string());
        self.current_scope
    }

    /// Leave the current scope
    pub fn pop_scope(&mut self) {
        assert!(self.scopes[self.current_scope]
            .variables
            .variables
            .is_empty());
        self.current_scope = self.scopes[self.current_scope].parent_scope;
        assert_ne!(self.current_scope, usize::MAX);
    }

    /// Leave the current scope and steal all local variable definitions out of it
    pub fn pop_scope_with_locals(&mut self) -> ir::ScopedDeclarations {
        let variables = std::mem::replace(
            &mut self.scopes[self.current_scope].variables,
            VariableBlock::new(),
        );
        let locals = ir::ScopedDeclarations {
            variables: variables.extract_locals(),
        };
        self.pop_scope();
        locals
    }

    /// Enter a scope again
    /// We must be in the same parent scope as before when calling this function
    pub fn revisit_scope(&mut self, scope: ScopeIndex) {
        assert_eq!(self.scopes[scope].parent_scope, self.current_scope);
        self.current_scope = scope
    }

    /// Enter a scope again
    /// We must be in the same parent scope as before when calling this function
    pub fn revisit_function(&mut self, id: ir::FunctionId) {
        self.revisit_scope(self.function_to_scope[&id])
    }

    /// Set the current scope to be within the given struct
    pub fn set_owning_struct(&mut self, id: ir::StructId) {
        assert_eq!(self.scopes[self.current_scope].owning_struct, None);
        self.scopes[self.current_scope].owning_struct = Some(id);
    }

    /// Get the struct we are currently scoped inside
    pub fn get_current_owning_struct(&self) -> ir::StructId {
        match self.search_scopes(|s| s.owning_struct) {
            Some(ret) => ret,
            None => panic!("Not inside struct"),
        }
    }

    /// Set the scope as a function scope with the given return type
    pub fn set_function_return_type(&mut self, return_type: ir::TypeId) {
        assert_eq!(self.scopes[self.current_scope].function_return_type, None);
        self.scopes[self.current_scope].function_return_type = Some(return_type);
    }

    /// Get the return type of the current function
    pub fn get_current_return_type(&self) -> ir::TypeId {
        match self.search_scopes(|s| s.function_return_type) {
            Some(ret) => ret,
            None => panic!("Not inside function"),
        }
    }

    /// Find a variable with a given name in the current scope
    pub fn find_identifier(
        &mut self,
        id: &ast::ScopedIdentifier,
    ) -> TyperResult<VariableExpression> {
        let (leaf_name, scopes) = id.identifiers.split_last().unwrap();

        let mut scope_index = match id.base {
            ast::ScopedIdentifierBase::Relative => self.current_scope,
            ast::ScopedIdentifierBase::Absolute => 0,
        };
        loop {
            if let Some(scope_index) = self.walk_into_scopes(scope_index, scopes) {
                let scope = &self.scopes[scope_index];

                // Try to find a matching variable in the searched scope
                if let Some(ve) = self.find_identifier_in_scope(scope, leaf_name) {
                    return Ok(ve);
                }
            }
            scope_index = self.scopes[scope_index].parent_scope;
            if scope_index == usize::MAX {
                break;
            }
        }
        Err(TyperError::UnknownIdentifier(id.clone()))
    }

    /// Find the id for a given type name
    pub fn find_type_id(
        &mut self,
        name: &ast::ScopedIdentifier,
        template_args: &[ir::TypeOrConstant],
    ) -> TyperResult<ir::TypeId> {
        let ty = match self.find_identifier(name) {
            Ok(VariableExpression::Type(ty)) => ty,
            Ok(_) => return Err(TyperError::ExpectedTypeReceivedExpression(name.clone())),
            Err(err) => return Err(err),
        };

        let (ty_unmodified, modifier) = self.module.type_registry.extract_modifier(ty);
        let tyl_unmodified = self.module.type_registry.get_type_layer(ty_unmodified);

        // Match template argument counts with type
        match tyl_unmodified {
            ir::TypeLayer::StructTemplate(id) => {
                // Templated type definitions require template arguments
                // We do not currently support default arguments
                if template_args.is_empty() {
                    // Generic error for now
                    Err(TyperError::UnknownType(name.into(), name.get_location()))
                } else {
                    let struct_template_data = &self.struct_template_data[id.0 as usize];
                    let struct_template_def = &self.module.struct_template_registry[id.0 as usize];
                    let ast = &struct_template_def.ast.clone();

                    if let Some(id) = struct_template_data.instantiations.get(template_args) {
                        let unmodified_id = self
                            .module
                            .type_registry
                            .register_type(ir::TypeLayer::Struct(*id));
                        let id = self
                            .module
                            .type_registry
                            .combine_modifier(unmodified_id, modifier);
                        Ok(id)
                    } else {
                        // Return to scope of the struct definition to build the template
                        let current_scope = self.current_scope;
                        self.current_scope = struct_template_data.scope;

                        let sid_res = build_struct_from_template(ast, template_args, self);

                        // Back to calling scope
                        self.current_scope = current_scope;

                        let sid = sid_res?;

                        // Register the instantiation
                        let struct_template_data = &mut self.struct_template_data[id.0 as usize];
                        struct_template_data
                            .instantiations
                            .insert(template_args.to_vec(), sid);

                        let unmodified_id = self
                            .module
                            .type_registry
                            .register_type(ir::TypeLayer::Struct(sid));
                        let id = self
                            .module
                            .type_registry
                            .combine_modifier(unmodified_id, modifier);
                        Ok(id)
                    }
                }
            }
            _ => {
                // Normal types do not expect any template arguments
                if template_args.is_empty() {
                    Ok(ty)
                } else {
                    // Generic error for now
                    Err(TyperError::UnknownType(name.into(), name.get_location()))
                }
            }
        }
    }

    /// Enter a chain of scopes from their names
    fn walk_into_scopes(&self, start: ScopeIndex, names: &[Located<String>]) -> Option<ScopeIndex> {
        let mut current = start;
        for scope in names {
            if let Some(symbols) = self.scopes[current].symbols.get(&scope.node) {
                for symbol in symbols {
                    // Currently only support namespaces and enums - not struct name scopes
                    match symbol {
                        ScopeSymbol::Namespace(index) => {
                            assert_eq!(current, start);
                            current = *index
                        }
                        ScopeSymbol::EnumScope(index) => {
                            assert_eq!(current, start);
                            current = *index
                        }
                        _ => {}
                    }
                }
            }
            if current == start {
                return None;
            }
        }
        Some(current)
    }

    /// Find the type of a global variable
    pub fn get_type_of_global(&self, id: ir::GlobalId) -> TyperResult<ExpressionType> {
        assert!(id.0 < self.module.global_registry.len() as u32);
        let type_id = self.module.global_registry[id.0 as usize].type_id;
        Ok(type_id.to_lvalue())
    }

    /// Find the type of a constant buffer member
    pub fn get_type_of_constant(&self, id: ir::ConstantBufferMemberId) -> ExpressionType {
        let parent_id = id.0;
        let def = &self.module.cbuffer_registry[parent_id.0 as usize].members[id.1 as usize];
        def.type_id.to_lvalue()
    }

    /// Find the type of a struct member
    /// Does not support functions
    pub fn get_type_of_struct_member(
        &self,
        id: ir::StructId,
        name: &str,
    ) -> TyperResult<ExpressionType> {
        assert!(id.0 < self.module.struct_registry.len() as u32);
        for member in &self.module.struct_registry[id.0 as usize].members {
            if member.name == name {
                return Ok(member.type_id.to_lvalue());
            }
        }

        Err(TyperError::StructMemberDoesNotExist(
            id,
            name.to_string(),
            SourceLocation::UNKNOWN,
        ))
    }

    /// Find the type of a struct member
    pub fn get_struct_member_expression(
        &self,
        id: ir::StructId,
        name: &str,
    ) -> TyperResult<StructMemberValue> {
        assert!(id.0 < self.module.struct_registry.len() as u32);

        let mut overloads = Vec::new();

        for id in &self.module.struct_registry[id.0 as usize].methods {
            let function_name = self.module.function_registry.get_function_name(*id);
            if function_name == name {
                overloads.push(*id);
            }
        }

        for member in &self.module.struct_registry[id.0 as usize].members {
            if member.name == name {
                assert!(overloads.is_empty());
                return Ok(StructMemberValue::Variable(member.type_id));
            }
        }

        if !overloads.is_empty() {
            return Ok(StructMemberValue::Method(overloads));
        }

        Err(TyperError::StructMemberDoesNotExist(
            id,
            name.to_string(),
            SourceLocation::UNKNOWN,
        ))
    }

    /// Find the signature of a function
    pub fn get_function_signature(
        &self,
        id: ir::FunctionId,
    ) -> TyperResult<&ir::FunctionSignature> {
        Ok(self.module.function_registry.get_function_signature(id))
    }

    /// Register a local variable
    pub fn insert_variable(
        &mut self,
        name: Located<String>,
        var_id: ir::VariableId,
        type_id: ir::TypeId,
    ) -> TyperResult<()> {
        self.scopes[self.current_scope].variables.insert_variable(
            name,
            var_id,
            type_id,
            &self.module,
        )
    }

    /// Register a function overload
    pub fn register_function(
        &mut self,
        name: Located<String>,
        signature: ir::FunctionSignature,
        scope: ScopeIndex,
        ast: ast::FunctionDefinition,
    ) -> TyperResult<ir::FunctionId> {
        // Find the fully qualified name based on the current scope
        let namespace = self.get_current_namespace();

        let is_template = !signature.template_params.is_empty();

        // Register the function with the module
        let id = self
            .module
            .function_registry
            .register_function(ir::FunctionNameDefinition { name, namespace }, signature);

        // Save the ast if we will need it for building template functions later
        if is_template {
            self.module.function_registry.set_template_source(id, ast);
        }

        // Set function data used by scope management
        self.function_to_scope.insert(id, scope);

        Ok(id)
    }

    /// Ensure we can insert a new function into the current scope
    pub fn check_existing_functions(
        &mut self,
        name: &Located<String>,
        signature: &ir::FunctionSignature,
        is_definition: bool,
    ) -> TyperResult<Option<ir::FunctionId>> {
        self.check_existing_functions_in_scope(self.current_scope, name, signature, is_definition)
    }

    /// Add a registered function to the active scope
    pub fn add_function_to_current_scope(&mut self, id: ir::FunctionId) -> TyperResult<()> {
        self.insert_function_in_scope(self.current_scope, id)
    }

    /// Register a new global variable
    pub fn insert_global(
        &mut self,
        name: Located<String>,
        type_id: ir::TypeId,
        storage_class: ir::GlobalStorage,
    ) -> TyperResult<ir::GlobalId> {
        let namespace = self.get_current_namespace();
        let id = ir::GlobalId(self.module.global_registry.len() as u32);
        self.module.global_registry.push(ir::GlobalVariable {
            name,
            namespace,
            type_id,
            storage_class,
            lang_slot: ir::LanguageBinding::default(),
            api_slot: None,
            init: None,
            constexpr_value: None,
            is_intrinsic: false,
        });

        let scope = &self.scopes[self.current_scope];
        let data = self.module.global_registry.last().unwrap();

        match self.find_identifier_in_scope(scope, &data.name) {
            Some(VariableExpression::Local(_, ref ty))
            | Some(VariableExpression::Global(_, ref ty))
            | Some(VariableExpression::ConstantBufferMember(_, ref ty))
            | Some(VariableExpression::EnumValue(_, ref ty)) => {
                return Err(TyperError::ValueAlreadyDefined(
                    data.name.clone(),
                    ty.to_error_type(),
                    data.type_id.to_error_type(),
                ));
            }
            _ => {}
        };

        let existing_symbols = self.scopes[self.current_scope]
            .symbols
            .entry(data.name.to_string())
            .or_default();

        for symbol in &*existing_symbols {
            if let ScopeSymbol::GlobalVariable(_) = symbol {
                unreachable!("global variable inserted multiple times");
            }
        }

        existing_symbols.push(ScopeSymbol::GlobalVariable(id));

        Ok(id)
    }

    /// Register a new struct type
    pub fn begin_struct(
        &mut self,
        name: Located<String>,
        is_non_template: bool,
    ) -> Result<ir::StructId, ir::TypeId> {
        let namespace = self.get_current_namespace();

        let id = ir::StructId(self.module.struct_registry.len() as u32);
        let type_id = self
            .module
            .type_registry
            .register_type(ir::TypeLayer::Struct(id));
        self.module.struct_registry.push(ir::StructDefinition {
            id,
            type_id,
            name,
            namespace,
            members: Default::default(),
            methods: Default::default(),
        });
        let data = self.module.struct_registry.last().unwrap();
        if is_non_template {
            let existing_symbols = self.scopes[self.current_scope]
                .symbols
                .entry(data.name.to_string())
                .or_default();

            // Check for existing symbols
            for symbol in &*existing_symbols {
                if let ScopeSymbol::Type(id) = symbol {
                    return Err(*id);
                }
            }

            existing_symbols.push(ScopeSymbol::Type(type_id));
        }
        Ok(id)
    }

    /// Register a new struct template
    pub fn register_struct_template(
        &mut self,
        name: Located<String>,
        ast: ast::StructDefinition,
    ) -> Result<ir::StructTemplateId, ir::TypeId> {
        let id = ir::StructTemplateId(self.struct_template_data.len() as u32);
        assert_eq!(
            self.struct_template_data.len(),
            self.module.struct_template_registry.len()
        );
        let type_id = self
            .module
            .type_registry
            .register_type(ir::TypeLayer::StructTemplate(id));
        let data = StructTemplateData {
            scope: self.current_scope,
            instantiations: HashMap::new(),
        };
        self.struct_template_data.push(data);
        self.module
            .struct_template_registry
            .push(ir::StructTemplateDefinition {
                id,
                type_id,
                name,
                ast,
            });
        let data = self.module.struct_template_registry.last().unwrap();
        {
            let existing_symbols = self.scopes[self.current_scope]
                .symbols
                .entry(data.name.to_string())
                .or_default();

            // Check for existing symbols
            for symbol in &*existing_symbols {
                if let ScopeSymbol::Type(id) = symbol {
                    return Err(*id);
                }
            }

            existing_symbols.push(ScopeSymbol::Type(type_id));
        }
        Ok(id)
    }

    /// Register a new enum type
    pub fn begin_enum(&mut self, name: Located<String>) -> Result<ir::EnumId, ir::TypeId> {
        let namespace = self.get_current_namespace();

        let id = self
            .module
            .enum_registry
            .register_enum(ir::EnumDefinition { name, namespace });

        let type_id = self
            .module
            .type_registry
            .register_type(ir::TypeLayer::Enum(id));

        self.module.enum_registry.set_enum_type_id(id, type_id);

        // Get name for insertion into current scope
        let name = self
            .module
            .enum_registry
            .get_enum_definition(id)
            .name
            .to_string();

        // Start the scope for the enum
        let parent_scope = self.current_scope;
        let new_scope = self.push_scope_with_name(&name);

        self.scopes[new_scope].owning_enum = Some(id);

        // Record the type and enum scope index in the parent scope
        {
            let existing_symbols = self.scopes[parent_scope]
                .symbols
                .entry(name.to_string())
                .or_default();

            // Check for existing symbols
            for symbol in &*existing_symbols {
                if let ScopeSymbol::Type(id) = symbol {
                    return Err(*id);
                }
            }

            existing_symbols.push(ScopeSymbol::EnumScope(new_scope));
            existing_symbols.push(ScopeSymbol::Type(type_id));
        }

        Ok(id)
    }

    /// Finish registering an enum type
    pub fn end_enum(&mut self) -> TyperResult<()> {
        let enum_scope = &mut self.scopes[self.current_scope];
        let parent_scope = enum_scope.parent_scope;
        let enum_id = enum_scope.owning_enum.unwrap();

        // Steal all symbols from the enum scope
        let enum_symbols = std::mem::take(&mut enum_scope.symbols);

        // Flatten enum values into an array
        let mut enum_values = Vec::new();
        for (name, symbols) in enum_symbols {
            assert_eq!(symbols.len(), 1);
            match symbols[0] {
                ScopeSymbol::EnumValueUntyped(id) => {
                    enum_values.push((name, id));
                }
                _ => unreachable!("non-enum symbol in enum scope"),
            }
        }

        // Gather value range for enum values
        // Zero is valid for all integer like types - and deduction is meant to assume a single 0 value if there are none
        let mut min_value = 0;
        let mut max_value = 0;
        for (_, enum_value_id) in &enum_values {
            let constant = &self
                .module
                .enum_registry
                .get_enum_value(*enum_value_id)
                .value;

            match *constant {
                ir::Constant::Bool(value) => {
                    min_value = std::cmp::min(min_value, value as i128);
                    max_value = std::cmp::max(max_value, value as i128);
                }
                ir::Constant::IntLiteral(value) => {
                    min_value = std::cmp::min(min_value, value);
                    max_value = std::cmp::max(max_value, value);
                }
                ir::Constant::Int32(value) => {
                    min_value = std::cmp::min(min_value, value as i128);
                    max_value = std::cmp::max(max_value, value as i128);
                }
                ir::Constant::UInt32(value) => {
                    min_value = std::cmp::min(min_value, value as i128);
                    max_value = std::cmp::max(max_value, value as i128);
                }
                ir::Constant::Int64(value) => {
                    min_value = std::cmp::min(min_value, value as i128);
                    max_value = std::cmp::max(max_value, value as i128);
                }
                ir::Constant::UInt64(value) => {
                    min_value = std::cmp::min(min_value, value as i128);
                    max_value = std::cmp::max(max_value, value as i128);
                }
                _ => panic!("invalid type inside enum value: {constant:?}"),
            }
        }

        // Select the underlying type
        // We don't try to make bool enums as they are 32-bit types anyway
        // We don't support 64-bit enums
        let scalar_type = if min_value >= i32::MIN as i128 && max_value <= i32::MAX as i128 {
            ir::ScalarType::Int32
        } else if min_value >= u32::MIN as i128 && max_value <= u32::MAX as i128 {
            ir::ScalarType::UInt32
        } else {
            let location = self
                .module
                .enum_registry
                .get_enum_definition(enum_id)
                .name
                .location;
            return Err(TyperError::EnumTypeCanNotBeDeduced(
                location, min_value, max_value,
            ));
        };

        // Set the underlying type on the enum data
        let underlying_ty = self
            .module
            .type_registry
            .register_type(ir::TypeLayer::Scalar(scalar_type));
        self.module
            .enum_registry
            .set_underlying_type_id(enum_id, underlying_ty, scalar_type);

        // Update values to be in the selected underlying type
        for (_, enum_value_id) in &enum_values {
            let constant = &self
                .module
                .enum_registry
                .get_enum_value(*enum_value_id)
                .value;

            let value = match *constant {
                ir::Constant::Bool(value) => value as i128,
                ir::Constant::IntLiteral(value) => value,
                ir::Constant::Int32(value) => value as i128,
                ir::Constant::UInt32(value) => value as i128,
                ir::Constant::Int64(value) => value as i128,
                ir::Constant::UInt64(value) => value as i128,
                _ => panic!("invalid type inside enum value: {constant:?}"),
            };

            let new_constant = match scalar_type {
                ir::ScalarType::Int32 => ir::Constant::Int32(value as i32),
                ir::ScalarType::UInt32 => ir::Constant::UInt32(value as u32),
                _ => unreachable!(),
            };

            self.module.enum_registry.update_underlying_type(
                *enum_value_id,
                new_constant,
                underlying_ty,
            );
        }

        // Promote untyped enum values to typed in parent scope
        let mut replacements = 0;
        for (name, _) in &enum_values {
            let symbols = self.scopes[parent_scope].symbols.get_mut(name).unwrap();
            assert_eq!(symbols.len(), 1);
            for symbol in symbols {
                if let ScopeSymbol::EnumValueUntyped(id) = symbol {
                    *symbol = ScopeSymbol::EnumValue(*id);
                    replacements += 1;
                }
            }
        }
        assert_eq!(replacements, enum_values.len());

        // Insert enum values back into enum scope in typed form
        for (name, id) in enum_values {
            if self.scopes[self.current_scope]
                .symbols
                .insert(name, Vec::from([ScopeSymbol::EnumValue(id)]))
                .is_some()
            {
                panic!("duplicate symbol when reinserting typed enum values");
            }
        }

        self.pop_scope();

        Ok(())
    }

    /// Register a new enum value
    pub fn register_enum_value(
        &mut self,
        enum_id: ir::EnumId,
        name: Located<String>,
        value: ir::Constant,
        value_ty: ir::TypeId,
    ) -> TyperResult<()> {
        let id =
            self.module
                .enum_registry
                .register_enum_value(enum_id, name.clone(), value, value_ty);

        let parent_scope = self.scopes[self.current_scope].parent_scope;

        // Check for existing symbols
        // Enum scope only cares about the values we are declaring in the current enum
        match self.find_identifier_in_scope(&self.scopes[self.current_scope], &name) {
            Some(VariableExpression::EnumValueUntyped(_, ref ty)) => {
                return Err(TyperError::ValueAlreadyDefined(
                    name.clone(),
                    ty.to_error_type(),
                    ErrorType::Unknown,
                ));
            }
            Some(VariableExpression::EnumValue(_, _)) => {
                panic!("Non-untyped enum value ended up in partial enum definition")
            }
            _ => {}
        };

        // The parent scope will have the majority of potential conflicts
        match self.find_identifier_in_scope(&self.scopes[parent_scope], &name) {
            Some(VariableExpression::Local(_, ref ty))
            | Some(VariableExpression::Global(_, ref ty))
            | Some(VariableExpression::ConstantBufferMember(_, ref ty))
            | Some(VariableExpression::EnumValue(_, ref ty))
            | Some(VariableExpression::Type(ref ty)) => {
                return Err(TyperError::ValueAlreadyDefined(
                    name.clone(),
                    ty.to_error_type(),
                    ErrorType::Unknown,
                ));
            }
            Some(VariableExpression::EnumValueUntyped(_, _)) => {
                panic!("Non-untyped enum value ended up in parent scope")
            }
            _ => {}
        };

        // Insert the value into the enum scope
        {
            let existing_symbols = self.scopes[self.current_scope]
                .symbols
                .entry(name.node.clone())
                .or_default();

            for symbol in &*existing_symbols {
                assert!(
                    !matches!(symbol, ScopeSymbol::EnumValueUntyped(_)),
                    "enum value inserted multiple times"
                );
            }

            existing_symbols.push(ScopeSymbol::EnumValueUntyped(id));
        }

        // Insert the value into the parent scope
        {
            let existing_symbols = self.scopes[parent_scope]
                .symbols
                .entry(name.node)
                .or_default();

            for symbol in &*existing_symbols {
                assert!(
                    !matches!(symbol, ScopeSymbol::EnumValueUntyped(_)),
                    "enum value inserted multiple times"
                );
            }

            existing_symbols.push(ScopeSymbol::EnumValueUntyped(id));
        }

        Ok(())
    }

    /// Get the namespace we are currently processing
    pub fn get_current_namespace(&self) -> Option<ir::NamespaceId> {
        self.scopes[self.current_scope].namespace
    }

    /// Register a new constant buffer
    pub fn insert_cbuffer(&mut self, id: ir::ConstantBufferId) -> TyperResult<()> {
        assert!(id.0 < self.module.cbuffer_registry.len() as u32);
        let data = &self.module.cbuffer_registry[id.0 as usize];

        let existing_symbols = self.scopes[self.current_scope]
            .symbols
            .entry(data.name.to_string())
            .or_default();

        // Check for existing symbols
        for symbol in &*existing_symbols {
            if let ScopeSymbol::ConstantBuffer(id) = symbol {
                return Err(TyperError::ConstantBufferAlreadyDefined(
                    data.name.clone(),
                    *id,
                ));
            }
        }

        existing_symbols.push(ScopeSymbol::ConstantBuffer(id));

        for (i, member) in data.members.iter().enumerate() {
            let member_id = ir::ConstantBufferMemberId(id, i as u32);

            let existing_symbols = self.scopes[self.current_scope]
                .symbols
                .entry(member.name.node.clone())
                .or_default();

            // Check for existing symbols
            for symbol in &*existing_symbols {
                if matches!(
                    symbol,
                    ScopeSymbol::Function(_)
                        | ScopeSymbol::GlobalVariable(_)
                        | ScopeSymbol::ConstantBufferMember(_)
                        | ScopeSymbol::EnumValue(_)
                        | ScopeSymbol::Namespace(_)
                ) {
                    return Err(TyperError::ValueAlreadyDefined(
                        member.name.clone(),
                        ErrorType::Unknown,
                        ErrorType::Unknown,
                    ));
                }
            }

            existing_symbols.push(ScopeSymbol::ConstantBufferMember(member_id));
        }

        Ok(())
    }

    /// Register a new typedef
    pub fn register_typedef(&mut self, name: Located<String>, ty: ir::TypeId) -> TyperResult<()> {
        let existing_symbols = self.scopes[self.current_scope]
            .symbols
            .entry(name.to_string())
            .or_default();

        // Check for existing symbols
        for symbol in &*existing_symbols {
            if let ScopeSymbol::Type(id) = symbol {
                return Err(TyperError::TypeAlreadyDefined(name, *id));
            }
        }

        existing_symbols.push(ScopeSymbol::Type(ty));
        Ok(())
    }

    /// Register a new template type parameter
    pub fn insert_template_type(&mut self, id: ir::TemplateTypeId) -> TyperResult<()> {
        let name = self
            .module
            .type_registry
            .get_template_type(id)
            .name
            .as_ref()
            .unwrap();

        let existing_symbols = self.scopes[self.current_scope]
            .symbols
            .entry(name.to_string())
            .or_default();

        // Check for existing symbols
        for symbol in &*existing_symbols {
            match symbol {
                ScopeSymbol::TemplateType(id) => {
                    return Err(TyperError::TemplateTypeAlreadyDefined(name.clone(), *id));
                }
                ScopeSymbol::TemplateValue(v) => {
                    return Err(TyperError::TemplateValueAlreadyDefined(name.clone(), *v));
                }
                _ => {}
            }
        }

        existing_symbols.push(ScopeSymbol::TemplateType(id));
        Ok(())
    }

    /// Register a new template value parameter
    pub fn insert_template_value(&mut self, id: ir::TemplateValueId) -> TyperResult<()> {
        let name = self
            .module
            .variable_registry
            .get_template_value(id)
            .name
            .as_ref()
            .unwrap();

        let existing_symbols = self.scopes[self.current_scope]
            .symbols
            .entry(name.to_string())
            .or_default();

        // Check for existing symbols
        for symbol in &*existing_symbols {
            match symbol {
                ScopeSymbol::TemplateType(id) => {
                    return Err(TyperError::TemplateTypeAlreadyDefined(name.clone(), *id));
                }
                ScopeSymbol::TemplateValue(v) => {
                    return Err(TyperError::TemplateValueAlreadyDefined(name.clone(), *v));
                }
                _ => {}
            }
        }

        existing_symbols.push(ScopeSymbol::TemplateValue(id));
        Ok(())
    }

    /// Start a namespace scope
    pub fn enter_namespace(&mut self, name: &Located<String>) -> TyperResult<()> {
        let existing_symbols = self.scopes[self.current_scope]
            .symbols
            .entry(name.to_string())
            .or_default();

        for symbol in &*existing_symbols {
            match symbol {
                ScopeSymbol::Namespace(index) => {
                    // If the namespace already exists then reopen it
                    let index = *index;
                    assert_eq!(self.scopes[index].parent_scope, self.current_scope);
                    self.current_scope = index;
                    return Ok(());
                }
                ScopeSymbol::Function(_)
                | ScopeSymbol::Type(_)
                | ScopeSymbol::GlobalVariable(_)
                | ScopeSymbol::ConstantBufferMember(_)
                | ScopeSymbol::EnumValue(_) => {
                    return Err(TyperError::ValueAlreadyDefined(
                        name.clone(),
                        ErrorType::Unknown,
                        ErrorType::Unknown,
                    ));
                }
                _ => {}
            }
        }

        // Register the new namespace with the module
        let parent_namespace = self.get_current_namespace();
        let namespace_id = self
            .module
            .namespace_registry
            .register_namespace(name.node.clone(), parent_namespace);

        // Make a new scope for the namespace
        let parent_scope = self.current_scope;
        let scope_index = self.push_scope_with_name(name);

        // Save the namespace id onto the new scope
        self.scopes[scope_index].namespace = Some(namespace_id);

        // Refetch symbols after scope modification
        let existing_symbols = self.scopes[parent_scope]
            .symbols
            .get_mut(&name.node)
            .unwrap();

        // There should still be no namespaces in the scope
        for symbol in &*existing_symbols {
            assert!(!matches!(symbol, ScopeSymbol::Namespace(_)));
        }

        existing_symbols.push(ScopeSymbol::Namespace(scope_index));
        Ok(())
    }

    // Leave the current namespace
    pub fn exit_namespace(&mut self) {
        self.pop_scope();
    }

    /// Walk up scopes and attempt to find something
    fn search_scopes<'a, Output: 'a>(
        &'a self,
        search: impl Fn(&'a ScopeData) -> Option<Output>,
    ) -> Option<Output> {
        let mut scope_index = self.current_scope;
        loop {
            if let Some(s) = search(&self.scopes[scope_index]) {
                return Some(s);
            }
            scope_index = self.scopes[scope_index].parent_scope;
            if scope_index == usize::MAX {
                break;
            }
        }
        None
    }

    fn find_identifier_in_scope(
        &self,
        scope: &ScopeData,
        name: &str,
    ) -> Option<VariableExpression> {
        if let Some(ve) = scope.variables.find_variable(name, &self.module) {
            return Some(ve);
        }

        let mut overloads = Vec::new();

        if let Some(symbols) = scope.symbols.get(name) {
            for symbol in symbols {
                debug_assert!(overloads.is_empty() || matches!(symbol, ScopeSymbol::Function(_)));
                match symbol {
                    ScopeSymbol::Function(id) => overloads.push(*id),
                    ScopeSymbol::ConstantBuffer(_) => {}
                    ScopeSymbol::ConstantBufferMember(id) => {
                        let parent_id = id.0;
                        let parent_def = &self.module.cbuffer_registry[parent_id.0 as usize];
                        let member_def = &parent_def.members[id.1 as usize];
                        return Some(VariableExpression::ConstantBufferMember(
                            *id,
                            member_def.type_id,
                        ));
                    }
                    ScopeSymbol::GlobalVariable(id) => {
                        let type_id = self.module.global_registry[id.0 as usize].type_id;
                        return Some(VariableExpression::Global(*id, type_id));
                    }
                    ScopeSymbol::EnumValueUntyped(enum_value_id) => {
                        let def = self.module.enum_registry.get_enum_value(*enum_value_id);
                        return Some(VariableExpression::EnumValueUntyped(
                            def.value.clone(),
                            def.underlying_type_id,
                        ));
                    }
                    ScopeSymbol::EnumValue(enum_value_id) => {
                        let def = self.module.enum_registry.get_enum_value(*enum_value_id);
                        return Some(VariableExpression::EnumValue(*enum_value_id, def.type_id));
                    }
                    ScopeSymbol::Type(_) => {}
                    ScopeSymbol::TemplateType(id) => {
                        let type_id = self.module.type_registry.get_template_type(*id).type_id;
                        return Some(VariableExpression::Type(type_id));
                    }
                    ScopeSymbol::TemplateValue(_) => {
                        // We do not expect to need to find template values when in value form
                        // These become named constants before parsing internals
                        unreachable!()
                    }
                    ScopeSymbol::Constant(c) => {
                        // Return a name bound to an evaluated constant value
                        return Some(VariableExpression::Constant(c.clone()));
                    }
                    ScopeSymbol::Namespace(_) => {}
                    ScopeSymbol::EnumScope(_) => {}
                }
            }
        }

        if !overloads.is_empty() {
            return Some(VariableExpression::Function(UnresolvedFunction {
                overloads,
            }));
        }

        // If the scope is for a struct then struct members are possible identifiers
        if let Some(id) = scope.owning_struct {
            match self.get_struct_member_expression(id, name) {
                Ok(StructMemberValue::Variable(ty)) => {
                    return Some(VariableExpression::Member(name.to_string(), ty));
                }
                Ok(StructMemberValue::Method(overloads)) => {
                    // Resolve as a function as the method type / value are implicit
                    return Some(VariableExpression::Method(UnresolvedFunction { overloads }));
                }
                Err(_) => {}
            }
        }

        // Check for type symbols - these are hidden by non-type symbols so are checked later
        if let Some(symbols) = scope.symbols.get(name) {
            for symbol in symbols {
                if let ScopeSymbol::Type(id) = symbol {
                    return Some(VariableExpression::Type(*id));
                }
            }
        }

        None
    }

    fn check_existing_functions_in_scope(
        &mut self,
        scope_index: usize,
        name: &Located<String>,
        signature: &ir::FunctionSignature,
        is_definition: bool,
    ) -> TyperResult<Option<ir::FunctionId>> {
        let mut declaration = None;

        // Error if a variable of the same name already exists
        match self.find_identifier_in_scope(&self.scopes[scope_index], name) {
            Some(VariableExpression::Local(_, ref ty))
            | Some(VariableExpression::Global(_, ref ty))
            | Some(VariableExpression::ConstantBufferMember(_, ref ty))
            | Some(VariableExpression::EnumValue(_, ref ty)) => {
                return Err(TyperError::ValueAlreadyDefined(
                    name.clone(),
                    ty.to_error_type(),
                    ErrorType::Unknown,
                ));
            }
            Some(VariableExpression::Function(fns)) => {
                // Fail if the overload already exists
                for existing_id in fns.overloads {
                    let existing_signature = self
                        .module
                        .function_registry
                        .get_function_signature(existing_id);
                    if existing_signature.param_types == signature.param_types
                        && check_similar_template_params(
                            &existing_signature.template_params,
                            &signature.template_params,
                            &self.module,
                        )
                    {
                        let has_impl = self
                            .module
                            .function_registry
                            .get_function_implementation(existing_id)
                            .is_some();

                        let is_intrinsic = self
                            .module
                            .function_registry
                            .get_intrinsic_data(existing_id)
                            .is_some();

                        if is_intrinsic || (is_definition && has_impl) {
                            return Err(TyperError::ValueAlreadyDefined(
                                name.clone(),
                                ErrorType::Unknown,
                                ErrorType::Unknown,
                            ));
                        } else {
                            assert_eq!(declaration, None);
                            declaration = Some(existing_id);
                        }
                    }
                }
            }
            _ => {}
        }

        Ok(declaration)
    }

    /// Add a function into the symbols for a scope
    ///
    /// Duplicate redefinitions should have already been checked
    fn insert_function_in_scope(
        &mut self,
        scope_index: usize,
        id: ir::FunctionId,
    ) -> TyperResult<()> {
        let name_data = self
            .module
            .function_registry
            .get_function_name_definition(id);

        // Add the function
        match self.scopes[scope_index]
            .symbols
            .entry(name_data.name.node.clone())
        {
            Entry::Occupied(mut occupied) => {
                // Insert a new overload
                occupied.get_mut().push(ScopeSymbol::Function(id));
            }
            Entry::Vacant(vacant) => {
                // Insert a new function with one overload
                vacant.insert(Vec::from([ScopeSymbol::Function(id)]));
            }
        };

        Ok(())
    }

    /// Construct or build the header of a template function
    pub fn build_function_template_signature(
        &mut self,
        id: ir::FunctionId,
        template_args: &[Located<ir::TypeOrConstant>],
    ) -> Option<ir::FunctionId> {
        let template_args_no_loc = template_args
            .iter()
            .map(|t| t.node.clone())
            .collect::<Vec<_>>();

        // Attempt to find an existing function instantiation
        if let Some(id) = self
            .module
            .function_registry
            .find_instantiation(id, &template_args_no_loc)
        {
            return Some(id);
        }

        // Setup the scope data based on the template function scope
        let old_scope_id = self.function_to_scope[&id];
        let parent_scope_id = self.scopes[old_scope_id].parent_scope;
        let new_scope_id = self.make_scope(parent_scope_id);

        // There should be no local variables on the template scope
        assert!(self.scopes[old_scope_id].variables.variables.is_empty());

        // None of these expected inside the function scope
        for symbols in self.scopes[old_scope_id].symbols.values() {
            for symbol in symbols {
                assert!(!matches!(symbol, ScopeSymbol::Function(_)));
                assert!(!matches!(symbol, ScopeSymbol::Type(_)));
                assert!(!matches!(symbol, ScopeSymbol::ConstantBuffer(_)));
                assert!(!matches!(symbol, ScopeSymbol::ConstantBufferMember(_)));
                assert!(!matches!(symbol, ScopeSymbol::GlobalVariable(_)));
            }
        }

        // Gather template parameters to replace with the provided types
        let mut new_symbols = Vec::new();
        for (template_param_name, symbols) in &self.scopes[old_scope_id].symbols {
            for symbol in symbols {
                match symbol {
                    ScopeSymbol::TemplateType(template_param_id) => {
                        let index = self
                            .module
                            .type_registry
                            .get_template_type(*template_param_id)
                            .positional_index;
                        match &template_args[index as usize].node {
                            ir::TypeOrConstant::Type(ty) => {
                                new_symbols
                                    .push((template_param_name.clone(), ScopeSymbol::Type(*ty)));
                            }
                            ir::TypeOrConstant::Constant(_) => return None,
                        }
                    }
                    ScopeSymbol::TemplateValue(template_param_id) => {
                        let index = self
                            .module
                            .variable_registry
                            .get_template_value(*template_param_id)
                            .positional_index;
                        match &template_args[index as usize].node {
                            ir::TypeOrConstant::Type(_) => return None,
                            ir::TypeOrConstant::Constant(c) => {
                                new_symbols.push((
                                    template_param_name.clone(),
                                    ScopeSymbol::Constant(c.clone().unrestrict()),
                                ));
                            }
                        }
                    }
                    _ => {}
                }
            }
        }

        // Insert the new symbols we just gathered
        // The new scope will not have any TemplateType or TemplateValue symbols as they are real types now
        for (name, symbol) in new_symbols {
            if self.scopes[new_scope_id]
                .symbols
                .insert(name, Vec::from([symbol]))
                .is_some()
            {
                panic!("duplicate name from template arg");
            }
        }

        // If we are a template method then the struct is the same
        self.scopes[new_scope_id].owning_struct = self.scopes[old_scope_id].owning_struct;

        // Function signature requires applying template substitution
        let base_signature = self.module.function_registry.get_function_signature(id);
        let signature = base_signature.clone().apply_templates(template_args, self);

        // Return type can be retrieved from the signature
        self.scopes[new_scope_id].function_return_type = Some(signature.return_type.return_type);

        // This should be the same as the template function scopes return type with templates applied
        {
            let active_fn_return_layout = super::types::apply_template_type_substitution(
                self.scopes[old_scope_id].function_return_type.unwrap(),
                template_args,
                self,
            );
            assert_eq!(signature.return_type.return_type, active_fn_return_layout);
        }

        // Push the instantiation as a new function
        let function_name = self
            .module
            .function_registry
            .get_function_name_definition(id);
        let new_id = self
            .module
            .function_registry
            .register_function(function_name.clone(), signature);

        self.module
            .function_registry
            .set_template_instantiation_data(
                new_id,
                ir::FunctionTemplateInstantiation {
                    parent_id: id,
                    template_args: template_args_no_loc,
                },
            );

        self.function_to_scope.insert(new_id, new_scope_id);

        Some(new_id)
    }

    /// Construct or build an instantiation of a template function
    pub fn build_function_template_body(&mut self, new_id: ir::FunctionId) -> TyperResult<()> {
        if self
            .module
            .function_registry
            .get_function_implementation(new_id)
            .is_none()
        {
            let parent_id = self
                .module
                .function_registry
                .get_template_instantiation_data(new_id)
                .as_ref()
                .unwrap()
                .parent_id;

            let parent_scope_id = self.scopes[self.function_to_scope[&new_id]].parent_scope;
            let signature = self.module.function_registry.get_function_signature(new_id);

            // Move active scope back to outside the template function definition
            let caller_scope_position = self.current_scope;
            self.current_scope = parent_scope_id;

            // Parse the function body and store it in the registry
            let ast = self
                .module
                .function_registry
                .get_template_source(parent_id)
                .clone()
                .unwrap();
            parse_function_body(&ast, new_id, signature.clone(), self)?;

            // Return active scope
            assert_eq!(self.current_scope, parent_scope_id);
            self.current_scope = caller_scope_position;
        }

        Ok(())
    }

    /// Construct or build the header of an intrinsic function
    pub fn build_intrinsic_template(
        &mut self,
        id: ir::FunctionId,
        template_args: &[Located<ir::TypeOrConstant>],
    ) -> ir::FunctionId {
        // Remove source locations from template arguments
        let template_args_no_loc = template_args
            .iter()
            .map(|t| t.node.clone())
            .collect::<Vec<_>>();

        // Attempt to find an existing intrinsic
        if let Some(id) = self
            .module
            .function_registry
            .find_instantiation(id, &template_args_no_loc)
        {
            return id;
        }

        // The name is the same as the base function
        let name = self
            .module
            .function_registry
            .get_function_name_definition(id)
            .clone();

        // The signature is obtained by applying the template parameters
        let signature = self.module.function_registry.get_function_signature(id);
        let signature = signature.clone().apply_templates(template_args, self);

        // Register the new intrinsic instantiation as a function
        let new_id = self
            .module
            .function_registry
            .register_function(name, signature);

        // Set the intrinsic data on the function to the same as the base function
        let intrinsic = self
            .module
            .function_registry
            .get_intrinsic_data(id)
            .clone()
            .unwrap();
        self.module
            .function_registry
            .set_intrinsic_data(new_id, intrinsic);

        // Set the template instantiation data
        self.module
            .function_registry
            .set_template_instantiation_data(
                new_id,
                ir::FunctionTemplateInstantiation {
                    parent_id: id,
                    template_args: template_args_no_loc,
                },
            );

        new_id
    }
}

#[derive(PartialEq, Debug, Clone)]
struct VariableBlock {
    pub variables: HashMap<String, ir::VariableId>,
}

impl VariableBlock {
    pub fn new() -> VariableBlock {
        VariableBlock {
            variables: HashMap::new(),
        }
    }

    fn insert_variable(
        &mut self,
        name: Located<String>,
        var_id: ir::VariableId,
        type_id: ir::TypeId,
        module: &ir::Module,
    ) -> TyperResult<()> {
        if let Some(id) = self.variables.get(&name.node) {
            let ty = module.variable_registry.get_local_variable(*id).type_id;
            return Err(TyperError::ValueAlreadyDefined(
                name,
                ty.to_error_type(),
                type_id.to_error_type(),
            ));
        };
        match self.variables.entry(name.node.clone()) {
            Entry::Occupied(occupied) => {
                let ty = module
                    .variable_registry
                    .get_local_variable(*occupied.get())
                    .type_id;
                Err(TyperError::ValueAlreadyDefined(
                    name,
                    ty.to_error_type(),
                    type_id.to_error_type(),
                ))
            }
            Entry::Vacant(vacant) => {
                vacant.insert(var_id);
                Ok(())
            }
        }
    }

    fn find_variable(&self, name: &str, module: &ir::Module) -> Option<VariableExpression> {
        match self.variables.get(name) {
            Some(id) => {
                let ty = module.variable_registry.get_local_variable(*id).type_id;
                Some(VariableExpression::Local(*id, ty))
            }
            None => None,
        }
    }

    fn extract_locals(self) -> Vec<ir::VariableId> {
        self.variables.iter().fold(Vec::new(), |mut vec, (_, id)| {
            vec.push(*id);
            vec
        })
    }
}

/// Check if an existing set of template parameters are a redefinition of a new set of template parameters
fn check_similar_template_params(
    left: &[ir::TemplateParam],
    right: &[ir::TemplateParam],
    module: &ir::Module,
) -> bool {
    if left.len() != right.len() {
        return false;
    }

    for (lp, rp) in left.iter().zip(right) {
        match (lp, rp) {
            // If they are both types then the slots match
            (ir::TemplateParam::Type(_), ir::TemplateParam::Type(_)) => {}
            // If they are both values then the slots match if the value type is the same
            (ir::TemplateParam::Value(lid), ir::TemplateParam::Value(rid)) => {
                let lv = module.variable_registry.get_template_value(*lid);
                let rv = module.variable_registry.get_template_value(*rid);
                if lv.type_id != rv.type_id {
                    return false;
                }
            }

            _ => return false,
        }
    }

    true
}

impl Default for Context {
    fn default() -> Self {
        Context::new()
    }
}

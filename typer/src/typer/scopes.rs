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

    function_data: HashMap<ir::FunctionId, FunctionData>,
    struct_data: Vec<StructData>,
    struct_template_data: Vec<StructTemplateData>,
    cbuffer_data: Vec<ConstantBufferData>,
}

pub type ScopeIndex = usize;

pub enum StructMemberValue {
    Variable(ir::TypeId),
    Method(Vec<ir::FunctionId>),
}

#[derive(Debug, Clone)]
struct FunctionData {
    scope: ScopeIndex,
}

#[derive(Debug, Clone)]
struct StructData {
    members: HashMap<String, ir::TypeId>,
    methods: HashMap<String, Vec<ir::FunctionId>>,
}

#[derive(Debug, Clone)]
struct StructTemplateData {
    scope: ScopeIndex,
    instantiations: HashMap<Vec<ir::TypeOrConstant>, ir::StructId>,
}

#[derive(Debug, Clone)]
struct ConstantBufferData {
    members: HashMap<String, ir::TypeId>,
}

#[derive(Debug, Clone)]
struct ScopeData {
    parent_scope: usize,
    scope_name: Option<String>,

    variables: VariableBlock,

    function_ids: HashMap<String, Vec<ir::FunctionId>>,
    types: HashMap<String, ir::TypeId>,
    cbuffer_ids: HashMap<String, ir::ConstantBufferId>,
    global_ids: HashMap<String, ir::GlobalId>,
    enum_values: HashMap<String, ir::EnumValueId>,
    untyped_enum_values: HashMap<String, ir::EnumValueId>,
    template_args: HashMap<String, (ir::TemplateTypeId, ir::TypeId)>,
    namespaces: HashMap<String, ScopeIndex>,
    enum_scopes: HashMap<String, ScopeIndex>,

    owning_struct: Option<ir::StructId>,
    owning_enum: Option<ir::EnumId>,
    function_return_type: Option<ir::TypeId>,
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
                function_ids: HashMap::with_capacity(1024),
                types: HashMap::new(),
                cbuffer_ids: HashMap::new(),
                global_ids: HashMap::new(),
                enum_values: HashMap::new(),
                untyped_enum_values: HashMap::new(),
                template_args: HashMap::new(),
                namespaces: HashMap::new(),
                enum_scopes: HashMap::new(),
                owning_struct: None,
                owning_enum: None,
                function_return_type: None,
            }]),
            current_scope: 0,
            function_data: HashMap::new(),
            struct_data: Vec::new(),
            struct_template_data: Vec::new(),
            cbuffer_data: Vec::new(),
        };

        for i in 0..context.module.function_registry.get_function_count() {
            let id = ir::FunctionId(i);

            assert!(context
                .module
                .function_registry
                .get_intrinsic_data(id)
                .is_some());

            // Register the function into the root scope
            context
                .insert_function_in_scope(context.current_scope, id, true)
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
            function_ids: HashMap::new(),
            types: HashMap::new(),
            cbuffer_ids: HashMap::new(),
            global_ids: HashMap::new(),
            enum_values: HashMap::new(),
            untyped_enum_values: HashMap::new(),
            template_args: HashMap::new(),
            namespaces: HashMap::new(),
            enum_scopes: HashMap::new(),
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
        self.revisit_scope(self.function_data[&id].scope)
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
        let mut scopes_up = 0;
        loop {
            if let Some(scope_index) = self.walk_into_scopes(scope_index, scopes) {
                let scope = &self.scopes[scope_index];

                // Try to find a matching variable in the searched scope
                if let Some(ve) = self.find_identifier_in_scope(scope, leaf_name, scopes_up) {
                    return Ok(ve);
                }
            }
            scope_index = self.scopes[scope_index].parent_scope;
            scopes_up += 1;
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
            // Currently only support namespaces and enums - not struct name scopes
            if let Some(index) = self.scopes[current].namespaces.get(&scope.node) {
                current = *index;
            } else if let Some(index) = self.scopes[current].enum_scopes.get(&scope.node) {
                current = *index;
            } else {
                return None;
            }
        }
        Some(current)
    }

    /// Find the type of a variable
    pub fn get_type_of_variable(&self, var_ref: ir::VariableRef) -> TyperResult<ExpressionType> {
        let mut scope_index = self.current_scope;
        for _ in 0..(var_ref.1 .0) {
            scope_index = self.scopes[scope_index].parent_scope;
            assert_ne!(scope_index, usize::MAX);
        }
        self.scopes[scope_index]
            .variables
            .get_type_of_variable(var_ref)
    }

    /// Find the type of a global variable
    pub fn get_type_of_global(&self, id: ir::GlobalId) -> TyperResult<ExpressionType> {
        assert!(id.0 < self.module.global_registry.len() as u32);
        let type_id = self.module.global_registry[id.0 as usize].type_id;
        Ok(type_id.to_lvalue())
    }

    /// Find the type of a constant buffer member
    pub fn get_type_of_constant(
        &self,
        id: ir::ConstantBufferId,
        name: &str,
    ) -> TyperResult<ExpressionType> {
        assert!(id.0 < self.cbuffer_data.len() as u32);
        match self.cbuffer_data[id.0 as usize].members.get(name) {
            Some(ty) => Ok(ty.to_lvalue()),
            None => Err(TyperError::ConstantDoesNotExist(
                id,
                name.to_string(),
                SourceLocation::UNKNOWN,
            )),
        }
    }

    /// Find the type of a struct member
    /// Does not support functions
    pub fn get_type_of_struct_member(
        &self,
        id: ir::StructId,
        name: &str,
    ) -> TyperResult<ExpressionType> {
        assert!(id.0 < self.struct_data.len() as u32);
        match self.struct_data[id.0 as usize].members.get(name) {
            Some(ty) => Ok(ty.to_lvalue()),
            None => Err(TyperError::StructMemberDoesNotExist(
                id,
                name.to_string(),
                SourceLocation::UNKNOWN,
            )),
        }
    }

    /// Find the type of a struct member
    pub fn get_struct_member_expression(
        &self,
        id: ir::StructId,
        name: &str,
    ) -> TyperResult<StructMemberValue> {
        assert!(id.0 < self.struct_data.len() as u32);
        if let Some(ty) = self.struct_data[id.0 as usize].members.get(name) {
            assert!(!self.struct_data[id.0 as usize].methods.contains_key(name));
            return Ok(StructMemberValue::Variable(*ty));
        }

        if let Some(methods) = self.struct_data[id.0 as usize].methods.get(name) {
            let mut overloads = Vec::with_capacity(methods.len());
            for method_id in methods {
                overloads.push(*method_id);
            }
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
        type_id: ir::TypeId,
    ) -> TyperResult<ir::VariableId> {
        self.scopes[self.current_scope]
            .variables
            .insert_variable(name, type_id)
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
        let full_name = self.get_qualified_name(&name);

        let is_template = signature.template_params.0 != 0;

        // Register the function with the module
        let id = self
            .module
            .function_registry
            .register_function(ir::FunctionNameDefinition { name, full_name }, signature);

        // Save the ast if we will need it for building template functions later
        if is_template {
            self.module.function_registry.set_template_source(id, ast);
        }

        // Set function data used by scope management
        self.function_data.insert(id, FunctionData { scope });

        Ok(id)
    }

    /// Add a registered function to the active scope
    pub fn add_function_to_current_scope(&mut self, id: ir::FunctionId) -> TyperResult<()> {
        self.insert_function_in_scope(self.current_scope, id, false)
    }

    /// Register a new global variable
    pub fn insert_global(
        &mut self,
        name: Located<String>,
        type_id: ir::TypeId,
        storage_class: ir::GlobalStorage,
    ) -> TyperResult<ir::GlobalId> {
        let full_name = self.get_qualified_name(&name);
        let id = ir::GlobalId(self.module.global_registry.len() as u32);
        self.module.global_registry.push(ir::GlobalVariable {
            id,
            name,
            full_name,
            type_id,
            storage_class,
            lang_slot: None,
            api_slot: None,
            init: None,
            constexpr_value: None,
        });

        let scope = &self.scopes[self.current_scope];
        let data = self.module.global_registry.last().unwrap();

        match self.find_identifier_in_scope(scope, &data.name, 0) {
            Some(VariableExpression::Local(_, ref ty))
            | Some(VariableExpression::Global(_, ref ty))
            | Some(VariableExpression::ConstantBufferMember(_, _, ref ty))
            | Some(VariableExpression::EnumValue(_, ref ty)) => {
                return Err(TyperError::ValueAlreadyDefined(
                    data.name.clone(),
                    ty.to_error_type(),
                    data.type_id.to_error_type(),
                ));
            }
            _ => {}
        };
        match self.scopes[self.current_scope]
            .global_ids
            .entry(data.name.node.clone())
        {
            Entry::Occupied(_) => unreachable!("global variable inserted multiple times"),
            Entry::Vacant(vacant) => {
                vacant.insert(id);
                Ok(id)
            }
        }
    }

    /// Register a new struct type
    pub fn begin_struct(
        &mut self,
        name: Located<String>,
        is_non_template: bool,
    ) -> Result<ir::StructId, ir::TypeId> {
        let full_name = self.get_qualified_name(&name);

        let id = ir::StructId(self.struct_data.len() as u32);
        assert_eq!(self.struct_data.len(), self.module.struct_registry.len());
        let type_id = self
            .module
            .type_registry
            .register_type(ir::TypeLayer::Struct(id));
        let data = StructData {
            members: HashMap::new(),
            methods: HashMap::new(),
        };
        self.struct_data.push(data);
        self.module.struct_registry.push(ir::StructDefinition {
            id,
            type_id,
            name,
            full_name,
            members: Default::default(),
            methods: Default::default(),
        });
        let data = self.module.struct_registry.last().unwrap();
        if is_non_template {
            match self.scopes[self.current_scope]
                .types
                .entry(data.name.to_string())
            {
                Entry::Vacant(v) => {
                    v.insert(type_id);
                }
                Entry::Occupied(o) => {
                    return Err(*o.get());
                }
            }
        }
        Ok(id)
    }

    // Finish setting up a struct type
    pub fn finish_struct(
        &mut self,
        id: ir::StructId,
        members: HashMap<String, ir::TypeId>,
        methods: HashMap<String, Vec<ir::FunctionId>>,
    ) {
        let data = &mut self.struct_data[id.0 as usize];
        assert!(data.members.is_empty());
        assert!(data.methods.is_empty());
        data.members = members;
        data.methods = methods;
    }

    /// Register a new struct template
    pub fn register_struct_template(
        &mut self,
        name: Located<String>,
        ast: ast::StructDefinition,
    ) -> Result<ir::StructTemplateId, ir::TypeId> {
        let id = ir::StructTemplateId(self.struct_template_data.len() as u32);
        assert_eq!(self.struct_data.len(), self.module.struct_registry.len());
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
        match self.scopes[self.current_scope]
            .types
            .entry(data.name.to_string())
        {
            Entry::Vacant(v) => {
                v.insert(type_id);
            }
            Entry::Occupied(o) => {
                return Err(*o.get());
            }
        }
        Ok(id)
    }

    /// Register a new enum type
    pub fn begin_enum(&mut self, name: Located<String>) -> Result<ir::EnumId, ir::TypeId> {
        let full_name = self.get_qualified_name(&name);

        let id = self
            .module
            .enum_registry
            .register_enum(ir::EnumDefinition { name, full_name });

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

        // Record the enum scope index in the parent scope
        self.scopes[parent_scope]
            .enum_scopes
            .insert(name.clone(), new_scope);

        // Record the type in the parent scope
        match self.scopes[parent_scope].types.entry(name) {
            Entry::Vacant(v) => {
                v.insert(type_id);
            }
            Entry::Occupied(o) => {
                return Err(*o.get());
            }
        }

        Ok(id)
    }

    /// Finish registering an enum type
    pub fn end_enum(&mut self) -> TyperResult<()> {
        let enum_scope = &mut self.scopes[self.current_scope];
        let parent_scope = enum_scope.parent_scope;
        let enum_id = enum_scope.owning_enum.unwrap();

        let mut enum_values = std::mem::take(&mut enum_scope.untyped_enum_values);

        // Gather value range for enum values
        // Zero is valid for all integer like types - and deduction is meant to assume a single 0 value if there are none
        let mut min_value = 0;
        let mut max_value = 0;
        for enum_value_id in enum_values.values() {
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
                ir::Constant::UntypedInt(value) => {
                    min_value = std::cmp::min(min_value, value);
                    max_value = std::cmp::max(max_value, value);
                }
                ir::Constant::Int(value) => {
                    min_value = std::cmp::min(min_value, value as i128);
                    max_value = std::cmp::max(max_value, value as i128);
                }
                ir::Constant::UInt(value) => {
                    min_value = std::cmp::min(min_value, value as i128);
                    max_value = std::cmp::max(max_value, value as i128);
                }
                ir::Constant::Long(value) => {
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
            ir::ScalarType::Int
        } else if min_value >= u32::MIN as i128 && max_value <= u32::MAX as i128 {
            ir::ScalarType::UInt
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
        for enum_value_id in enum_values.values() {
            let constant = &self
                .module
                .enum_registry
                .get_enum_value(*enum_value_id)
                .value;

            let value = match *constant {
                ir::Constant::Bool(value) => value as i128,
                ir::Constant::UntypedInt(value) => value,
                ir::Constant::Int(value) => value as i128,
                ir::Constant::UInt(value) => value as i128,
                ir::Constant::Long(value) => value as i128,
                _ => panic!("invalid type inside enum value: {constant:?}"),
            };

            let new_constant = match scalar_type {
                ir::ScalarType::Int => ir::Constant::Int(value as i32),
                ir::ScalarType::UInt => ir::Constant::UInt(value as u32),
                _ => unreachable!(),
            };

            self.module.enum_registry.update_underlying_type(
                *enum_value_id,
                new_constant,
                underlying_ty,
            );
        }

        // Remove untyped enum values from the parent scope
        assert_eq!(
            self.scopes[parent_scope].untyped_enum_values.len(),
            enum_values.len()
        );
        self.scopes[parent_scope].untyped_enum_values.clear();

        // Insert them again as typed values
        for (enum_name, enum_value_id) in &enum_values {
            let res = self.scopes[parent_scope]
                .enum_values
                .insert(enum_name.clone(), *enum_value_id);
            assert!(res.is_none());
        }

        // Promote untyped enum values to typed in enum scope
        let enum_scope = &mut self.scopes[self.current_scope];
        assert!(enum_scope.enum_values.is_empty());
        std::mem::swap(&mut enum_scope.enum_values, &mut enum_values);

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
        match self.find_identifier_in_scope(&self.scopes[self.current_scope], &name, 0) {
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
        match self.find_identifier_in_scope(&self.scopes[parent_scope], &name, 0) {
            Some(VariableExpression::Local(_, ref ty))
            | Some(VariableExpression::Global(_, ref ty))
            | Some(VariableExpression::ConstantBufferMember(_, _, ref ty))
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
        let res = self.scopes[self.current_scope]
            .untyped_enum_values
            .insert(name.node.clone(), id);
        assert!(res.is_none(), "enum value inserted multiple times");

        // Insert the value into the parent scope
        let res = self.scopes[parent_scope]
            .untyped_enum_values
            .insert(name.node, id);
        assert!(res.is_none(), "enum value inserted multiple times");

        Ok(())
    }

    /// Build fully qualified name
    fn get_qualified_name(&self, name: &str) -> ir::ScopedName {
        let mut full_name = Vec::from([name.to_string()]);
        let mut scope_index = self.current_scope;
        loop {
            let parent_index = self.scopes[scope_index].parent_scope;
            if let Some(s) = &self.scopes[scope_index].scope_name {
                assert_ne!(parent_index, usize::MAX);
                full_name.insert(0, s.clone());
            } else {
                assert_eq!(parent_index, usize::MAX);
            }
            scope_index = parent_index;
            if scope_index == usize::MAX {
                break;
            }
        }
        ir::ScopedName(full_name)
    }

    /// Register a new constant buffer
    pub fn insert_cbuffer(
        &mut self,
        name: Located<String>,
        members: HashMap<String, ir::TypeId>,
    ) -> Result<ir::ConstantBufferId, ir::ConstantBufferId> {
        let data = ConstantBufferData { members };
        let id = ir::ConstantBufferId(self.cbuffer_data.len() as u32);
        assert_eq!(self.cbuffer_data.len(), self.module.cbuffer_registry.len());
        self.cbuffer_data.push(data);
        self.module.cbuffer_registry.push(ir::ConstantBuffer {
            id,
            name,
            lang_binding: None,
            api_binding: None,
            members: Vec::new(),
        });
        let data = self.module.cbuffer_registry.last().unwrap();
        match self.scopes[self.current_scope]
            .cbuffer_ids
            .entry(data.name.to_string())
        {
            Entry::Vacant(id_v) => {
                id_v.insert(id);
                Ok(id)
            }
            Entry::Occupied(id_o) => Err(*id_o.get()),
        }
    }

    /// Register a new typedef
    pub fn register_typedef(&mut self, name: Located<String>, ty: ir::TypeId) -> TyperResult<()> {
        match self.scopes[self.current_scope]
            .types
            .entry(name.to_string())
        {
            Entry::Vacant(v) => {
                v.insert(ty);
                Ok(())
            }
            Entry::Occupied(o) => Err(TyperError::TypeAlreadyDefined(name, *o.get())),
        }
    }

    /// Register a new template type parameter
    pub fn insert_template_type(
        &mut self,
        name: Located<String>,
    ) -> TyperResult<ir::TemplateTypeId> {
        let current_arg_count = self.scopes[self.current_scope].template_args.len();
        match self.scopes[self.current_scope]
            .template_args
            .entry(name.to_string())
        {
            Entry::Vacant(id_v) => {
                let id = ir::TemplateTypeId(current_arg_count as u32);
                let ty_id = self
                    .module
                    .type_registry
                    .register_type(ir::TypeLayer::TemplateParam(id));
                id_v.insert((id, ty_id));
                Ok(id)
            }
            Entry::Occupied(id_o) => {
                Err(TyperError::TemplateTypeAlreadyDefined(name, id_o.get().0))
            }
        }
    }

    /// Start a namespace scope
    pub fn enter_namespace(&mut self, name: &String) {
        if let Some(index) = self.scopes[self.current_scope].namespaces.get(name) {
            // If the namespace already exists then reopen it
            assert_eq!(self.scopes[*index].parent_scope, self.current_scope);
            self.current_scope = *index;
        } else {
            // Make a new scope for the namespace
            let parent_scope = self.current_scope;
            let scope_index = self.push_scope_with_name(name);
            self.scopes[parent_scope]
                .namespaces
                .insert(name.clone(), scope_index);
        }
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
        scopes_up: u32,
    ) -> Option<VariableExpression> {
        if let Some(ve) = scope.variables.find_variable(name, scopes_up) {
            return Some(ve);
        }

        if let Some(ids) = scope.function_ids.get(name) {
            let mut overloads = Vec::with_capacity(ids.len());
            for id in ids {
                overloads.push(*id);
            }
            return Some(VariableExpression::Function(UnresolvedFunction {
                overloads,
            }));
        }

        for id in scope.cbuffer_ids.values() {
            for (member_name, ty) in &self.cbuffer_data[id.0 as usize].members {
                if member_name == name {
                    return Some(VariableExpression::ConstantBufferMember(
                        *id,
                        name.to_string(),
                        *ty,
                    ));
                }
            }
        }

        if let Some(id) = scope.global_ids.get(name) {
            let type_id = self.module.global_registry[id.0 as usize].type_id;
            return Some(VariableExpression::Global(*id, type_id));
        }

        if let Some(enum_value_id) = scope.untyped_enum_values.get(name) {
            let def = self.module.enum_registry.get_enum_value(*enum_value_id);
            return Some(VariableExpression::EnumValueUntyped(
                def.value.clone(),
                def.underlying_type_id,
            ));
        }

        if let Some(enum_value_id) = scope.enum_values.get(name) {
            let def = self.module.enum_registry.get_enum_value(*enum_value_id);
            return Some(VariableExpression::EnumValue(*enum_value_id, def.type_id));
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

        // Try to find a type name in the searched scope
        if let Some(ty) = scope.types.get(name) {
            return Some(VariableExpression::Type(*ty));
        }

        // Try to find a template type name in the searched scope
        if let Some((_, ty_id)) = scope.template_args.get(name) {
            return Some(VariableExpression::Type(*ty_id));
        }

        None
    }

    fn insert_function_in_scope(
        &mut self,
        scope_index: usize,
        id: ir::FunctionId,
        skip_checks: bool,
    ) -> TyperResult<()> {
        let name_data = self
            .module
            .function_registry
            .get_function_name_definition(id);

        if !skip_checks {
            let signature = &self.module.function_registry.get_function_signature(id);

            // Error if a variable of the same name already exists
            match self.find_identifier_in_scope(&self.scopes[scope_index], &name_data.name.node, 0)
            {
                Some(VariableExpression::Local(_, ref ty))
                | Some(VariableExpression::Global(_, ref ty))
                | Some(VariableExpression::ConstantBufferMember(_, _, ref ty))
                | Some(VariableExpression::EnumValue(_, ref ty)) => {
                    return Err(TyperError::ValueAlreadyDefined(
                        name_data.name.clone(),
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
                        if existing_signature.param_types == signature.param_types {
                            return Err(TyperError::ValueAlreadyDefined(
                                name_data.name.clone(),
                                ErrorType::Unknown,
                                ErrorType::Unknown,
                            ));
                        }
                    }
                }
                _ => {}
            }
        }

        // Try to add the function
        match self.scopes[scope_index]
            .function_ids
            .entry(name_data.name.node.clone())
        {
            Entry::Occupied(mut occupied) => {
                // Insert a new overload
                occupied.get_mut().push(id);
            }
            Entry::Vacant(vacant) => {
                // Insert a new function with one overload
                vacant.insert(Vec::from([id]));
            }
        };

        Ok(())
    }

    /// Construct or build the header of a template function
    pub fn build_function_template_signature(
        &mut self,
        id: ir::FunctionId,
        template_args: &[Located<ir::TypeOrConstant>],
    ) -> ir::FunctionId {
        let data = &self.function_data[&id];
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
            return id;
        }

        // Setup the scope data based on the template function scope
        let old_scope_id = data.scope;
        let parent_scope_id = self.scopes[old_scope_id].parent_scope;
        let new_scope_id = self.make_scope(parent_scope_id);

        // There should be no local variables on the template scope
        assert!(self.scopes[old_scope_id].variables.variables.is_empty());

        // None of these expected inside the function scope
        assert!(self.scopes[old_scope_id].function_ids.is_empty());
        assert!(self.scopes[old_scope_id].types.is_empty());
        assert!(self.scopes[old_scope_id].cbuffer_ids.is_empty());
        assert!(self.scopes[old_scope_id].global_ids.is_empty());

        // Insert template parameter names as the provided types
        // The new scopes template_args is empty as they are real types now
        for (template_param_name, (template_param_id, _)) in
            self.scopes[old_scope_id].template_args.clone()
        {
            match &template_args[template_param_id.0 as usize].node {
                ir::TypeOrConstant::Type(ty) => {
                    self.scopes[new_scope_id]
                        .types
                        .insert(template_param_name, *ty);
                }
                ir::TypeOrConstant::Constant(_) => todo!("Non-type template arguments"),
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

        self.function_data.insert(
            new_id,
            FunctionData {
                scope: new_scope_id,
            },
        );

        new_id
    }

    /// Construct or build an instantiation of a template function
    pub fn build_function_template_body(
        &mut self,
        new_id: ir::FunctionId,
    ) -> TyperResult<ir::FunctionId> {
        let template_data = &self.function_data[&new_id];

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

            let parent_scope_id = self.scopes[template_data.scope].parent_scope;
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

        Ok(new_id)
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
    pub variables: HashMap<String, (ir::TypeId, ir::VariableId)>,
    pub next_free_variable_id: ir::VariableId,
}

impl VariableBlock {
    pub fn new() -> VariableBlock {
        VariableBlock {
            variables: HashMap::new(),
            next_free_variable_id: ir::VariableId(0),
        }
    }

    fn insert_variable(
        &mut self,
        name: Located<String>,
        type_id: ir::TypeId,
    ) -> TyperResult<ir::VariableId> {
        if let Some((ty, _)) = self.variables.get(&name.node) {
            return Err(TyperError::ValueAlreadyDefined(
                name,
                ty.to_error_type(),
                type_id.to_error_type(),
            ));
        };
        match self.variables.entry(name.node.clone()) {
            Entry::Occupied(occupied) => Err(TyperError::ValueAlreadyDefined(
                name,
                occupied.get().0.to_error_type(),
                type_id.to_error_type(),
            )),
            Entry::Vacant(vacant) => {
                let id = self.next_free_variable_id;
                self.next_free_variable_id = ir::VariableId(self.next_free_variable_id.0 + 1);
                vacant.insert((type_id, id));
                Ok(id)
            }
        }
    }

    fn find_variable(&self, name: &str, scopes_up: u32) -> Option<VariableExpression> {
        match self.variables.get(name) {
            Some((ty, id)) => {
                let var = ir::VariableRef(*id, ir::ScopeRef(scopes_up));
                Some(VariableExpression::Local(var, *ty))
            }
            None => None,
        }
    }

    fn get_type_of_variable(&self, var_ref: ir::VariableRef) -> TyperResult<ExpressionType> {
        let ir::VariableRef(ref id, _) = var_ref;
        for (var_ty, var_id) in self.variables.values() {
            if id == var_id {
                return Ok(var_ty.to_lvalue());
            }
        }
        panic!("Invalid local variable id: {var_ref:?}");
    }

    fn extract_locals(self) -> HashMap<ir::VariableId, (String, ir::TypeId)> {
        self.variables
            .iter()
            .fold(HashMap::new(), |mut map, (name, (ty, id))| {
                map.insert(*id, (name.clone(), *ty));
                map
            })
    }
}

impl Default for Context {
    fn default() -> Self {
        Context::new()
    }
}

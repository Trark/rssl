use super::errors::ErrorType;
use super::errors::{ToErrorType, TyperError, TyperResult};
use super::expressions::{UnresolvedFunction, VariableExpression};
use super::functions::{parse_function_body, Callable, FunctionOverload, FunctionSignature};
use super::structs::build_struct_from_template;
use super::types::apply_template_type_substitution;
use ir::{ExpressionType, ToExpressionType};
use rssl_ast as ast;
use rssl_ir as ir;
use rssl_text::{Located, SourceLocation};
use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::rc::Rc;

/// Stores all ids for types and variables by a module
#[derive(Debug, Clone)]
pub struct Context {
    scopes: Vec<ScopeData>,
    current_scope: ScopeIndex,

    function_data: Vec<FunctionData>,
    struct_data: Vec<StructData>,
    struct_template_data: Vec<StructTemplateData>,
    cbuffer_data: Vec<ConstantBufferData>,
    global_data: Vec<GlobalData>,
}

pub type ScopeIndex = usize;

pub enum StructMemberValue {
    Variable(ir::Type),
    Method(Vec<FunctionOverload>),
}

#[derive(Debug, Clone)]
struct FunctionData {
    name: Located<String>,
    full_name: ir::ScopedName,
    overload: FunctionOverload,
    scope: ScopeIndex,
    ast: Option<Rc<ast::FunctionDefinition>>,
    instantiations: HashMap<Vec<ir::TypeOrConstant>, ir::FunctionId>,
}

#[derive(Debug, Clone)]
struct StructData {
    name: Located<String>,
    full_name: ir::ScopedName,
    members: HashMap<String, ir::Type>,
    methods: HashMap<String, Vec<ir::FunctionId>>,
}

#[derive(Debug, Clone)]
struct StructTemplateData {
    name: Located<String>,
    ast: Rc<ast::StructDefinition>,
    scope: ScopeIndex,
    instantiations: HashMap<Vec<ir::TypeOrConstant>, ir::StructId>,
}

#[derive(Debug, Clone)]
struct ConstantBufferData {
    name: Located<String>,
    members: HashMap<String, ir::Type>,
}

#[derive(Debug, Clone)]
struct GlobalData {
    name: Located<String>,
    ty: ir::Type,
}

#[derive(Debug, Clone)]
struct ScopeData {
    parent_scope: usize,
    scope_name: Option<String>,

    variables: VariableBlock,

    function_ids: HashMap<String, Vec<ir::FunctionId>>,
    types: HashMap<String, ir::Type>,
    cbuffer_ids: HashMap<String, ir::ConstantBufferId>,
    global_ids: HashMap<String, ir::GlobalId>,
    template_args: HashMap<String, ir::TemplateTypeId>,
    namespaces: HashMap<String, ScopeIndex>,

    owning_struct: Option<ir::StructId>,
    function_return_type: Option<ir::Type>,
}

impl Context {
    /// Create a new instance to store type and variable context
    pub fn new() -> Self {
        let mut context = Context {
            scopes: Vec::from([ScopeData {
                parent_scope: usize::MAX,
                scope_name: None,
                variables: VariableBlock::new(),
                function_ids: HashMap::new(),
                types: HashMap::new(),
                cbuffer_ids: HashMap::new(),
                global_ids: HashMap::new(),
                template_args: HashMap::new(),
                namespaces: HashMap::new(),
                owning_struct: None,
                function_return_type: None,
            }]),
            current_scope: 0,
            function_data: Vec::new(),
            struct_data: Vec::new(),
            struct_template_data: Vec::new(),
            cbuffer_data: Vec::new(),
            global_data: Vec::new(),
        };
        for (name, overload) in get_intrinsics() {
            context
                .insert_intrinsic(Located::none(name.clone()), overload)
                .expect("Failed to add intrinsic");
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
            template_args: HashMap::new(),
            namespaces: HashMap::new(),
            owning_struct: None,
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
        self.revisit_scope(self.function_data[id.0 as usize].scope)
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
    pub fn set_function_return_type(&mut self, return_type: ir::Type) {
        assert_eq!(self.scopes[self.current_scope].function_return_type, None);
        self.scopes[self.current_scope].function_return_type = Some(return_type);
    }

    /// Get the return type of the current function
    pub fn get_current_return_type(&self) -> ir::Type {
        match self.search_scopes(|s| s.function_return_type.clone()) {
            Some(ret) => ret,
            None => panic!("Not inside function"),
        }
    }

    /// Find a variable with a given name in the current scope
    pub fn find_identifier(&self, id: &ast::ScopedIdentifier) -> TyperResult<VariableExpression> {
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
                if let Some(ve) = self.find_variable_in_scope(scope, leaf_name, scopes_up) {
                    return Ok(ve);
                }

                // If the scope is for a struct then struct members are possible identifiers
                if let Some(id) = scope.owning_struct {
                    match self.get_struct_member_expression(id, leaf_name) {
                        Ok(StructMemberValue::Variable(ty)) => {
                            return Ok(VariableExpression::Member(leaf_name.to_string(), ty));
                        }
                        Ok(StructMemberValue::Method(overloads)) => {
                            // Resolve as a function as the method type / value are implicit
                            return Ok(VariableExpression::Method(UnresolvedFunction {
                                overloads,
                            }));
                        }
                        Err(_) => {}
                    }
                }

                // Try to find a type name in the searched scope
                if let Some(ty) = scope.types.get(&leaf_name.node) {
                    return Ok(VariableExpression::Type(ty.clone()));
                }

                // Try to find a template type name in the searched scope
                if let Some(id) = scope.template_args.get(&leaf_name.node) {
                    return Ok(VariableExpression::Type(ir::Type::from_layout(
                        ir::TypeLayout::TemplateParam(*id),
                    )));
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
    ) -> TyperResult<ir::Type> {
        let ty = match self.find_identifier(name) {
            Ok(VariableExpression::Type(ty)) => ty,
            Ok(_) => return Err(TyperError::ExpectedTypeReceivedExpression(name.clone())),
            Err(err) => return Err(err),
        };

        // Match template argument counts with type
        match ty {
            ir::Type(ir::TypeLayout::StructTemplate(id), modifier) => {
                // Templated type definitions require template arguments
                // We do not currently support default arguments
                if template_args.is_empty() {
                    // Generic error for now
                    Err(TyperError::UnknownType(name.into()))
                } else {
                    let struct_template_data = &self.struct_template_data[id.0 as usize];
                    let ast = &struct_template_data.ast.clone();

                    if let Some(id) = struct_template_data.instantiations.get(template_args) {
                        Ok(ir::Type(ir::TypeLayout::Struct(*id), modifier))
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

                        Ok(ir::Type(ir::TypeLayout::Struct(sid), modifier))
                    }
                }
            }
            _ => {
                // Normal types do not expect any template arguments
                if template_args.is_empty() {
                    Ok(ty)
                } else {
                    // Generic error for now
                    Err(TyperError::UnknownType(name.into()))
                }
            }
        }
    }

    /// Enter a chain of scopes from their names
    fn walk_into_scopes(&self, start: ScopeIndex, names: &[Located<String>]) -> Option<ScopeIndex> {
        let mut current = start;
        for scope in names {
            // Currently only support namespaces - not struct name scopes
            if let Some(index) = self.scopes[current].namespaces.get(&scope.node) {
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
        assert!(id.0 < self.global_data.len() as u32);
        Ok(self.global_data[id.0 as usize].ty.clone().to_lvalue())
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
            None => Err(TyperError::ConstantDoesNotExist(id, name.to_string())),
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
            None => Err(TyperError::StructMemberDoesNotExist(id, name.to_string())),
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
            return Ok(StructMemberValue::Variable(ty.clone()));
        }

        if let Some(methods) = self.struct_data[id.0 as usize].methods.get(name) {
            let mut overloads = Vec::with_capacity(methods.len());
            for method_id in methods {
                overloads.push(self.function_data[method_id.0 as usize].overload.clone());
            }
            return Ok(StructMemberValue::Method(overloads));
        }

        Err(TyperError::StructMemberDoesNotExist(id, name.to_string()))
    }

    /// Find the signature of a function
    pub fn get_function_signature(&self, id: ir::FunctionId) -> TyperResult<&FunctionSignature> {
        assert!(id.0 < self.function_data.len() as u32);
        Ok(&self.function_data[id.0 as usize].overload.1)
    }

    /// Find the source ast of a function
    pub fn get_function_ast(&self, id: ir::FunctionId) -> Rc<ast::FunctionDefinition> {
        assert!(id.0 < self.function_data.len() as u32);
        self.function_data[id.0 as usize]
            .ast
            .as_ref()
            .unwrap()
            .clone()
    }

    /// Find the return type of a function
    pub fn get_type_of_function_return(
        &self,
        id: ir::FunctionId,
        template_args: &[Located<ir::TypeOrConstant>],
    ) -> TyperResult<ExpressionType> {
        let signature = self.get_function_signature(id)?;
        if template_args.is_empty() {
            Ok(signature.return_type.return_type.clone().to_rvalue())
        } else {
            let return_type = apply_template_type_substitution(
                signature.return_type.return_type.clone(),
                template_args,
            );
            Ok(return_type.to_rvalue())
        }
    }

    /// Get the name from a function id
    pub fn get_function_name(&self, id: ir::FunctionId) -> &str {
        assert!(id.0 < self.function_data.len() as u32);
        &self.function_data[id.0 as usize].name.node
    }

    /// Get the name from a function
    pub fn get_function_or_intrinsic_name(&self, id: &Callable) -> &str {
        match *id {
            Callable::Function(id) => self.get_function_name(id),
            Callable::Intrinsic(_) => {
                for data in &self.function_data {
                    if data.overload.0 == *id {
                        return &data.name;
                    }
                }
                "<unknown>"
            }
        }
    }

    /// Get the name from a struct id
    pub fn get_struct_name(&self, id: ir::StructId) -> &str {
        assert!(id.0 < self.struct_data.len() as u32);
        &self.struct_data[id.0 as usize].name.node
    }

    /// Get the name from a struct template_id
    pub fn get_struct_template_name(&self, id: ir::StructTemplateId) -> &str {
        assert!(id.0 < self.struct_template_data.len() as u32);
        &self.struct_template_data[id.0 as usize].name.node
    }

    /// Get the name from a constant buffer id
    pub fn get_cbuffer_name(&self, id: ir::ConstantBufferId) -> &str {
        assert!(id.0 < self.cbuffer_data.len() as u32);
        &self.cbuffer_data[id.0 as usize].name.node
    }

    /// Get the name from a global variable id
    pub fn get_global_name(&self, id: ir::GlobalId) -> &str {
        assert!(id.0 < self.global_data.len() as u32);
        &self.global_data[id.0 as usize].name.node
    }

    /// Get the source location from a function id
    pub fn get_function_location(&self, id: ir::FunctionId) -> SourceLocation {
        assert!(id.0 < self.function_data.len() as u32);
        self.function_data[id.0 as usize].name.location
    }

    /// Get the source location from a type
    pub fn get_type_location(&self, id: &ir::Type) -> SourceLocation {
        match id.0 {
            ir::TypeLayout::Struct(id) => {
                assert!(id.0 < self.struct_data.len() as u32);
                self.struct_data[id.0 as usize].name.location
            }
            _ => SourceLocation::UNKNOWN,
        }
    }

    /// Get the source location from a constant buffer id
    pub fn get_cbuffer_location(&self, id: ir::ConstantBufferId) -> SourceLocation {
        assert!(id.0 < self.cbuffer_data.len() as u32);
        self.cbuffer_data[id.0 as usize].name.location
    }

    /// Register a local variable
    pub fn insert_variable(
        &mut self,
        name: Located<String>,
        typename: ir::Type,
    ) -> TyperResult<ir::VariableId> {
        self.scopes[self.current_scope]
            .variables
            .insert_variable(name, typename)
    }

    /// Register a function overload
    pub fn register_function(
        &mut self,
        name: Located<String>,
        signature: FunctionSignature,
        scope: ScopeIndex,
        ast: ast::FunctionDefinition,
    ) -> TyperResult<ir::FunctionId> {
        let id = ir::FunctionId(self.function_data.len() as u32);
        let full_name = self.get_qualified_name(&name);
        self.function_data.push(FunctionData {
            name,
            full_name,
            overload: FunctionOverload(Callable::Function(id), signature),
            scope,
            ast: Some(Rc::new(ast)),
            instantiations: HashMap::new(),
        });
        Ok(id)
    }

    /// Add a registered function to the active scope
    pub fn add_function_to_current_scope(&mut self, id: ir::FunctionId) -> TyperResult<()> {
        self.insert_function_in_scope(self.current_scope as usize, id)
    }

    /// Register an intrinsic
    fn insert_intrinsic(
        &mut self,
        name: Located<String>,
        overload: FunctionOverload,
    ) -> TyperResult<ir::FunctionId> {
        let id = ir::FunctionId(self.function_data.len() as u32);
        let full_name = ir::ScopedName(Vec::from([name.node.clone()]));
        self.function_data.push(FunctionData {
            name,
            full_name,
            overload,
            scope: usize::MAX,
            ast: None,
            instantiations: HashMap::new(),
        });
        self.insert_function_in_scope(self.current_scope as usize, id)?;
        Ok(id)
    }

    /// Register a new global variable
    pub fn insert_global(
        &mut self,
        name: Located<String>,
        ty: ir::Type,
    ) -> TyperResult<ir::GlobalId> {
        let data = GlobalData { name, ty };
        let id = ir::GlobalId(self.global_data.len() as u32);
        self.global_data.push(data);

        let scope = &self.scopes[self.current_scope];
        let data = self.global_data.last().unwrap();

        match self.find_variable_in_scope(scope, &data.name, 0) {
            Some(VariableExpression::Local(_, ref ty))
            | Some(VariableExpression::Global(_, ref ty))
            | Some(VariableExpression::Constant(_, _, ref ty)) => {
                return Err(TyperError::ValueAlreadyDefined(
                    data.name.clone(),
                    ty.to_error_type(),
                    data.ty.to_error_type(),
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
    ) -> Result<ir::StructId, ir::Type> {
        let full_name = self.get_qualified_name(&name);

        let data = StructData {
            name,
            full_name,
            members: HashMap::new(),
            methods: HashMap::new(),
        };
        let id = ir::StructId(self.struct_data.len() as u32);
        self.struct_data.push(data);
        let data = self.struct_data.last().unwrap();
        let type_for_struct = ir::Type::from_layout(ir::TypeLayout::Struct(id));
        if is_non_template {
            match self.scopes[self.current_scope]
                .types
                .entry(data.name.to_string())
            {
                Entry::Vacant(v) => {
                    v.insert(type_for_struct);
                }
                Entry::Occupied(o) => {
                    return Err(o.get().clone());
                }
            }
        }
        Ok(id)
    }

    // Finish setting up a struct type
    pub fn finish_struct(
        &mut self,
        id: ir::StructId,
        members: HashMap<String, ir::Type>,
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
    ) -> Result<ir::StructTemplateId, ir::Type> {
        let data = StructTemplateData {
            name,
            ast: Rc::new(ast),
            scope: self.current_scope,
            instantiations: HashMap::new(),
        };
        let id = ir::StructTemplateId(self.struct_template_data.len() as u32);
        self.struct_template_data.push(data);
        let data = self.struct_template_data.last().unwrap();
        let type_for_struct = ir::Type::from_layout(ir::TypeLayout::StructTemplate(id));
        match self.scopes[self.current_scope]
            .types
            .entry(data.name.to_string())
        {
            Entry::Vacant(v) => {
                v.insert(type_for_struct);
            }
            Entry::Occupied(o) => {
                return Err(o.get().clone());
            }
        }
        Ok(id)
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
        members: HashMap<String, ir::Type>,
    ) -> Result<ir::ConstantBufferId, ir::ConstantBufferId> {
        let data = ConstantBufferData { name, members };
        let id = ir::ConstantBufferId(self.cbuffer_data.len() as u32);
        self.cbuffer_data.push(data);
        let data = self.cbuffer_data.last().unwrap();
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
    pub fn register_typedef(&mut self, name: Located<String>, ty: ir::Type) -> TyperResult<()> {
        match self.scopes[self.current_scope]
            .types
            .entry(name.to_string())
        {
            Entry::Vacant(v) => {
                v.insert(ty);
                Ok(())
            }
            Entry::Occupied(o) => Err(TyperError::StructAlreadyDefined(name, o.get().clone())),
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
                id_v.insert(id);
                Ok(id)
            }
            Entry::Occupied(id_o) => Err(TyperError::TemplateTypeAlreadyDefined(name, *id_o.get())),
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

    fn find_variable_in_scope(
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
                overloads.push(self.function_data[id.0 as usize].overload.clone());
            }
            return Some(VariableExpression::Function(UnresolvedFunction {
                overloads,
            }));
        }

        for id in scope.cbuffer_ids.values() {
            for (member_name, ty) in &self.cbuffer_data[id.0 as usize].members {
                if member_name == name {
                    return Some(VariableExpression::Constant(
                        *id,
                        name.to_string(),
                        ty.clone(),
                    ));
                }
            }
        }

        if let Some(id) = scope.global_ids.get(name) {
            return Some(VariableExpression::Global(
                *id,
                self.global_data[id.0 as usize].ty.clone(),
            ));
        }

        None
    }

    fn insert_function_in_scope(
        &mut self,
        scope_index: usize,
        id: ir::FunctionId,
    ) -> TyperResult<()> {
        let data = &self.function_data[id.0 as usize];

        // Error if a variable of the same name already exists
        match self.find_variable_in_scope(&self.scopes[scope_index], &data.name.node, 0) {
            Some(VariableExpression::Local(_, ref ty))
            | Some(VariableExpression::Global(_, ref ty))
            | Some(VariableExpression::Constant(_, _, ref ty)) => {
                return Err(TyperError::ValueAlreadyDefined(
                    data.name.clone(),
                    ty.to_error_type(),
                    ErrorType::Unknown,
                ));
            }
            _ => {}
        };

        // Try to add the function
        match self.scopes[scope_index]
            .function_ids
            .entry(data.name.node.clone())
        {
            Entry::Occupied(mut occupied) => {
                // Fail if the overload already exists
                for existing_id in occupied.get() {
                    if self.function_data[existing_id.0 as usize]
                        .overload
                        .1
                        .param_types
                        == data.overload.1.param_types
                    {
                        return Err(TyperError::ValueAlreadyDefined(
                            data.name.clone(),
                            ErrorType::Unknown,
                            ErrorType::Unknown,
                        ));
                    }
                }
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

    /// Construct or build an instantiation of a template function
    pub fn build_function_template(
        &mut self,
        id: ir::FunctionId,
        template_args: &[Located<ir::TypeOrConstant>],
    ) -> TyperResult<ir::FunctionId> {
        let data = &self.function_data[id.0 as usize];
        let template_args_no_loc = template_args
            .iter()
            .map(|t| t.node.clone())
            .collect::<Vec<_>>();
        if let Some(id) = data.instantiations.get(&template_args_no_loc) {
            return Ok(*id);
        }

        // Setup the scope data based on the template function scope
        let old_scope_id = data.scope;
        let parent_scope_id = self.scopes[old_scope_id].parent_scope;
        let new_scope_id = self.make_scope(parent_scope_id);

        // Reborrow data after mutating scopes (false requirement in this case as function array is not touched)
        let data = &self.function_data[id.0 as usize];

        // There should be no local variables on the template scope
        assert!(self.scopes[old_scope_id].variables.variables.is_empty());

        // None of these expected inside the function scope
        assert!(self.scopes[old_scope_id].function_ids.is_empty());
        assert!(self.scopes[old_scope_id].types.is_empty());
        assert!(self.scopes[old_scope_id].cbuffer_ids.is_empty());
        assert!(self.scopes[old_scope_id].global_ids.is_empty());

        // Insert template parameter names as the provided types
        // The new scopes template_args is empty as they are real types now
        for (template_param_name, template_param_id) in
            self.scopes[old_scope_id].template_args.clone()
        {
            match &template_args[template_param_id.0 as usize].node {
                ir::TypeOrConstant::Type(ty) => {
                    self.scopes[new_scope_id]
                        .types
                        .insert(template_param_name, ty.clone());
                }
                ir::TypeOrConstant::Constant(_) => todo!("Non-type template arguments"),
            }
        }

        // If we are a template method then the struct is the same
        self.scopes[new_scope_id].owning_struct = self.scopes[old_scope_id].owning_struct;

        // Function signature requires applying template substitution
        let signature = data.overload.1.clone().apply_templates(template_args);

        // Return type can be retrieved from the signature
        self.scopes[new_scope_id].function_return_type =
            Some(signature.return_type.return_type.clone());

        // This should be the same as the template function scopes return type with templates applied
        assert_eq!(
            signature.return_type.return_type,
            super::types::apply_template_type_substitution(
                self.scopes[old_scope_id]
                    .function_return_type
                    .clone()
                    .unwrap(),
                template_args,
            )
        );

        // Push the instantiation as a new function
        let new_id = ir::FunctionId(self.function_data.len() as u32);
        self.function_data.push(FunctionData {
            name: data.name.clone(),
            full_name: data.full_name.clone(),
            overload: FunctionOverload(Callable::Function(new_id), signature.clone()),
            scope: new_scope_id,
            ast: None,
            instantiations: HashMap::new(),
        });

        // Move active scope back to outside the template function definition
        let caller_scope_position = self.current_scope;
        self.current_scope = parent_scope_id;

        // Parse the function body - but currently we do not store the result anywhere
        let ast = self.get_function_ast(id);
        parse_function_body(&ast, new_id, signature, self)?;

        // Return active scope
        assert_eq!(self.current_scope, parent_scope_id);
        self.current_scope = caller_scope_position;

        // Save the new function in the instantiations map and return
        let data = &mut self.function_data[id.0 as usize];
        data.instantiations.insert(template_args_no_loc, new_id);
        Ok(new_id)
    }

    /// Make a name map from all non-local ids
    pub fn gather_global_names(&self) -> ir::GlobalDeclarations {
        let mut decls = ir::GlobalDeclarations {
            functions: HashMap::new(),
            globals: HashMap::new(),
            structs: HashMap::new(),
            struct_templates: HashMap::new(),
            constants: HashMap::new(),
        };

        for struct_index in 0..self.struct_data.len() {
            let id = ir::StructId(struct_index as u32);
            let data = &self.struct_data[struct_index];
            decls.structs.insert(id, data.full_name.clone());
        }

        for struct_template_index in 0..self.struct_template_data.len() {
            let id = ir::StructTemplateId(struct_template_index as u32);
            let data = &self.struct_template_data[struct_template_index];
            decls.struct_templates.insert(id, data.name.to_string());
        }

        for cbuffer_index in 0..self.cbuffer_data.len() {
            let id = ir::ConstantBufferId(cbuffer_index as u32);
            let data = &self.cbuffer_data[cbuffer_index];
            decls.constants.insert(id, data.name.to_string());
        }

        for global_index in 0..self.global_data.len() {
            let id = ir::GlobalId(global_index as u32);
            let data = &self.global_data[global_index];
            decls.globals.insert(id, data.name.to_string());
        }

        for function_index in 0..self.function_data.len() {
            let id = ir::FunctionId(function_index as u32);
            let data = &self.function_data[function_index];
            decls.functions.insert(id, data.full_name.clone());
        }

        decls
    }
}

#[derive(PartialEq, Debug, Clone)]
struct VariableBlock {
    pub variables: HashMap<String, (ir::Type, ir::VariableId)>,
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
        typename: ir::Type,
    ) -> TyperResult<ir::VariableId> {
        if let Some(&(ref ty, _)) = self.variables.get(&name.node) {
            return Err(TyperError::ValueAlreadyDefined(
                name,
                ty.to_error_type(),
                typename.to_error_type(),
            ));
        };
        match self.variables.entry(name.node.clone()) {
            Entry::Occupied(occupied) => Err(TyperError::ValueAlreadyDefined(
                name,
                occupied.get().0.to_error_type(),
                typename.to_error_type(),
            )),
            Entry::Vacant(vacant) => {
                let id = self.next_free_variable_id;
                self.next_free_variable_id = ir::VariableId(self.next_free_variable_id.0 + 1);
                vacant.insert((typename, id));
                Ok(id)
            }
        }
    }

    fn find_variable(&self, name: &str, scopes_up: u32) -> Option<VariableExpression> {
        match self.variables.get(name) {
            Some(&(ref ty, ref id)) => {
                let var = ir::VariableRef(*id, ir::ScopeRef(scopes_up));
                Some(VariableExpression::Local(var, ty.clone()))
            }
            None => None,
        }
    }

    fn get_type_of_variable(&self, var_ref: ir::VariableRef) -> TyperResult<ExpressionType> {
        let ir::VariableRef(ref id, _) = var_ref;
        for &(ref var_ty, ref var_id) in self.variables.values() {
            if id == var_id {
                return Ok(var_ty.to_lvalue());
            }
        }
        panic!("Invalid local variable id: {:?}", var_ref);
    }

    fn extract_locals(self) -> HashMap<ir::VariableId, (String, ir::Type)> {
        self.variables
            .iter()
            .fold(HashMap::new(), |mut map, (name, &(ref ty, ref id))| {
                map.insert(*id, (name.clone(), ty.clone()));
                map
            })
    }
}

/// Create a map of all the intrinsic functions we need to parse
fn get_intrinsics() -> Vec<(String, FunctionOverload)> {
    use crate::intrinsics::*;
    let funcs = get_intrinsics();

    let mut overloads = Vec::with_capacity(funcs.len());
    for &(ref name, ref intrinsic, params) in funcs {
        let factory = IntrinsicFactory::Function(intrinsic.clone(), params);
        let return_type = ir::FunctionReturn {
            return_type: factory.get_return_type().0,
            semantic: None,
        };
        let param_types = params.to_vec();
        let overload = FunctionOverload(
            Callable::Intrinsic(factory.clone()),
            FunctionSignature {
                return_type,
                template_params: ir::TemplateParamCount(0),
                param_types,
            },
        );
        overloads.push((name.to_string(), overload));
    }
    overloads
}

impl Default for Context {
    fn default() -> Self {
        Context::new()
    }
}

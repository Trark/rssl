use super::errors::{ToErrorType, TyperError, TyperResult};
use super::expressions::{UnresolvedFunction, VariableExpression};
use crate::typer::errors::ErrorType;
use crate::typer::functions::{FunctionName, FunctionOverload};
use ir::{ExpressionType, ToExpressionType};
use rssl_ast as ast;
use rssl_ir as ir;
use rssl_text::{Located, SourceLocation};
use std::collections::hash_map::Entry;
use std::collections::HashMap;

/// Stores all ids for types and variables by a module
#[derive(Debug, Clone)]
pub struct Context {
    scopes: Vec<ScopeData>,
    current_scope: usize,

    function_data: Vec<FunctionData>,
    struct_data: Vec<StructData>,
    cbuffer_data: Vec<ConstantBufferData>,
    global_data: Vec<GlobalData>,
}

#[derive(Debug, Clone)]
struct FunctionData {
    name: Located<String>,
    overload: FunctionOverload,
}

#[derive(Debug, Clone)]
struct StructData {
    name: Located<String>,
    members: HashMap<String, ir::Type>,
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

    variables: VariableBlock,

    function_ids: HashMap<String, Vec<ir::FunctionId>>,
    struct_ids: HashMap<String, ir::StructId>,
    cbuffer_ids: HashMap<String, ir::ConstantBufferId>,
    global_ids: HashMap<String, ir::GlobalId>,
    template_args: HashMap<String, ir::TemplateTypeId>,

    function_return_type: Option<ir::Type>,
}

impl Context {
    /// Create a new instance to store type and variable context
    pub fn new() -> Self {
        let mut context = Context {
            scopes: Vec::from([ScopeData {
                parent_scope: usize::MAX,
                variables: VariableBlock::new(),
                function_ids: HashMap::new(),
                struct_ids: HashMap::new(),
                cbuffer_ids: HashMap::new(),
                global_ids: HashMap::new(),
                template_args: HashMap::new(),
                function_return_type: None,
            }]),
            current_scope: 0,
            function_data: Vec::new(),
            struct_data: Vec::new(),
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

    /// Add a new scope
    pub fn push_scope(&mut self) {
        self.scopes.push(ScopeData {
            parent_scope: self.current_scope,
            variables: VariableBlock::new(),
            function_ids: HashMap::new(),
            struct_ids: HashMap::new(),
            cbuffer_ids: HashMap::new(),
            global_ids: HashMap::new(),
            template_args: HashMap::new(),
            function_return_type: None,
        });
        self.current_scope = self.scopes.len() - 1;
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

    /// Set the scope as a function scope with the given return type
    pub fn set_function_return_type(&mut self, return_type: ir::Type) {
        assert_eq!(self.scopes[self.current_scope].function_return_type, None);
        self.scopes[self.current_scope].function_return_type = Some(return_type);
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

    /// Get the return type of the current function
    pub fn get_current_return_type(&self) -> ir::Type {
        match self.search_scopes(|s| s.function_return_type.clone()) {
            Some(ret) => ret,
            None => panic!("Not inside function"),
        }
    }

    /// Find a variable with a given name in the current scope
    pub fn find_variable(&self, name: &str) -> TyperResult<VariableExpression> {
        let mut scope_index = self.current_scope;
        let mut scopes_up = 0;
        loop {
            if let Some(ve) =
                self.find_variable_in_scope(&self.scopes[scope_index], name, scopes_up)
            {
                return Ok(ve);
            }
            scope_index = self.scopes[scope_index].parent_scope;
            scopes_up += 1;
            if scope_index == usize::MAX {
                break;
            }
        }
        Err(TyperError::UnknownIdentifier(name.to_string()))
    }

    /// Find the id for a given type name
    pub fn find_type_id(&self, name: &str) -> TyperResult<ir::TypeLayout> {
        match self.search_scopes(|s| {
            if let Some(id) = s.struct_ids.get(name) {
                return Some(ir::TypeLayout::Struct(*id));
            }
            if let Some(id) = s.template_args.get(name) {
                return Some(ir::TypeLayout::TemplateParam(*id));
            }
            None
        }) {
            Some(id) => Ok(id),
            None => Err(TyperError::UnknownType(ErrorType::Untyped(
                ast::Type::custom(name),
            ))),
        }
    }

    /// Find the type of a variable
    pub fn get_type_of_variable(&self, var_ref: &ir::VariableRef) -> TyperResult<ExpressionType> {
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
    pub fn get_type_of_global(&self, id: &ir::GlobalId) -> TyperResult<ExpressionType> {
        assert!(id.0 < self.global_data.len() as u32);
        Ok(self.global_data[id.0 as usize].ty.clone().to_lvalue())
    }

    /// Find the type of a constant buffer member
    pub fn get_type_of_constant(
        &self,
        id: &ir::ConstantBufferId,
        name: &str,
    ) -> TyperResult<ExpressionType> {
        assert!(id.0 < self.cbuffer_data.len() as u32);
        match self.cbuffer_data[id.0 as usize].members.get(name) {
            Some(ty) => Ok(ty.to_lvalue()),
            None => Err(TyperError::ConstantDoesNotExist(*id, name.to_string())),
        }
    }

    /// Find the type of a struct member
    pub fn get_type_of_struct_member(
        &self,
        id: &ir::StructId,
        name: &str,
    ) -> TyperResult<ExpressionType> {
        assert!(id.0 < self.struct_data.len() as u32);
        match self.struct_data[id.0 as usize].members.get(name) {
            Some(ty) => Ok(ty.to_lvalue()),
            None => Err(TyperError::StructMemberDoesNotExist(*id, name.to_string())),
        }
    }

    /// Find the return type of a function
    pub fn get_type_of_function_return(&self, id: &ir::FunctionId) -> TyperResult<ExpressionType> {
        assert!(id.0 < self.function_data.len() as u32);
        Ok(self.function_data[id.0 as usize]
            .overload
            .1
            .clone()
            .to_rvalue())
    }

    /// Get the name from a function id
    pub fn get_function_name(&self, id: &ir::FunctionId) -> &str {
        assert!(id.0 < self.function_data.len() as u32);
        &self.function_data[id.0 as usize].name.node
    }

    /// Get the name from a struct id
    pub fn get_struct_name(&self, id: &ir::StructId) -> &str {
        assert!(id.0 < self.struct_data.len() as u32);
        &self.struct_data[id.0 as usize].name.node
    }

    /// Get the name from a constant buffer id
    pub fn get_cbuffer_name(&self, id: &ir::ConstantBufferId) -> &str {
        assert!(id.0 < self.cbuffer_data.len() as u32);
        &self.cbuffer_data[id.0 as usize].name.node
    }

    /// Get the name from a global variable id
    pub fn get_global_name(&self, id: &ir::GlobalId) -> &str {
        assert!(id.0 < self.global_data.len() as u32);
        &self.global_data[id.0 as usize].name.node
    }

    /// Get the source location from a function id
    pub fn get_function_location(&self, id: &ir::FunctionId) -> SourceLocation {
        assert!(id.0 < self.function_data.len() as u32);
        self.function_data[id.0 as usize].name.location
    }

    /// Get the source location from a struct id
    pub fn get_struct_location(&self, id: &ir::StructId) -> SourceLocation {
        assert!(id.0 < self.struct_data.len() as u32);
        self.struct_data[id.0 as usize].name.location
    }

    /// Get the source location from a constant buffer id
    pub fn get_cbuffer_location(&self, id: &ir::ConstantBufferId) -> SourceLocation {
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
    pub fn insert_function(
        &mut self,
        name: Located<String>,
        return_type: ir::Type,
        param_types: Vec<ir::ParamType>,
    ) -> TyperResult<ir::FunctionId> {
        let id = ir::FunctionId(self.function_data.len() as u32);
        self.function_data.push(FunctionData {
            name,
            overload: FunctionOverload(FunctionName::User(id), return_type, param_types),
        });
        self.insert_function_in_scope(self.current_scope as usize, id)?;
        Ok(id)
    }

    /// Register an intrinsic
    fn insert_intrinsic(
        &mut self,
        name: Located<String>,
        overload: FunctionOverload,
    ) -> TyperResult<ir::FunctionId> {
        let id = ir::FunctionId(self.function_data.len() as u32);
        self.function_data.push(FunctionData { name, overload });
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
    pub fn insert_struct(
        &mut self,
        name: Located<String>,
        members: HashMap<String, ir::Type>,
    ) -> Result<ir::StructId, ir::StructId> {
        let data = StructData { name, members };
        let id = ir::StructId(self.struct_data.len() as u32);
        self.struct_data.push(data);
        let data = self.struct_data.last().unwrap();
        match self.scopes[self.current_scope]
            .struct_ids
            .entry(data.name.to_string())
        {
            Entry::Vacant(id_v) => {
                id_v.insert(id);
                Ok(id)
            }
            Entry::Occupied(id_o) => Err(*id_o.get()),
        }
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
                    if self.function_data[existing_id.0 as usize].overload.2 == data.overload.2 {
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

    fn get_type_of_variable(&self, var_ref: &ir::VariableRef) -> TyperResult<ExpressionType> {
        let &ir::VariableRef(ref id, _) = var_ref;
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
        let return_type = factory.get_return_type();
        let overload = FunctionOverload(
            FunctionName::Intrinsic(factory.clone()),
            return_type.0,
            params.to_vec(),
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

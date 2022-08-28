use super::errors::{ToErrorType, TyperError, TyperResult};
use crate::typer::errors::ErrorType;
use crate::typer::functions::{FunctionName, FunctionOverload};
use ir::{ExpressionType, Intrinsic, ToExpressionType};
use rssl_ast as ast;
use rssl_ir as ir;
use std::collections::hash_map::Entry;
use std::collections::HashMap;

/// Stores all ids for types and variables by a module
#[derive(Debug, Clone)]
pub struct Context {
    scopes: Vec<ScopeData>,
    current_scope: usize,

    next_free_function_id: ir::FunctionId,
    next_free_struct_id: ir::StructId,
    next_free_cbuffer_id: ir::ConstantBufferId,
    next_free_global_id: ir::GlobalId,
}

#[derive(Debug, Clone)]
struct ScopeData {
    parent_scope: usize,

    variables: VariableBlock,

    functions: HashMap<String, UnresolvedFunction>,
    function_names: HashMap<ir::FunctionId, String>,

    struct_ids: HashMap<String, ir::StructId>,
    struct_names: HashMap<ir::StructId, String>,
    struct_definitions: HashMap<ir::StructId, HashMap<String, ir::Type>>,

    cbuffer_ids: HashMap<String, ir::ConstantBufferId>,
    cbuffer_names: HashMap<ir::ConstantBufferId, String>,
    cbuffer_definitions: HashMap<ir::ConstantBufferId, HashMap<String, ir::Type>>,

    globals: HashMap<String, (ir::Type, ir::GlobalId)>,
    global_names: HashMap<ir::GlobalId, String>,

    function_return_type: Option<ir::Type>,
}

/// Result of a variable query
pub enum VariableExpression {
    Local(ir::VariableRef, ir::Type),
    Global(ir::GlobalId, ir::Type),
    Constant(ir::ConstantBufferId, String, ir::Type),
    Function(UnresolvedFunction),
}

/// Set of overloaded functions
#[derive(PartialEq, Debug, Clone)]
pub struct UnresolvedFunction(pub String, pub Vec<FunctionOverload>);

impl Context {
    /// Create a new instance to store type and variable context
    pub fn new() -> Self {
        Context {
            scopes: Vec::from([ScopeData {
                parent_scope: usize::MAX,
                variables: VariableBlock::new(),
                functions: get_intrinsics(),
                function_names: HashMap::new(),
                struct_ids: HashMap::new(),
                struct_names: HashMap::new(),
                struct_definitions: HashMap::new(),
                cbuffer_ids: HashMap::new(),
                cbuffer_names: HashMap::new(),
                cbuffer_definitions: HashMap::new(),
                globals: HashMap::new(),
                global_names: HashMap::new(),
                function_return_type: None,
            }]),
            current_scope: 0,
            next_free_function_id: ir::FunctionId(0),
            next_free_struct_id: ir::StructId(0),
            next_free_cbuffer_id: ir::ConstantBufferId(0),
            next_free_global_id: ir::GlobalId(0),
        }
    }

    /// Add a new scope
    pub fn push_scope(&mut self) {
        self.scopes.push(ScopeData {
            parent_scope: self.current_scope,
            variables: VariableBlock::new(),
            functions: HashMap::new(),
            function_names: HashMap::new(),
            struct_ids: HashMap::new(),
            struct_names: HashMap::new(),
            struct_definitions: HashMap::new(),
            cbuffer_ids: HashMap::new(),
            cbuffer_names: HashMap::new(),
            cbuffer_definitions: HashMap::new(),
            globals: HashMap::new(),
            global_names: HashMap::new(),
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

    /// Add a new scope for a function
    pub fn push_function_scope(&mut self, return_type: ir::Type) {
        self.push_scope();
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
        let mut scope_index = self.current_scope;
        loop {
            if let Some(ref ret) = self.scopes[scope_index].function_return_type {
                return ret.clone();
            }
            scope_index = self.scopes[scope_index].parent_scope;
            if scope_index == usize::MAX {
                panic!("Not inside function")
            }
        }
    }

    /// Find a variable with a given name in the current scope
    pub fn find_variable(&self, name: &str) -> TyperResult<VariableExpression> {
        let mut scope_index = self.current_scope;
        let mut scopes_up = 0;
        loop {
            if let Some(ve) = self.scopes[scope_index].find_variable(name, scopes_up) {
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

    /// Find the id for a given struct name
    pub fn find_struct_id(&self, name: &str) -> TyperResult<ir::StructId> {
        let mut scope_index = self.current_scope;
        loop {
            if let Some(id) = self.scopes[scope_index].find_struct_id(name) {
                return Ok(id);
            }
            scope_index = self.scopes[scope_index].parent_scope;
            if scope_index == usize::MAX {
                break;
            }
        }
        Err(TyperError::UnknownType(ErrorType::Untyped(
            ast::Type::custom(name),
        )))
    }

    /// Find the type of a variable
    pub fn get_type_of_variable(&self, var_ref: &ir::VariableRef) -> TyperResult<ExpressionType> {
        let mut scope_index = self.current_scope;
        for _ in 0..(var_ref.1 .0) {
            scope_index = self.scopes[scope_index].parent_scope;
            assert_ne!(scope_index, usize::MAX);
        }
        self.scopes[scope_index].get_type_of_variable(var_ref)
    }

    /// Find the type of a global variable
    pub fn get_type_of_global(&self, id: &ir::GlobalId) -> TyperResult<ExpressionType> {
        let mut scope_index = self.current_scope;
        loop {
            if let Some(ty) = self.scopes[scope_index].get_type_of_global(id) {
                return Ok(ty);
            }
            scope_index = self.scopes[scope_index].parent_scope;
            if scope_index == usize::MAX {
                break;
            }
        }
        panic!("Invalid global variable id: {:?}", id)
    }

    /// Find the type of a constant buffer member
    pub fn get_type_of_constant(
        &self,
        id: &ir::ConstantBufferId,
        name: &str,
    ) -> TyperResult<ExpressionType> {
        let mut scope_index = self.current_scope;
        loop {
            if let Some(res) = self.scopes[scope_index].get_type_of_constant(id, name) {
                return res;
            }
            scope_index = self.scopes[scope_index].parent_scope;
            if scope_index == usize::MAX {
                break;
            }
        }
        panic!("Invalid constant buffer id: {:?}", id)
    }

    /// Find the type of a struct member
    pub fn get_type_of_struct_member(
        &self,
        id: &ir::StructId,
        name: &str,
    ) -> TyperResult<ExpressionType> {
        let mut scope_index = self.current_scope;
        loop {
            if let Some(res) = self.scopes[scope_index].get_type_of_struct_member(id, name) {
                return res;
            }
            scope_index = self.scopes[scope_index].parent_scope;
            if scope_index == usize::MAX {
                break;
            }
        }
        panic!("Invalid struct id: {:?}", id)
    }

    /// Find the return type of a function
    pub fn get_type_of_function_return(&self, id: &ir::FunctionId) -> TyperResult<ExpressionType> {
        let mut scope_index = self.current_scope;
        loop {
            if let Some(ty) = self.scopes[scope_index].get_type_of_function_return(id) {
                return Ok(ty);
            }
            scope_index = self.scopes[scope_index].parent_scope;
            if scope_index == usize::MAX {
                break;
            }
        }
        panic!("Invalid function id: {:?}", id)
    }

    /// Register a local variable
    pub fn insert_variable(
        &mut self,
        name: String,
        typename: ir::Type,
    ) -> TyperResult<ir::VariableId> {
        self.scopes[self.current_scope]
            .variables
            .insert_variable(name, typename)
    }

    /// Get a function id to setup overload
    pub fn make_function_id(&mut self) -> ir::FunctionId {
        let value = self.next_free_function_id;
        self.next_free_function_id = ir::FunctionId(self.next_free_function_id.0 + 1);
        value
    }

    /// Register a function overload
    pub fn insert_function(
        &mut self,
        name: String,
        function_type: FunctionOverload,
    ) -> TyperResult<()> {
        self.scopes[self.current_scope].insert_function(name, function_type)
    }

    /// Register a new global variable
    pub fn insert_global(&mut self, name: String, typename: ir::Type) -> TyperResult<ir::GlobalId> {
        let id = self.next_free_global_id;
        self.next_free_global_id = ir::GlobalId(self.next_free_global_id.0 + 1);
        self.scopes[self.current_scope].insert_global(id, name, typename)
    }

    /// Register a new struct type
    pub fn insert_struct(
        &mut self,
        name: &str,
        members: HashMap<String, ir::Type>,
    ) -> Option<ir::StructId> {
        let id = self.next_free_struct_id;
        self.next_free_struct_id = ir::StructId(self.next_free_struct_id.0 + 1);
        self.scopes[self.current_scope].insert_struct(id, name, members)
    }

    /// Register a new constant buffer
    pub fn insert_cbuffer(
        &mut self,
        name: &str,
        members: HashMap<String, ir::Type>,
    ) -> Option<ir::ConstantBufferId> {
        let id = self.next_free_cbuffer_id;
        self.next_free_cbuffer_id = ir::ConstantBufferId(self.next_free_cbuffer_id.0 + 1);
        self.scopes[self.current_scope].insert_cbuffer(id, name, members)
    }

    /// Make a name map from a set of root definitions
    pub fn gather_global_names(
        &self,
        root_definitions: &[ir::RootDefinition],
    ) -> ir::GlobalDeclarations {
        // We currently only expect these in the root so we only need to go to the first scope
        // This match would be best elsewhere with context exposing just the name finding

        assert_eq!(self.current_scope, 0);

        let mut decls = ir::GlobalDeclarations {
            functions: HashMap::new(),
            globals: HashMap::new(),
            structs: HashMap::new(),
            constants: HashMap::new(),
        };

        for def in root_definitions {
            match def {
                ir::RootDefinition::Struct(ref sd) => {
                    match self.scopes[0].struct_names.get(&sd.id) {
                        Some(name) => {
                            decls.structs.insert(sd.id, name.clone());
                        }
                        None => {
                            panic!("struct name does not exist");
                        }
                    }
                }
                ir::RootDefinition::ConstantBuffer(ref cb) => {
                    match self.scopes[0].cbuffer_names.get(&cb.id) {
                        Some(name) => {
                            decls.constants.insert(cb.id, name.clone());
                        }
                        None => {
                            panic!("constant buffer name does not exist");
                        }
                    }
                }
                ir::RootDefinition::GlobalVariable(ref gv) => {
                    match self.scopes[0].global_names.get(&gv.id) {
                        Some(name) => {
                            decls.globals.insert(gv.id, name.clone());
                        }
                        None => {
                            panic!("global variable name does not exist");
                        }
                    }
                }
                ir::RootDefinition::Function(ref func) => {
                    match self.scopes[0].function_names.get(&func.id) {
                        Some(name) => {
                            decls.functions.insert(func.id, name.clone());
                        }
                        None => {
                            panic!("function name does not exist");
                        }
                    }
                }
            }
        }

        decls
    }
}

impl ScopeData {
    fn find_variable(&self, name: &str, scopes_up: u32) -> Option<VariableExpression> {
        if let Some(ve) = self.variables.find_variable(name, scopes_up) {
            return Some(ve);
        }

        if let Some(tys) = self.functions.get(name) {
            return Some(VariableExpression::Function(tys.clone()));
        }

        for (id, members) in &self.cbuffer_definitions {
            for (member_name, ty) in members {
                if member_name == name {
                    return Some(VariableExpression::Constant(
                        *id,
                        name.to_string(),
                        ty.clone(),
                    ));
                }
            }
        }

        if let Some(&(ref ty, ref id)) = self.globals.get(name) {
            return Some(VariableExpression::Global(*id, ty.clone()));
        }

        None
    }

    fn find_struct_id(&self, name: &str) -> Option<ir::StructId> {
        self.struct_ids.get(name).copied()
    }

    fn get_type_of_variable(&self, var_ref: &ir::VariableRef) -> TyperResult<ExpressionType> {
        let &ir::VariableRef(ref id, _) = var_ref;
        for &(ref var_ty, ref var_id) in self.variables.variables.values() {
            if id == var_id {
                return Ok(var_ty.to_lvalue());
            }
        }
        panic!("Invalid local variable id: {:?}", var_ref);
    }

    fn get_type_of_global(&self, id: &ir::GlobalId) -> Option<ExpressionType> {
        for &(ref global_ty, ref global_id) in self.globals.values() {
            if id == global_id {
                return Some(global_ty.to_lvalue());
            }
        }
        None
    }

    fn get_type_of_constant(
        &self,
        id: &ir::ConstantBufferId,
        name: &str,
    ) -> Option<TyperResult<ExpressionType>> {
        match self.cbuffer_definitions.get(id) {
            Some(cm) => match cm.get(name) {
                Some(ty) => Some(Ok(ty.to_lvalue())),
                None => Some(Err(TyperError::ConstantDoesNotExist(*id, name.to_string()))),
            },
            None => None,
        }
    }

    fn get_type_of_struct_member(
        &self,
        id: &ir::StructId,
        name: &str,
    ) -> Option<TyperResult<ExpressionType>> {
        match self.struct_definitions.get(id) {
            Some(cm) => match cm.get(name) {
                Some(ty) => Some(Ok(ty.to_lvalue())),
                None => Some(Err(TyperError::StructMemberDoesNotExist(
                    *id,
                    name.to_string(),
                ))),
            },
            None => None,
        }
    }

    fn get_type_of_function_return(&self, id: &ir::FunctionId) -> Option<ExpressionType> {
        for unresolved in self.functions.values() {
            for overload in &unresolved.1 {
                match overload.0 {
                    FunctionName::Intrinsic(_) => {}
                    FunctionName::User(ref func_id) => {
                        if func_id == id {
                            return Some(overload.1.clone().to_rvalue());
                        }
                    }
                }
            }
        }
        None
    }

    fn insert_function(
        &mut self,
        name: String,
        function_type: FunctionOverload,
    ) -> TyperResult<()> {
        // Error if a variable of the same name already exists
        match self.find_variable(&name, 0) {
            Some(VariableExpression::Local(_, ref ty))
            | Some(VariableExpression::Global(_, ref ty))
            | Some(VariableExpression::Constant(_, _, ref ty)) => {
                return Err(TyperError::ValueAlreadyDefined(
                    name,
                    ty.to_error_type(),
                    ErrorType::Unknown,
                ));
            }
            _ => {}
        };

        fn insert_function_name(
            function_names: &mut HashMap<ir::FunctionId, String>,
            function_type: FunctionOverload,
            name: String,
        ) {
            match function_type.0 {
                FunctionName::User(id) => match function_names.entry(id) {
                    Entry::Occupied(_) => {
                        panic!("function id named twice");
                    }
                    Entry::Vacant(vacant) => {
                        vacant.insert(name);
                    }
                },
                FunctionName::Intrinsic(_) => {}
            }
        }

        // Try to add the function
        match self.functions.entry(name.clone()) {
            Entry::Occupied(mut occupied) => {
                // Fail if the overload already exists
                for &FunctionOverload(_, _, ref args) in &occupied.get().1 {
                    if *args == function_type.2 {
                        return Err(TyperError::ValueAlreadyDefined(
                            name,
                            ErrorType::Unknown,
                            ErrorType::Unknown,
                        ));
                    }
                }
                // Insert a new overload
                insert_function_name(&mut self.function_names, function_type.clone(), name);
                occupied.get_mut().1.push(function_type);
                Ok(())
            }
            Entry::Vacant(vacant) => {
                // Insert a new function with one overload
                insert_function_name(
                    &mut self.function_names,
                    function_type.clone(),
                    name.clone(),
                );
                vacant.insert(UnresolvedFunction(name, vec![function_type]));
                Ok(())
            }
        }
    }

    fn insert_global(
        &mut self,
        id: ir::GlobalId,
        name: String,
        typename: ir::Type,
    ) -> TyperResult<ir::GlobalId> {
        match self.find_variable(&name, 0) {
            Some(VariableExpression::Local(_, ref ty))
            | Some(VariableExpression::Global(_, ref ty))
            | Some(VariableExpression::Constant(_, _, ref ty)) => {
                return Err(TyperError::ValueAlreadyDefined(
                    name,
                    ty.to_error_type(),
                    typename.to_error_type(),
                ));
            }
            _ => {}
        };
        match self.globals.entry(name.clone()) {
            Entry::Occupied(_) => unreachable!("global variable inserted multiple times"),
            Entry::Vacant(vacant) => {
                vacant.insert((typename, id));
                match self.global_names.insert(id, name) {
                    Some(_) => panic!("global variable named multiple times"),
                    None => {}
                };
                Ok(id)
            }
        }
    }

    fn insert_struct(
        &mut self,
        id: ir::StructId,
        name: &str,
        members: HashMap<String, ir::Type>,
    ) -> Option<ir::StructId> {
        match (
            self.struct_ids.entry(name.to_string()),
            self.struct_names.entry(id),
            self.struct_definitions.entry(id),
        ) {
            (Entry::Vacant(id_entry), Entry::Vacant(name_entry), Entry::Vacant(def_entry)) => {
                id_entry.insert(id);
                name_entry.insert(name.to_string());
                def_entry.insert(members);
                Some(id)
            }
            _ => None,
        }
    }

    fn insert_cbuffer(
        &mut self,
        id: ir::ConstantBufferId,
        name: &str,
        members: HashMap<String, ir::Type>,
    ) -> Option<ir::ConstantBufferId> {
        match (
            self.cbuffer_ids.entry(name.to_string()),
            self.cbuffer_names.entry(id),
            self.cbuffer_definitions.entry(id),
        ) {
            (Entry::Vacant(id_entry), Entry::Vacant(name_entry), Entry::Vacant(def_entry)) => {
                id_entry.insert(id);
                name_entry.insert(name.to_string());
                def_entry.insert(members);
                Some(id)
            }
            _ => None,
        }
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

    fn insert_variable(&mut self, name: String, typename: ir::Type) -> TyperResult<ir::VariableId> {
        if let Some(&(ref ty, _)) = self.variables.get(&name) {
            return Err(TyperError::ValueAlreadyDefined(
                name,
                ty.to_error_type(),
                typename.to_error_type(),
            ));
        };
        match self.variables.entry(name.clone()) {
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
fn get_intrinsics() -> HashMap<String, UnresolvedFunction> {
    use crate::intrinsics::*;
    let funcs = get_intrinsics();

    let mut strmap: HashMap<String, UnresolvedFunction> = HashMap::new();
    for &(ref name, params, ref factory) in funcs {
        let return_type = match *factory {
            IntrinsicFactory::Intrinsic0(ref i) => i.get_return_type(),
            IntrinsicFactory::Intrinsic1(ref i) => i.get_return_type(),
            IntrinsicFactory::Intrinsic2(ref i) => i.get_return_type(),
            IntrinsicFactory::Intrinsic3(ref i) => i.get_return_type(),
        };
        let overload = FunctionOverload(
            FunctionName::Intrinsic(factory.clone()),
            return_type.0,
            params.to_vec(),
        );
        match strmap.entry(name.to_string()) {
            Entry::Occupied(mut occupied) => {
                let &mut UnresolvedFunction(_, ref mut overloads) = occupied.get_mut();
                overloads.push(overload);
            }
            Entry::Vacant(vacant) => {
                vacant.insert(UnresolvedFunction(name.to_string(), vec![overload]));
            }
        }
    }
    strmap
}

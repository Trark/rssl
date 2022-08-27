use super::errors::{ToErrorType, TyperError, TyperResult};
use crate::typer::errors::ErrorType;
use crate::typer::functions::{FunctionName, FunctionOverload};
use ir::{ExpressionType, Intrinsic, ToExpressionType};
use rssl_ast as ast;
use rssl_ir as ir;
use std::collections::hash_map::Entry;
use std::collections::HashMap;

/// Context which can query known variables and types
pub trait ExpressionContext: StructIdFinder + TypeContext {
    fn find_variable(&self, name: &str) -> TyperResult<VariableExpression>;
    fn find_struct_member(&self, id: &ir::StructId, member_name: &str) -> TyperResult<ir::Type>;
    fn get_return_type(&self) -> ir::Type;

    fn as_struct_id_finder(&self) -> &dyn StructIdFinder;
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

/// Context which can query struct ids from names
pub trait StructIdFinder {
    fn find_struct_id(&self, name: &str) -> TyperResult<ir::StructId>;
}

/// An object to hold all context of the type of definitions at a point in
/// the program
pub trait TypeContext: AsTypeContext {
    fn get_local(&self, var_ref: &ir::VariableRef) -> FindTypeResult;
    fn get_global(&self, id: &ir::GlobalId) -> FindTypeResult;
    fn get_constant(&self, id: &ir::ConstantBufferId, name: &str) -> FindTypeResult;
    fn get_struct_member(&self, id: &ir::StructId, name: &str) -> FindTypeResult;
    fn get_function_return(&self, id: &ir::FunctionId) -> FindTypeResult;
}

pub type FindTypeResult = Result<ExpressionType, FindTypeError>;

/// Error in parsing the type of an expression
/// These will all be internal errors as they represent eeither incorrectly
/// generated ir trees or incorrectly tracking the ids in a tree
#[derive(PartialEq, Debug, Clone)]
pub enum FindTypeError {
    InvalidLocal,
    GlobalDoesNotExist(ir::GlobalId),
    ConstantBufferDoesNotExist(ir::ConstantBufferId),
    ConstantDoesNotExist(ir::ConstantBufferId, String),
    StructDoesNotExist(ir::StructId),
    StructMemberDoesNotExist(ir::StructId, String),
    FunctionDoesNotExist(ir::FunctionId),

    InvalidTypeForSwizzle(ir::TypeLayout),
    MemberNodeMustBeUsedOnStruct(ir::TypeLayout, String),
    ArrayIndexMustBeUsedOnArrayType(ir::TypeLayout),
}

pub trait AsTypeContext {
    fn as_type_context(&self) -> &dyn TypeContext;
}

impl<T: TypeContext> AsTypeContext for T {
    fn as_type_context(&self) -> &dyn TypeContext {
        self
    }
}

#[derive(PartialEq, Debug, Clone)]
pub enum Context {
    Global(Box<GlobalContext>),
    Scope(ScopeContext),
}

#[derive(PartialEq, Debug, Clone)]
pub struct ScopeContext {
    pub parent: Box<Context>,

    pub variables: VariableBlock,
}

#[derive(PartialEq, Debug, Clone)]
pub struct VariableBlock {
    pub variables: HashMap<String, (ir::Type, ir::VariableId)>,
    pub next_free_variable_id: ir::VariableId,
}

#[derive(PartialEq, Debug, Clone)]
struct TypeBlock {
    struct_ids: HashMap<String, ir::StructId>,
    struct_names: HashMap<ir::StructId, String>,
    struct_definitions: HashMap<ir::StructId, HashMap<String, ir::Type>>,
    next_free_struct_id: ir::StructId,

    cbuffer_ids: HashMap<String, ir::ConstantBufferId>,
    cbuffer_names: HashMap<ir::ConstantBufferId, String>,
    cbuffer_definitions: HashMap<ir::ConstantBufferId, HashMap<String, ir::Type>>,
    next_free_cbuffer_id: ir::ConstantBufferId,
}

#[derive(PartialEq, Debug, Clone)]
pub struct GlobalContext {
    functions: HashMap<String, UnresolvedFunction>,
    function_names: HashMap<ir::FunctionId, String>,
    next_free_function_id: ir::FunctionId,

    types: TypeBlock,
    globals: HashMap<String, (ir::Type, ir::GlobalId)>,
    global_names: HashMap<ir::GlobalId, String>,
    next_free_global_id: ir::GlobalId,

    pub current_return_type: Option<ir::Type>,
}

impl Context {
    fn find_variable_recur(&self, name: &str, scopes_up: u32) -> TyperResult<VariableExpression> {
        match *self {
            Context::Global(ref global) => global.find_variable_recur(name, scopes_up),
            Context::Scope(ref scope) => scope.find_variable_recur(name, scopes_up),
        }
    }

    fn find_struct_id(&self, name: &str) -> TyperResult<ir::StructId> {
        match *self {
            Context::Global(ref global) => global.find_struct_id(name),
            Context::Scope(ref scope) => scope.find_struct_id(name),
        }
    }

    fn get_return_type(&self) -> ir::Type {
        match *self {
            Context::Global(ref global) => global.get_return_type(),
            Context::Scope(ref scope) => scope.get_return_type(),
        }
    }

    fn find_struct_member(&self, id: &ir::StructId, member_name: &str) -> TyperResult<ir::Type> {
        match *self {
            Context::Global(ref global) => global.find_struct_member(id, member_name),
            Context::Scope(ref scope) => scope.find_struct_member(id, member_name),
        }
    }

    fn get_type_block(&self) -> &TypeBlock {
        match *self {
            Context::Global(ref global) => global.get_type_block(),
            Context::Scope(ref scope) => scope.get_type_block(),
        }
    }
}

impl ScopeContext {
    pub fn from_scope(parent: &ScopeContext) -> ScopeContext {
        ScopeContext {
            parent: Box::new(Context::Scope(parent.clone())),
            variables: VariableBlock::new(),
        }
    }

    pub fn from_global(parent: &GlobalContext) -> ScopeContext {
        ScopeContext {
            parent: Box::new(Context::Global(Box::new(parent.clone()))),
            variables: VariableBlock::new(),
        }
    }

    fn find_variable_recur(&self, name: &str, scopes_up: u32) -> TyperResult<VariableExpression> {
        match self.variables.find_variable(name, scopes_up) {
            Some(texp) => Ok(texp),
            None => self.parent.find_variable_recur(name, scopes_up + 1),
        }
    }

    pub fn destruct(self) -> ir::ScopedDeclarations {
        ir::ScopedDeclarations {
            variables: self.variables.destruct(),
        }
    }

    pub fn insert_variable(
        &mut self,
        name: String,
        typename: ir::Type,
    ) -> TyperResult<ir::VariableId> {
        let type_block = self.parent.get_type_block();
        let variables = &mut self.variables;
        variables.insert_variable(name, typename, type_block)
    }

    fn get_type_block(&self) -> &TypeBlock {
        self.parent.get_type_block()
    }
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
        name: String,
        typename: ir::Type,
        _: &TypeBlock,
    ) -> TyperResult<ir::VariableId> {
        if let Some(&(ref ty, _)) = self.has_variable(&name) {
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

    fn has_variable(&self, name: &String) -> Option<&(ir::Type, ir::VariableId)> {
        self.variables.get(name)
    }

    pub fn find_variable(&self, name: &str, scopes_up: u32) -> Option<VariableExpression> {
        match self.variables.get(name) {
            Some(&(ref ty, ref id)) => {
                let var = ir::VariableRef(*id, ir::ScopeRef(scopes_up));
                Some(VariableExpression::Local(var, ty.clone()))
            }
            None => None,
        }
    }

    pub fn destruct(self) -> HashMap<ir::VariableId, (String, ir::Type)> {
        self.variables
            .iter()
            .fold(HashMap::new(), |mut map, (name, &(ref ty, ref id))| {
                map.insert(*id, (name.clone(), ty.clone()));
                map
            })
    }
}

impl TypeBlock {
    pub fn new() -> TypeBlock {
        TypeBlock {
            struct_ids: HashMap::new(),
            struct_names: HashMap::new(),
            struct_definitions: HashMap::new(),
            next_free_struct_id: ir::StructId(0),
            cbuffer_ids: HashMap::new(),
            cbuffer_names: HashMap::new(),
            cbuffer_definitions: HashMap::new(),
            next_free_cbuffer_id: ir::ConstantBufferId(0),
        }
    }

    pub fn insert_struct(
        &mut self,
        name: &str,
        members: HashMap<String, ir::Type>,
    ) -> Option<ir::StructId> {
        let id = self.next_free_struct_id;
        self.next_free_struct_id = ir::StructId(self.next_free_struct_id.0 + 1);
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

    pub fn find_struct_member(
        &self,
        id: &ir::StructId,
        member_name: &str,
    ) -> TyperResult<ir::Type> {
        match self.struct_definitions.get(id) {
            Some(def) => def.get(member_name).cloned().ok_or_else(|| {
                TyperError::UnknownTypeMember(
                    ir::Type::from_struct(*id).to_error_type(),
                    member_name.to_string(),
                )
            }),
            None => Err(TyperError::UnknownType(
                ir::Type::from_struct(*id).to_error_type(),
            )),
        }
    }

    pub fn insert_cbuffer(
        &mut self,
        name: &str,
        members: HashMap<String, ir::Type>,
    ) -> Option<ir::ConstantBufferId> {
        let id = self.next_free_cbuffer_id;
        self.next_free_cbuffer_id = ir::ConstantBufferId(self.next_free_cbuffer_id.0 + 1);
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

    pub fn find_variable(&self, name: &str) -> Option<VariableExpression> {
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
        None
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

impl GlobalContext {
    pub fn new() -> GlobalContext {
        GlobalContext {
            functions: get_intrinsics(),
            function_names: HashMap::new(),
            next_free_function_id: ir::FunctionId(0),
            types: TypeBlock::new(),
            globals: HashMap::new(),
            global_names: HashMap::new(),
            next_free_global_id: ir::GlobalId(0),
            current_return_type: None,
        }
    }

    pub fn insert_function(
        &mut self,
        name: String,
        function_type: FunctionOverload,
    ) -> TyperResult<()> {
        // Error if a variable of the same name already exists
        if let Some(&(ref ty, _)) = self.has_variable(&name) {
            return Err(TyperError::ValueAlreadyDefined(
                name,
                ty.to_error_type(),
                ErrorType::Unknown,
            ));
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

    fn find_variable_recur(&self, name: &str, scopes_up: u32) -> TyperResult<VariableExpression> {
        assert!(scopes_up != 0);
        match self.functions.get(name) {
            Some(tys) => return Ok(VariableExpression::Function(tys.clone())),
            None => {}
        };
        match self.types.find_variable(name) {
            Some(tys) => return Ok(tys),
            None => {}
        };
        match self.find_global(name) {
            Some(tys) => return Ok(tys),
            None => {}
        };
        Err(TyperError::UnknownIdentifier(name.to_string()))
    }

    pub fn make_function_id(&mut self) -> ir::FunctionId {
        let value = self.next_free_function_id;
        self.next_free_function_id = ir::FunctionId(self.next_free_function_id.0 + 1);
        value
    }

    fn has_variable(&self, name: &String) -> Option<&(ir::Type, ir::GlobalId)> {
        self.globals.get(name)
    }

    pub fn insert_global(&mut self, name: String, typename: ir::Type) -> TyperResult<ir::GlobalId> {
        if let Some(&(ref ty, _)) = self.has_variable(&name) {
            return Err(TyperError::ValueAlreadyDefined(
                name,
                ty.to_error_type(),
                typename.to_error_type(),
            ));
        };
        match self.globals.entry(name.clone()) {
            Entry::Occupied(_) => unreachable!("global variable inserted multiple times"),
            Entry::Vacant(vacant) => {
                let id = self.next_free_global_id;
                self.next_free_global_id = ir::GlobalId(self.next_free_global_id.0 + 1);
                vacant.insert((typename, id));
                match self.global_names.insert(id, name) {
                    Some(_) => panic!("global variable named multiple times"),
                    None => {}
                };
                Ok(id)
            }
        }
    }

    pub fn find_global(&self, name: &str) -> Option<VariableExpression> {
        match self.globals.get(name) {
            Some(&(ref ty, ref id)) => Some(VariableExpression::Global(*id, ty.clone())),
            None => None,
        }
    }

    pub fn insert_struct(
        &mut self,
        name: &str,
        members: HashMap<String, ir::Type>,
    ) -> Option<ir::StructId> {
        self.types.insert_struct(name, members)
    }

    pub fn insert_cbuffer(
        &mut self,
        name: &str,
        members: HashMap<String, ir::Type>,
    ) -> Option<ir::ConstantBufferId> {
        self.types.insert_cbuffer(name, members)
    }

    fn get_type_block(&self) -> &TypeBlock {
        &self.types
    }
}

impl ExpressionContext for ScopeContext {
    fn find_variable(&self, name: &str) -> TyperResult<VariableExpression> {
        self.find_variable_recur(name, 0)
    }

    fn find_struct_member(&self, id: &ir::StructId, member_name: &str) -> TyperResult<ir::Type> {
        self.parent.find_struct_member(id, member_name)
    }

    fn get_return_type(&self) -> ir::Type {
        self.parent.get_return_type()
    }

    fn as_struct_id_finder(&self) -> &dyn StructIdFinder {
        self
    }
}

impl ExpressionContext for GlobalContext {
    fn find_variable(&self, name: &str) -> TyperResult<VariableExpression> {
        self.find_variable_recur(name, 0)
    }

    fn find_struct_member(&self, id: &ir::StructId, member_name: &str) -> TyperResult<ir::Type> {
        self.types.find_struct_member(id, member_name)
    }

    fn get_return_type(&self) -> ir::Type {
        self.current_return_type
            .clone()
            .expect("not inside function")
    }

    fn as_struct_id_finder(&self) -> &dyn StructIdFinder {
        self
    }
}

impl StructIdFinder for Context {
    fn find_struct_id(&self, name: &str) -> TyperResult<ir::StructId> {
        match *self {
            Context::Global(ref global) => global.find_struct_id(name),
            Context::Scope(ref scope) => scope.find_struct_id(name),
        }
    }
}

impl StructIdFinder for ScopeContext {
    fn find_struct_id(&self, name: &str) -> TyperResult<ir::StructId> {
        self.parent.find_struct_id(name)
    }
}

impl StructIdFinder for TypeBlock {
    fn find_struct_id(&self, name: &str) -> TyperResult<ir::StructId> {
        self.struct_ids.get(name).copied().ok_or_else(|| {
            TyperError::UnknownType(ErrorType::Untyped(ast::Type::from_layout(
                ast::TypeLayout::Custom(name.to_string()),
            )))
        })
    }
}

impl StructIdFinder for GlobalContext {
    fn find_struct_id(&self, name: &str) -> TyperResult<ir::StructId> {
        self.types.find_struct_id(name)
    }
}

impl TypeContext for Context {
    fn get_local(&self, var_ref: &ir::VariableRef) -> FindTypeResult {
        match *self {
            Context::Global(ref global) => global.get_local(var_ref),
            Context::Scope(ref scope) => scope.get_local(var_ref),
        }
    }
    fn get_global(&self, id: &ir::GlobalId) -> FindTypeResult {
        match *self {
            Context::Global(ref global) => global.get_global(id),
            Context::Scope(ref scope) => scope.get_global(id),
        }
    }
    fn get_constant(&self, id: &ir::ConstantBufferId, name: &str) -> FindTypeResult {
        match *self {
            Context::Global(ref global) => global.get_constant(id, name),
            Context::Scope(ref scope) => scope.get_constant(id, name),
        }
    }
    fn get_struct_member(&self, id: &ir::StructId, name: &str) -> FindTypeResult {
        match *self {
            Context::Global(ref global) => global.get_struct_member(id, name),
            Context::Scope(ref scope) => scope.get_struct_member(id, name),
        }
    }
    fn get_function_return(&self, id: &ir::FunctionId) -> FindTypeResult {
        match *self {
            Context::Global(ref global) => global.get_function_return(id),
            Context::Scope(ref scope) => scope.get_function_return(id),
        }
    }
}

impl TypeContext for ScopeContext {
    fn get_local(&self, var_ref: &ir::VariableRef) -> FindTypeResult {
        let &ir::VariableRef(ref id, ref scope) = var_ref;
        match scope.0 {
            0 => {
                for &(ref var_ty, ref var_id) in self.variables.variables.values() {
                    if id == var_id {
                        return Ok(var_ty.to_lvalue());
                    }
                }
                Err(FindTypeError::InvalidLocal)
            }
            up => self
                .parent
                .get_local(&ir::VariableRef(*id, ir::ScopeRef(up - 1))),
        }
    }
    fn get_global(&self, id: &ir::GlobalId) -> FindTypeResult {
        self.parent.get_global(id)
    }
    fn get_constant(&self, id: &ir::ConstantBufferId, name: &str) -> FindTypeResult {
        self.parent.get_constant(id, name)
    }
    fn get_struct_member(&self, id: &ir::StructId, name: &str) -> FindTypeResult {
        self.parent.get_struct_member(id, name)
    }
    fn get_function_return(&self, id: &ir::FunctionId) -> FindTypeResult {
        self.parent.get_function_return(id)
    }
}

impl TypeContext for GlobalContext {
    fn get_local(&self, _: &ir::VariableRef) -> FindTypeResult {
        Err(FindTypeError::InvalidLocal)
    }
    fn get_global(&self, id: &ir::GlobalId) -> FindTypeResult {
        for &(ref global_ty, ref global_id) in self.globals.values() {
            if id == global_id {
                return Ok(global_ty.to_lvalue());
            }
        }
        Err(FindTypeError::GlobalDoesNotExist(*id))
    }
    fn get_constant(&self, id: &ir::ConstantBufferId, name: &str) -> FindTypeResult {
        match self.types.cbuffer_definitions.get(id) {
            Some(cm) => match cm.get(name) {
                Some(ty) => Ok(ty.to_lvalue()),
                None => Err(FindTypeError::ConstantDoesNotExist(*id, name.to_string())),
            },
            None => Err(FindTypeError::ConstantBufferDoesNotExist(*id)),
        }
    }
    fn get_struct_member(&self, id: &ir::StructId, name: &str) -> FindTypeResult {
        match self.types.struct_definitions.get(id) {
            Some(cm) => match cm.get(name) {
                Some(ty) => Ok(ty.to_lvalue()),
                None => Err(FindTypeError::StructMemberDoesNotExist(
                    *id,
                    name.to_string(),
                )),
            },
            None => Err(FindTypeError::StructDoesNotExist(*id)),
        }
    }
    fn get_function_return(&self, id: &ir::FunctionId) -> FindTypeResult {
        for unresolved in self.functions.values() {
            for overload in &unresolved.1 {
                match overload.0 {
                    FunctionName::Intrinsic(_) => {}
                    FunctionName::User(ref func_id) => {
                        if func_id == id {
                            return Ok(overload.1.clone().to_rvalue());
                        }
                    }
                }
            }
        }
        Err(FindTypeError::FunctionDoesNotExist(*id))
    }
}

/// Make a name map from a set of root definitions
pub fn gather_global_names(
    root_definitions: &[ir::RootDefinition],
    context: &GlobalContext,
) -> ir::GlobalDeclarations {
    root_definitions.iter().fold(
        ir::GlobalDeclarations {
            functions: HashMap::new(),
            globals: HashMap::new(),
            structs: HashMap::new(),
            constants: HashMap::new(),
        },
        |mut map, def| {
            match *def {
                ir::RootDefinition::Struct(ref sd) => {
                    match context.types.struct_names.get(&sd.id) {
                        Some(name) => {
                            map.structs.insert(sd.id, name.clone());
                        }
                        None => {
                            panic!("struct name does not exist");
                        }
                    }
                }
                ir::RootDefinition::ConstantBuffer(ref cb) => {
                    match context.types.cbuffer_names.get(&cb.id) {
                        Some(name) => {
                            map.constants.insert(cb.id, name.clone());
                        }
                        None => {
                            panic!("constant buffer name does not exist");
                        }
                    }
                }
                ir::RootDefinition::GlobalVariable(ref gv) => {
                    match context.global_names.get(&gv.id) {
                        Some(name) => {
                            map.globals.insert(gv.id, name.clone());
                        }
                        None => {
                            panic!("global variable name does not exist");
                        }
                    }
                }
                ir::RootDefinition::Function(ref func) => {
                    match context.function_names.get(&func.id) {
                        Some(name) => {
                            map.functions.insert(func.id, name.clone());
                        }
                        None => {
                            panic!("function name does not exist");
                        }
                    }
                }
            }
            map
        },
    )
}

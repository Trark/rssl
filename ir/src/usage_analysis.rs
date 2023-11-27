use crate::*;
use std::collections::{HashMap, HashSet};

#[derive(Debug)]
pub struct GlobalUsageAnalysis(HashMap<UsageSymbol, LocalUsageAnalysis>);

#[derive(Default, Debug)]
struct LocalUsageAnalysis {
    required: HashSet<UsageSymbol>,
}

/// Any kind of symbol that is used
#[derive(Debug, Eq, PartialEq, Hash, Copy, Clone)]
pub enum UsageSymbol {
    Function(FunctionId),
    GlobalVariable(GlobalId),
    ConstantBuffer(ConstantBufferId),
}

impl GlobalUsageAnalysis {
    pub fn calculate(module: &Module) -> GlobalUsageAnalysis {
        let result = GlobalUsageAnalysis::calculate_local(module);
        result.recurse()
    }

    fn calculate_local(module: &Module) -> GlobalUsageAnalysis {
        let mut result = HashMap::new();

        let function_count = module.function_registry.get_function_count();
        for index in 0..function_count {
            let id = FunctionId(index);
            let usage = LocalUsageAnalysis::calculate_for_function(id, module);
            let valid_insert = result.insert(UsageSymbol::Function(id), usage).is_none();
            assert!(valid_insert);
        }

        for i in 0..module.global_registry.len() {
            let id = GlobalId(i as u32);
            // TODO: Currently no analysis of globals
            // Currently we do not need the usage analysis for default values
            let usage = LocalUsageAnalysis::default();
            let valid_insert = result
                .insert(UsageSymbol::GlobalVariable(id), usage)
                .is_none();
            assert!(valid_insert);
        }

        for i in 0..module.cbuffer_registry.len() {
            let id = ConstantBufferId(i as u32);
            // Constant buffers can not depend on any other global / function / constant buffer
            let usage = LocalUsageAnalysis::default();
            let valid_insert = result
                .insert(UsageSymbol::ConstantBuffer(id), usage)
                .is_none();
            assert!(valid_insert);
        }

        GlobalUsageAnalysis(result)
    }

    fn recurse(mut self) -> GlobalUsageAnalysis {
        let keys = self.0.keys().cloned().collect::<Vec<_>>();
        loop {
            let mut modified = false;

            for key in &keys {
                let current_set = self.0.get(key).unwrap();
                let mut new_set = current_set.required.clone();
                for other in &current_set.required {
                    new_set.extend(&self.0.get(other).unwrap().required);
                }
                if new_set.len() > current_set.required.len() {
                    let stored_analysis = self.0.get_mut(key).unwrap();
                    stored_analysis.required = new_set;
                    modified = true;
                }
            }

            if !modified {
                break;
            }
        }
        self
    }

    /// Retrieve the set of required symbols for a function
    pub fn get_usage_for_function(&self, id: FunctionId) -> &HashSet<UsageSymbol> {
        &self.0.get(&UsageSymbol::Function(id)).unwrap().required
    }
}

impl LocalUsageAnalysis {
    fn calculate_for_function(id: FunctionId, module: &Module) -> LocalUsageAnalysis {
        let mut usage = LocalUsageAnalysis::default();

        // Gather all usage within the function body
        // We do not currently consider function argument default values

        let def = module.function_registry.get_function_implementation(id);
        if let Some(def) = def {
            gather_usage_for_scope_block(&def.scope_block, &mut usage);
        }

        usage
    }
}

fn gather_usage_for_scope_block(scope_block: &ScopeBlock, usage: &mut LocalUsageAnalysis) {
    for statement in &scope_block.0 {
        gather_usage_for_statement(statement, usage);
    }
}

fn gather_usage_for_statement(statement: &Statement, usage: &mut LocalUsageAnalysis) {
    match &statement.kind {
        StatementKind::Expression(expr) => gather_usage_for_expression(expr, usage),
        StatementKind::Var(vd) => gather_usage_for_init_opt(&vd.init, usage),
        StatementKind::Block(scope_block) => {
            gather_usage_for_scope_block(scope_block, usage);
        }
        StatementKind::If(expr, scope_block) => {
            gather_usage_for_expression(expr, usage);
            gather_usage_for_scope_block(scope_block, usage);
        }
        StatementKind::IfElse(expr, block_true, block_false) => {
            gather_usage_for_expression(expr, usage);
            gather_usage_for_scope_block(block_true, usage);
            gather_usage_for_scope_block(block_false, usage);
        }
        StatementKind::For(init, cond, acc, scope_block) => {
            match init {
                ForInit::Empty => {}
                ForInit::Expression(expr) => gather_usage_for_expression(expr, usage),
                ForInit::Definitions(defs) => {
                    for def in defs {
                        gather_usage_for_init_opt(&def.init, usage)
                    }
                }
            }
            if let Some(cond) = cond {
                gather_usage_for_expression(cond, usage);
            }
            if let Some(acc) = acc {
                gather_usage_for_expression(acc, usage);
            }
            gather_usage_for_scope_block(scope_block, usage);
        }
        StatementKind::While(expr, scope_block) => {
            gather_usage_for_expression(expr, usage);
            gather_usage_for_scope_block(scope_block, usage);
        }
        StatementKind::DoWhile(scope_block, expr) => {
            gather_usage_for_scope_block(scope_block, usage);
            gather_usage_for_expression(expr, usage);
        }
        StatementKind::Switch(expr, scope_block) => {
            gather_usage_for_expression(expr, usage);
            gather_usage_for_scope_block(scope_block, usage);
        }
        StatementKind::Break => {}
        StatementKind::Continue => {}
        StatementKind::Discard => {}
        StatementKind::Return(Some(expr)) => gather_usage_for_expression(expr, usage),
        StatementKind::Return(None) => {}
        StatementKind::CaseLabel(_) => {}
        StatementKind::DefaultLabel => {}
    }
}

fn gather_usage_for_expression(expr: &Expression, usage: &mut LocalUsageAnalysis) {
    match *expr {
        Expression::Literal(_) => {}
        Expression::Variable(_) => {}
        Expression::MemberVariable(_, _) => {}
        Expression::Global(id) => {
            usage.required.insert(UsageSymbol::GlobalVariable(id));
        }
        Expression::ConstantVariable(id) => {
            usage.required.insert(UsageSymbol::ConstantBuffer(id.0));
        }
        Expression::EnumValue(_) => {}
        Expression::TernaryConditional(ref cond, ref expr_true, ref expr_false) => {
            gather_usage_for_expression(cond, usage);
            gather_usage_for_expression(expr_true, usage);
            gather_usage_for_expression(expr_false, usage);
        }
        Expression::Sequence(ref exprs) => {
            for expr in exprs {
                gather_usage_for_expression(expr, usage);
            }
        }
        Expression::Swizzle(ref object, _) => gather_usage_for_expression(object, usage),
        Expression::MatrixSwizzle(ref object, _) => {
            gather_usage_for_expression(object, usage);
        }
        Expression::ArraySubscript(ref object, ref index) => {
            gather_usage_for_expression(object, usage);
            gather_usage_for_expression(index, usage);
        }
        Expression::StructMember(ref object, _, _) => {
            gather_usage_for_expression(object, usage);
        }
        Expression::ObjectMember(ref object, _) => {
            gather_usage_for_expression(object, usage);
        }
        Expression::Call(id, _, ref args) => {
            usage.required.insert(UsageSymbol::Function(id));
            for arg in args {
                gather_usage_for_expression(arg, usage);
            }
        }
        Expression::Constructor(_, ref slots) => {
            for slot in slots {
                gather_usage_for_expression(&slot.expr, usage);
            }
        }
        Expression::Cast(_, ref inner) => gather_usage_for_expression(inner, usage),
        Expression::SizeOf(_) => {}
        Expression::IntrinsicOp(_, ref args) => {
            for arg in args {
                gather_usage_for_expression(arg, usage);
            }
        }
    }
}

fn gather_usage_for_init_opt(init: &Option<Initializer>, usage: &mut LocalUsageAnalysis) {
    if let Some(init) = init {
        gather_usage_for_init(init, usage);
    }
}

fn gather_usage_for_init(init: &Initializer, usage: &mut LocalUsageAnalysis) {
    match init {
        Initializer::Expression(expr) => gather_usage_for_expression(expr, usage),
        Initializer::Aggregate(entries) => {
            for entry in entries {
                gather_usage_for_init(entry, usage);
            }
        }
    }
}

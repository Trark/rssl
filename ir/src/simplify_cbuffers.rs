use crate::*;
use rssl_text::Located;
use std::collections::HashMap;

/// Replace cbuffers with global objects
pub fn simplify_cbuffers(module: &mut Module) {
    // All cbuffers must go
    let cbuffer_registry = std::mem::take(&mut module.cbuffer_registry);

    let mut cbuffer_to_globals = HashMap::new();
    let mut member_to_expression = HashMap::new();

    for (i, cbuffer) in cbuffer_registry.into_iter().enumerate() {
        let cbuffer_id = ConstantBufferId(i as u32);
        let struct_id = StructId(module.struct_registry.len() as u32);
        let struct_type_id = module
            .type_registry
            .register_type(TypeLayer::Struct(struct_id));

        let mut members = Vec::new();

        for member in &cbuffer.members {
            members.push(StructMember {
                name: member.name.node.clone(),
                type_id: member.type_id,
                // We do not consider binary layout differences between cbuffer form and struct form at the moment
                semantic: None,
                interpolation_modifier: None,
                precise: false,
            });
        }

        module.struct_registry.push(StructDefinition {
            id: struct_id,
            type_id: struct_type_id,
            name: Located::none(format!("{}Type", cbuffer.name.node)),
            namespace: cbuffer.namespace,
            members,
            methods: Default::default(),
        });

        let object_type_id =
            module
                .type_registry
                .register_type(TypeLayer::Object(ObjectType::ConstantBuffer(
                    struct_type_id,
                )));

        let global_id = GlobalId(module.global_registry.len() as u32);
        module.global_registry.push(GlobalVariable {
            name: cbuffer.name,
            namespace: cbuffer.namespace,
            type_id: object_type_id,
            storage_class: GlobalStorage::Extern,
            lang_slot: cbuffer.lang_binding,
            api_slot: cbuffer.api_binding,
            init: None,
            constexpr_value: None,
            is_intrinsic: false,
            is_bindless: false,
        });

        cbuffer_to_globals.insert(cbuffer_id, (global_id, struct_id));

        for member_index in 0..cbuffer.members.len() {
            member_to_expression.insert(
                ConstantBufferMemberId(cbuffer_id, member_index as u32),
                Expression::StructMember(
                    Box::new(Expression::Global(global_id)),
                    struct_id,
                    member_index as u32,
                ),
            );
        }
    }

    // Replace the top level definitions with the new type / global ids
    let mut new_definitions = Vec::new();
    for def in std::mem::take(&mut module.root_definitions) {
        match def {
            RootDefinition::ConstantBuffer(id) => {
                let (global_id, struct_id) = *cbuffer_to_globals.get(&id).unwrap();
                new_definitions.push(RootDefinition::Struct(struct_id));
                new_definitions.push(RootDefinition::GlobalVariable(global_id));
            }
            _ => new_definitions.push(def),
        }
    }
    module.root_definitions = new_definitions;

    let replacements = Replacements {
        member_to_expression,
    };

    // Structs do not currently have default values for member variables so have nothing to update
    // Struct methods are handled in the function loop

    // Ignore struct templates as they are already resolved by this point anyway

    for id in module.function_registry.iter() {
        if let Some(function_impl) = module.function_registry.get_function_implementation_mut(id) {
            for param in &mut function_impl.params {
                if let Some(expr) = &mut param.default_expr {
                    replace_cbuffer_in_expression(expr, &replacements);
                }
            }
            replace_cbuffer_in_scope_block(&mut function_impl.scope_block, &replacements);
        }
    }

    for global_var in &mut module.global_registry {
        replace_cbuffer_in_init_opt(&mut global_var.init, &replacements);
    }
}

struct Replacements {
    member_to_expression: HashMap<ConstantBufferMemberId, Expression>,
}

fn replace_cbuffer_in_scope_block(scope_block: &mut ScopeBlock, replacements: &Replacements) {
    for statement in &mut scope_block.0 {
        replace_cbuffer_in_statement(statement, replacements);
    }
}

fn replace_cbuffer_in_statement(statement: &mut Statement, replacements: &Replacements) {
    match &mut statement.kind {
        StatementKind::Expression(expr) => replace_cbuffer_in_expression(expr, replacements),
        StatementKind::Var(vd) => replace_cbuffer_in_init_opt(&mut vd.init, replacements),
        StatementKind::Block(scope_block) => {
            replace_cbuffer_in_scope_block(scope_block, replacements);
        }
        StatementKind::If(expr, scope_block) => {
            replace_cbuffer_in_expression(expr, replacements);
            replace_cbuffer_in_scope_block(scope_block, replacements);
        }
        StatementKind::IfElse(expr, block_true, block_false) => {
            replace_cbuffer_in_expression(expr, replacements);
            replace_cbuffer_in_scope_block(block_true, replacements);
            replace_cbuffer_in_scope_block(block_false, replacements);
        }
        StatementKind::For(init, cond, acc, scope_block) => {
            match init {
                ForInit::Empty => {}
                ForInit::Expression(expr) => replace_cbuffer_in_expression(expr, replacements),
                ForInit::Definitions(defs) => {
                    for def in defs {
                        replace_cbuffer_in_init_opt(&mut def.init, replacements)
                    }
                }
            }
            if let Some(cond) = cond {
                replace_cbuffer_in_expression(cond, replacements);
            }
            if let Some(acc) = acc {
                replace_cbuffer_in_expression(acc, replacements);
            }
            replace_cbuffer_in_scope_block(scope_block, replacements);
        }
        StatementKind::While(expr, scope_block) => {
            replace_cbuffer_in_expression(expr, replacements);
            replace_cbuffer_in_scope_block(scope_block, replacements);
        }
        StatementKind::DoWhile(scope_block, expr) => {
            replace_cbuffer_in_scope_block(scope_block, replacements);
            replace_cbuffer_in_expression(expr, replacements);
        }
        StatementKind::Switch(expr, scope_block) => {
            replace_cbuffer_in_expression(expr, replacements);
            replace_cbuffer_in_scope_block(scope_block, replacements);
        }
        StatementKind::Break => {}
        StatementKind::Continue => {}
        StatementKind::Discard => {}
        StatementKind::Return(Some(expr)) => replace_cbuffer_in_expression(expr, replacements),
        StatementKind::Return(None) => {}
        StatementKind::CaseLabel(_) => {}
        StatementKind::DefaultLabel => {}
    }
}

fn replace_cbuffer_in_expression(expr: &mut Expression, replacements: &Replacements) {
    match *expr {
        Expression::Literal(_) => {}
        Expression::Variable(_) => {}
        Expression::MemberVariable(_, _) => {}
        Expression::Global(_) => {}
        Expression::ConstantVariable(id) => {
            *expr = replacements.member_to_expression[&id].clone();
        }
        Expression::EnumValue(_) => {}
        Expression::TernaryConditional(ref mut cond, ref mut expr_true, ref mut expr_false) => {
            replace_cbuffer_in_expression(cond, replacements);
            replace_cbuffer_in_expression(expr_true, replacements);
            replace_cbuffer_in_expression(expr_false, replacements);
        }
        Expression::Sequence(ref mut exprs) => {
            for expr in exprs {
                replace_cbuffer_in_expression(expr, replacements);
            }
        }
        Expression::Swizzle(ref mut object, _) => {
            replace_cbuffer_in_expression(object, replacements)
        }
        Expression::MatrixSwizzle(ref mut object, _) => {
            replace_cbuffer_in_expression(object, replacements);
        }
        Expression::ArraySubscript(ref mut object, ref mut index) => {
            replace_cbuffer_in_expression(object, replacements);
            replace_cbuffer_in_expression(index, replacements);
        }
        Expression::StructMember(ref mut object, _, _) => {
            replace_cbuffer_in_expression(object, replacements);
        }
        Expression::ObjectMember(ref mut object, _) => {
            replace_cbuffer_in_expression(object, replacements);
        }
        Expression::Call(_, _, ref mut args) => {
            for arg in args {
                replace_cbuffer_in_expression(arg, replacements);
            }
        }
        Expression::Constructor(_, ref mut slots) => {
            for slot in slots {
                replace_cbuffer_in_expression(&mut slot.expr, replacements);
            }
        }
        Expression::Cast(_, ref mut inner) => replace_cbuffer_in_expression(inner, replacements),
        Expression::SizeOf(_) => {}
        Expression::IntrinsicOp(_, ref mut args) => {
            for arg in args {
                replace_cbuffer_in_expression(arg, replacements);
            }
        }
    }
}

fn replace_cbuffer_in_init_opt(init: &mut Option<Initializer>, replacements: &Replacements) {
    if let Some(init) = init {
        replace_cbuffer_in_init(init, replacements);
    }
}

fn replace_cbuffer_in_init(init: &mut Initializer, replacements: &Replacements) {
    match init {
        Initializer::Expression(expr) => replace_cbuffer_in_expression(expr, replacements),
        Initializer::Aggregate(entries) => {
            for entry in entries {
                replace_cbuffer_in_init(entry, replacements);
            }
        }
    }
}

use rssl_ast as ast;
use std::fmt::Write;

/// Target language to output for
#[derive(PartialEq, Debug)]
pub enum Target {
    /// RSSL
    Rssl,

    /// HLSL
    Hlsl,
}

/// Error result when formatting fails
#[derive(PartialEq, Debug)]
pub enum FormatError {
    /// Encountered an AmbiguousParseBranch expression
    AmbiguousParseBranch,
}

/// Format ast module as text
pub fn format(module: &ast::Module, target: Target) -> Result<String, FormatError> {
    let mut context = FormatContext::new(target);
    let mut output_string = String::new();

    format_root_definitions(
        module,
        &module.root_definitions,
        &mut output_string,
        &mut context,
    )?;
    context.new_line(&mut output_string);

    Ok(output_string)
}

/// Format root definitions
fn format_root_definitions(
    module: &ast::Module,
    decls: &[ast::RootDefinition],
    output: &mut String,
    context: &mut FormatContext,
) -> Result<(), FormatError> {
    let mut last_was_variable = false;
    for decl in decls {
        format_root_definition(module, decl, &mut last_was_variable, output, context)?;
    }

    Ok(())
}

/// Format a root definition
fn format_root_definition(
    module: &ast::Module,
    decl: &ast::RootDefinition,
    last_was_variable: &mut bool,
    output: &mut String,
    context: &mut FormatContext,
) -> Result<(), FormatError> {
    // Start a new line
    context.new_line(output);

    // Start a second new line if we are not two sequential variables
    match decl {
        ast::RootDefinition::GlobalVariable(_) => {
            if !*last_was_variable {
                context.new_line(output);
            }
            *last_was_variable = true;
        }
        _ => {
            context.new_line(output);
            *last_was_variable = false;
        }
    }

    match decl {
        ast::RootDefinition::Struct(def) => {
            format_struct(def, output, context)?;
        }
        ast::RootDefinition::Enum(def) => {
            format_enum(def, output, context)?;
        }
        ast::RootDefinition::Typedef(_) => {
            panic!("typedef is not supported in formatter");
        }
        ast::RootDefinition::ConstantBuffer(def) => {
            format_constant_buffer(def, output, context)?;
        }
        ast::RootDefinition::GlobalVariable(def) => {
            format_global_variable(def, output, context)?;
        }
        ast::RootDefinition::Function(def) => {
            format_function(def, output, context)?;
        }
        ast::RootDefinition::Namespace(name, root_definitions) => {
            output.push_str("namespace ");
            output.push_str(name);
            output.push_str(" {");

            format_root_definitions(module, root_definitions, output, context)?;

            context.new_line(output);
            context.new_line(output);
            output.push_str("} // namespace ");
            output.push_str(name);
        }
        ast::RootDefinition::Pipeline(_) if context.target == Target::Rssl => {
            panic!("pipeline definitions are not supported in formatter");
        }
        ast::RootDefinition::Pipeline(_) => {
            panic!("pipeline definitions are not supported outside of RSSL");
        }
    }

    Ok(())
}

/// Format a global variable
fn format_global_variable(
    def: &ast::GlobalVariable,
    output: &mut String,
    context: &mut FormatContext,
) -> Result<(), FormatError> {
    format_attributes(&def.attributes, false, output, context)?;
    format_type(&def.global_type, output, context)?;
    format_init_declarators(&def.defs, output, context)?;
    output.push(';');
    Ok(())
}

/// Format a register slot annotation
fn format_register_annotation(
    slot: &Option<ast::Register>,
    output: &mut String,
) -> Result<(), FormatError> {
    if let Some(slot) = &slot {
        output.push_str(" : register(");
        if let Some(register_slot) = &slot.slot {
            write!(output, "{}{}", register_slot.slot_type, register_slot.index).unwrap()
        }
        if slot.slot.is_some() && slot.space.is_some() {
            output.push_str(", ");
        }
        if let Some(space) = slot.space {
            write!(output, "space{space}").unwrap();
        }
        output.push(')');
    }

    Ok(())
}

/// Format a function definition
fn format_function(
    def: &ast::FunctionDefinition,
    output: &mut String,
    context: &mut FormatContext,
) -> Result<(), FormatError> {
    for attribute in &def.attributes {
        format_attribute(attribute, true, output, context)?;
    }

    format_template_param_list(&def.template_params, output, context)?;

    format_type(&def.returntype.return_type, output, context)?;
    output.push(' ');

    output.push_str(&def.name);

    // Scope also contains the names of parameters
    context.push_indent();

    output.push('(');

    if let Some((last, main)) = def.params.split_last() {
        for param in main {
            format_function_param(param, output, context)?;
            output.push_str(", ");
        }
        format_function_param(last, output, context)?;
    }

    output.push(')');

    format_location_annotations(&def.returntype.location_annotations, output)?;

    if let Some(body) = &def.body {
        output.push_str(" {");
        for statement in body {
            format_statement(statement, output, context)?;
        }
    } else {
        output.push(';');
    }

    context.pop_indent();

    if let Some(body) = &def.body {
        if !body.is_empty() {
            context.new_line(output);
        }
        output.push('}');
    }

    Ok(())
}

/// Format a set of attributes
fn format_attributes(
    attrs: &[ast::Attribute],
    new_line: bool,
    output: &mut String,
    context: &mut FormatContext,
) -> Result<(), FormatError> {
    for attr in attrs {
        format_attribute(attr, new_line, output, context)?;
    }
    Ok(())
}

/// Format an attribute
fn format_attribute(
    attr: &ast::Attribute,
    new_line: bool,
    output: &mut String,
    context: &mut FormatContext,
) -> Result<(), FormatError> {
    output.push('[');
    if attr.two_square_brackets {
        output.push('[');
    }

    if let Some((last, main)) = attr.name.split_last() {
        for name in main {
            output.push_str(name);
            output.push_str("::");
        }
        output.push_str(last);
    }

    if let Some((last, main)) = attr.arguments.split_last() {
        output.push('(');
        for expr in main {
            format_expression(expr, output, context)?;
            output.push_str(", ");
        }
        format_expression(last, output, context)?;
        output.push(')');
    }

    if attr.two_square_brackets {
        output.push(']');
    }
    output.push(']');

    if new_line {
        context.new_line(output);
    } else {
        output.push(' ');
    }

    Ok(())
}

/// Format a function parameter
fn format_function_param(
    param: &ast::FunctionParam,
    output: &mut String,
    context: &mut FormatContext,
) -> Result<(), FormatError> {
    format_type(&param.param_type, output, context)?;

    output.push(' ');
    format_declarator(&param.declarator, output, context)?;
    format_location_annotations(&param.location_annotations, output)?;

    if let Some(default_expr) = &param.default_expr {
        output.push_str(" = ");
        format_expression(default_expr, output, context)?;
    }

    Ok(())
}

/// Format a template parameter list
fn format_template_param_list(
    template_params: &ast::TemplateParamList,
    output: &mut String,
    context: &mut FormatContext,
) -> Result<(), FormatError> {
    if !template_params.0.is_empty() {
        output.push_str("template<");
        for i in 0..template_params.0.len() {
            if i != 0 {
                output.push_str(", ");
            }
            let param = &template_params.0[i];
            match param {
                ast::TemplateParam::Type(type_param) => {
                    output.push_str("typename");

                    if let Some(name) = &type_param.name {
                        output.push(' ');
                        output.push_str(name);
                    }

                    if type_param.default.is_some() {
                        todo!("template parameters with default values are not supported for formatting");
                    }
                }
                ast::TemplateParam::Value(value_param) => {
                    format_type(&value_param.value_type, output, context)?;

                    if let Some(name) = &value_param.name {
                        output.push(' ');
                        output.push_str(name);
                    }

                    if value_param.default.is_some() {
                        todo!("template parameters with default values are not supported for formatting");
                    }
                }
            }
        }
        output.push('>');
        context.new_line(output);
    }
    Ok(())
}

/// Format a semantic annotation
fn format_semantic_annotation(
    semantic: &Option<ast::Semantic>,
    output: &mut String,
) -> Result<(), FormatError> {
    if let Some(semantic) = &semantic {
        output.push_str(" : ");
        match semantic {
            ast::Semantic::DispatchThreadId => output.push_str("SV_DispatchThreadID"),
            ast::Semantic::GroupId => output.push_str("SV_GroupID"),
            ast::Semantic::GroupIndex => output.push_str("SV_GroupIndex"),
            ast::Semantic::GroupThreadId => output.push_str("SV_GroupThreadID"),
            ast::Semantic::VertexId => output.push_str("SV_VertexID"),
            ast::Semantic::InstanceId => output.push_str("SV_InstanceID"),
            ast::Semantic::PrimitiveId => output.push_str("SV_PrimitiveID"),
            ast::Semantic::Position => output.push_str("SV_Position"),
            ast::Semantic::Target(i) => write!(output, "SV_Target{i}").unwrap(),
            ast::Semantic::Depth => output.push_str("SV_Depth"),
            ast::Semantic::DepthGreaterEqual => output.push_str("SV_DepthGreaterEqual"),
            ast::Semantic::DepthLessEqual => output.push_str("SV_DepthLessEqual"),
            ast::Semantic::User(s) => output.push_str(s),
        }
    }
    Ok(())
}

/// Format a scoped identifier
fn format_scoped_identifier(
    id: &ast::ScopedIdentifier,
    output: &mut String,
    _: &mut FormatContext,
) -> Result<(), FormatError> {
    if id.base == ast::ScopedIdentifierBase::Absolute {
        output.push_str("::");
    }
    if let Some((leaf, scopes)) = id.identifiers.split_last() {
        for scope in scopes {
            output.push_str(scope);
            output.push_str("::");
        }
        output.push_str(leaf);
    }
    Ok(())
}

/// Format a type
fn format_type(
    ty: &ast::Type,
    output: &mut String,
    context: &mut FormatContext,
) -> Result<(), FormatError> {
    format_type_modifiers(&ty.modifiers, output)?;
    format_type_layout(&ty.layout, output, context)?;
    Ok(())
}

/// Format a type without modifiers
fn format_type_layout(
    ty: &ast::TypeLayout,
    output: &mut String,
    context: &mut FormatContext,
) -> Result<(), FormatError> {
    format_scoped_identifier(&ty.0, output, context)?;
    if let Some((last, main)) = ty.1.split_last() {
        output.push('<');
        for param in main {
            format_expression_or_type(param, output, context)?;
            output.push_str(", ");
        }
        format_expression_or_type(last, output, context)?;
        output.push('>');
    }
    Ok(())
}

/// Format type modifiers
fn format_type_modifiers(
    modifiers: &ast::TypeModifierSet,
    output: &mut String,
) -> Result<(), FormatError> {
    for modifier in &modifiers.modifiers {
        // The debug formatting is the same as we want here
        write!(output, "{:?} ", modifier.node).unwrap();
    }
    Ok(())
}

/// Format a set of variable names and parts of type bound to name with an initializer
fn format_init_declarators(
    init_declarators: &[ast::InitDeclarator],
    output: &mut String,
    context: &mut FormatContext,
) -> Result<(), FormatError> {
    let mut first = true;
    for entry in init_declarators {
        // Add separator between declarations
        if !first {
            output.push(',');
        }
        first = false;

        format_init_declarator(entry, output, context)?;
    }
    Ok(())
}

/// Format variable name and parts of type bound to name with an initializer
fn format_init_declarator(
    init_declarator: &ast::InitDeclarator,
    output: &mut String,
    context: &mut FormatContext,
) -> Result<(), FormatError> {
    output.push(' ');
    format_declarator(&init_declarator.declarator, output, context)?;
    format_location_annotations(&init_declarator.location_annotations, output)?;
    format_initializer(&init_declarator.init, output, context)?;
    Ok(())
}

/// Format variable name and parts of type bound to name
fn format_declarator(
    declarator: &ast::Declarator,
    output: &mut String,
    context: &mut FormatContext,
) -> Result<(), FormatError> {
    match declarator {
        ast::Declarator::Empty => {}
        ast::Declarator::Identifier(name, attributes) => {
            format_scoped_identifier(name, output, context)?;
            format_attributes(attributes, false, output, context)?;
        }
        ast::Declarator::Pointer(ast::PointerDeclarator {
            attributes,
            qualifiers,
            inner,
        }) => {
            output.push('*');
            format_attributes(attributes, false, output, context)?;
            format_type_modifiers(qualifiers, output)?;
            format_declarator(inner, output, context)?;
        }
        ast::Declarator::Reference(ast::ReferenceDeclarator { attributes, inner }) => {
            output.push('&');
            format_attributes(attributes, false, output, context)?;
            format_declarator(inner, output, context)?;
        }
        ast::Declarator::Array(ast::ArrayDeclarator {
            inner,
            array_size,
            attributes,
        }) => {
            format_declarator(inner, output, context)?;
            output.push('[');
            if let Some(expr) = array_size {
                format_expression(expr, output, context)?;
            }
            output.push(']');
            format_attributes(attributes, false, output, context)?;
        }
    }
    Ok(())
}

/// Format annotations for register, packoffset, or semantic
fn format_location_annotations(
    annotations: &[ast::LocationAnnotation],
    output: &mut String,
) -> Result<(), FormatError> {
    for location_annotation in annotations {
        format_location_annotation(location_annotation, output)?;
    }
    Ok(())
}

/// Format annotation for register, packoffset, or semantic
fn format_location_annotation(
    annotation: &ast::LocationAnnotation,
    output: &mut String,
) -> Result<(), FormatError> {
    match annotation {
        ast::LocationAnnotation::Semantic(semantic) => {
            format_semantic_annotation(&Some(semantic.clone()), output)
        }
        ast::LocationAnnotation::PackOffset(_) => unimplemented!("packoffset not implemented"),
        ast::LocationAnnotation::Register(semantic) => {
            format_register_annotation(&Some(semantic.clone()), output)
        }
    }
}

/// Format an expression or type
fn format_expression_or_type(
    value: &ast::ExpressionOrType,
    output: &mut String,
    context: &mut FormatContext,
) -> Result<(), FormatError> {
    match value {
        ast::ExpressionOrType::Expression(expr) | ast::ExpressionOrType::Either(expr, _) => {
            format_expression(expr, output, context)
        }
        ast::ExpressionOrType::Type(ty) => format_type(ty, output, context),
    }
}

/// Format a literal
fn format_literal(
    literal: &ast::Literal,
    output: &mut String,
    context: &mut FormatContext,
) -> Result<(), FormatError> {
    match literal {
        ast::Literal::Bool(true) => output.push_str("true"),
        ast::Literal::Bool(false) => output.push_str("false"),
        ast::Literal::IntUntyped(v) => write!(output, "{v}").unwrap(),
        ast::Literal::IntUnsigned32(v) => write!(output, "{v}u").unwrap(),
        ast::Literal::IntUnsigned64(v) => write!(output, "{v}ul").unwrap(),
        ast::Literal::IntSigned64(v) => write!(output, "{v}l").unwrap(),
        ast::Literal::FloatUntyped(v) if *v == f64::INFINITY => {
            write_infinity_untyped(output, context);
        }
        ast::Literal::FloatUntyped(v) if *v == f64::NEG_INFINITY => {
            output.push('-');
            write_infinity_untyped(output, context);
        }
        ast::Literal::FloatUntyped(v) if *v == (*v as i64 as f64) => {
            write!(output, "{}.0", *v as i64).unwrap()
        }
        ast::Literal::FloatUntyped(v) if *v > i64::MAX as f64 || *v < i64::MIN as f64 => {
            write!(output, "{v}.0").unwrap()
        }
        ast::Literal::FloatUntyped(v) => write!(output, "{v}").unwrap(),
        ast::Literal::Float16(v) if *v == f32::INFINITY => {
            write_infinity_f16(output, context);
        }
        ast::Literal::Float16(v) if *v == f32::NEG_INFINITY => {
            output.push('-');
            write_infinity_f16(output, context);
        }
        ast::Literal::Float16(v) if *v == f32::NEG_INFINITY => write!(output, "-INFINITY").unwrap(),
        ast::Literal::Float16(v) => write!(output, "{v}h").unwrap(),
        ast::Literal::Float32(v) if *v == f32::INFINITY => {
            write_infinity_f32(output, context);
        }
        ast::Literal::Float32(v) if *v == f32::NEG_INFINITY => {
            output.push('-');
            write_infinity_f32(output, context);
        }
        ast::Literal::Float32(v) if *v == (*v as i64 as f32) => {
            write!(output, "{}.0f", *v as i64).unwrap()
        }
        ast::Literal::Float32(v) if *v > i64::MAX as f32 || *v < i64::MIN as f32 => {
            write!(output, "{v}.0f").unwrap()
        }
        ast::Literal::Float32(v) => write!(output, "{v}f").unwrap(),
        ast::Literal::Float64(v) if *v == f64::INFINITY => {
            write_infinity_f64(output, context);
        }
        ast::Literal::Float64(v) if *v == f64::NEG_INFINITY => {
            output.push('-');
            write_infinity_f64(output, context);
        }
        ast::Literal::Float64(v) => write!(output, "{v}L").unwrap(),
        ast::Literal::String(s) => write!(output, "\"{s}\"").unwrap(),
    }
    Ok(())
}

fn write_infinity_untyped(output: &mut String, _: &mut FormatContext) {
    output.push_str("1.#INF");
}

fn write_infinity_f16(output: &mut String, _: &mut FormatContext) {
    output.push_str("1.#INFh");
}

fn write_infinity_f32(output: &mut String, _: &mut FormatContext) {
    output.push_str("1.#INFf");
}

fn write_infinity_f64(output: &mut String, _: &mut FormatContext) {
    output.push_str("1.#INFL");
}

/// Format a statement
fn format_statement(
    statement: &ast::Statement,
    output: &mut String,
    context: &mut FormatContext,
) -> Result<(), FormatError> {
    context.new_line(output);

    format_attributes(&statement.attributes, true, output, context)?;

    match &statement.kind {
        ast::StatementKind::Empty => {
            output.push(';');
        }
        ast::StatementKind::Expression(expr) => {
            format_expression(expr, output, context)?;
            output.push(';');
        }
        ast::StatementKind::Var(def) => {
            format_variable_definition(def, output, context)?;
            output.push(';');
        }
        ast::StatementKind::AmbiguousDeclarationOrExpression(..) => {
            return Err(FormatError::AmbiguousParseBranch);
        }
        ast::StatementKind::Block(block) => {
            output.push('{');
            context.push_indent();
            for statement in block {
                format_statement(statement, output, context)?;
            }
            context.pop_indent();
            context.new_line(output);
            output.push('}');
        }
        ast::StatementKind::If(cond, block) => {
            output.push_str("if (");
            format_expression(cond, output, context)?;
            output.push(')');
            format_statement(block, output, context)?;
        }
        ast::StatementKind::IfElse(cond, block_true, block_false) => {
            output.push_str("if (");
            format_expression(cond, output, context)?;
            output.push(')');
            format_statement(block_true, output, context)?;

            context.new_line(output);
            output.push_str("else");
            format_statement(block_false, output, context)?;
        }
        ast::StatementKind::For(init, cond, inc, block) => {
            output.push_str("for (");
            format_for_init(init, output, context)?;
            output.push(';');
            if let Some(cond) = cond {
                output.push(' ');
                format_expression(cond, output, context)?;
            }
            output.push(';');
            if let Some(inc) = inc {
                output.push(' ');
                format_expression(inc, output, context)?;
            }
            output.push(')');

            format_statement(block, output, context)?;
        }
        ast::StatementKind::While(cond, block) => {
            output.push_str("while (");
            format_expression(cond, output, context)?;
            output.push(')');

            format_statement(block, output, context)?;
        }
        ast::StatementKind::DoWhile(block, cond) => {
            output.push_str("do");

            format_statement(block, output, context)?;

            context.new_line(output);
            output.push_str("while (");
            format_expression(cond, output, context)?;
            output.push(')');
            output.push(';');
        }
        ast::StatementKind::Switch(cond, block) => {
            output.push_str("switch (");
            format_expression(cond, output, context)?;
            output.push(')');

            format_statement(block, output, context)?;
        }
        ast::StatementKind::Break => output.push_str("break;"),
        ast::StatementKind::Continue => output.push_str("continue;"),
        ast::StatementKind::Discard => output.push_str("discard;"),
        ast::StatementKind::Return(expr_opt) => {
            output.push_str("return");
            if let Some(expr) = expr_opt {
                output.push(' ');
                format_expression(expr, output, context)?;
            }
            output.push(';');
        }
        ast::StatementKind::CaseLabel(value, next) => {
            output.push_str("case ");
            format_expression(value, output, context)?;
            output.push(':');
            format_statement(next, output, context)?
        }
        ast::StatementKind::DefaultLabel(next) => {
            output.push_str("default:");
            format_statement(next, output, context)?
        }
    }
    Ok(())
}

/// Format a variable definition
fn format_variable_definition(
    def: &ast::VarDef,
    output: &mut String,
    context: &mut FormatContext,
) -> Result<(), FormatError> {
    // Format the part of the type that is shared between declarations
    format_type(&def.local_type, output, context)?;
    format_init_declarators(&def.defs, output, context)?;
    Ok(())
}

/// Format a for init expression
fn format_for_init(
    init: &ast::InitStatement,
    output: &mut String,
    context: &mut FormatContext,
) -> Result<(), FormatError> {
    match init {
        ast::InitStatement::Empty => Ok(()),
        ast::InitStatement::Expression(expr) => format_expression(expr, output, context),
        ast::InitStatement::Declaration(def) => format_variable_definition(def, output, context),
    }
}

/// Format an expression
fn format_expression(
    expr: &ast::Expression,
    output: &mut String,
    context: &mut FormatContext,
) -> Result<(), FormatError> {
    format_subexpression(
        expr,
        u32::max_value(),
        OperatorSide::Middle,
        output,
        context,
    )
}

enum OperatorSide {
    Left,
    Right,
    Middle,
    CommaList,
}

enum Associativity {
    LeftToRight,
    RightToLeft,
    None,
}

/// Format an expression within another expression
fn format_subexpression(
    expr: &ast::Expression,
    outer_precedence: u32,
    side: OperatorSide,
    output: &mut String,
    context: &mut FormatContext,
) -> Result<(), FormatError> {
    let prec = get_expression_precedence(expr)?;
    let requires_paren = match prec.cmp(&outer_precedence) {
        std::cmp::Ordering::Greater => true,
        std::cmp::Ordering::Less => false,
        std::cmp::Ordering::Equal => !matches!(
            (side, get_precedence_associativity(prec)),
            (OperatorSide::Left, Associativity::LeftToRight)
                | (OperatorSide::Right, Associativity::RightToLeft)
                | (OperatorSide::Middle, _)
        ),
    };
    if requires_paren {
        output.push('(')
    }
    match expr {
        ast::Expression::Literal(lit) => format_literal(lit, output, context)?,
        ast::Expression::Identifier(id) => format_scoped_identifier(id, output, context)?,
        ast::Expression::UnaryOperation(op, inner) => {
            let postfix = matches!(
                op,
                ast::UnaryOp::PostfixIncrement | ast::UnaryOp::PostfixDecrement
            );
            if postfix {
                format_subexpression(inner, prec, OperatorSide::Left, output, context)?;
                format_unary_op(op, output)?;
            } else {
                format_unary_op(op, output)?;
                format_subexpression(inner, prec, OperatorSide::Right, output, context)?;
            }
        }
        ast::Expression::BinaryOperation(op, left, right) => {
            format_subexpression(left, prec, OperatorSide::Left, output, context)?;
            if *op != ast::BinOp::Sequence {
                output.push(' ');
            }
            format_bin_op(op, output)?;
            output.push(' ');
            format_subexpression(right, prec, OperatorSide::Right, output, context)?;
        }
        ast::Expression::TernaryConditional(expr_cond, expr_true, expr_false) => {
            format_subexpression(expr_cond, prec, OperatorSide::Left, output, context)?;
            output.push_str(" ? ");
            format_subexpression(expr_true, prec, OperatorSide::Middle, output, context)?;
            output.push_str(" : ");
            format_subexpression(expr_false, prec, OperatorSide::Right, output, context)?;
        }
        ast::Expression::ArraySubscript(expr_object, expr_index) => {
            format_subexpression(expr_object, prec, OperatorSide::Left, output, context)?;
            output.push('[');
            format_subexpression(expr_index, prec, OperatorSide::Middle, output, context)?;
            output.push(']');
        }
        ast::Expression::Cast(ty, expr) => {
            output.push('(');
            format_type(ty, output, context)?;
            output.push(')');
            format_subexpression(expr, prec, OperatorSide::Right, output, context)?;
        }
        ast::Expression::SizeOf(expr) => {
            output.push_str("sizeof(");
            format_expression_or_type(expr, output, context)?;
            output.push(')');
        }
        ast::Expression::Member(expr, name) => {
            format_subexpression(expr, prec, OperatorSide::Left, output, context)?;
            output.push('.');
            format_scoped_identifier(name, output, context)?;
        }
        ast::Expression::Call(object, template_args, args) => {
            format_subexpression(object, 2, OperatorSide::Left, output, context)?;
            format_template_type_args(template_args, output, context)?;
            output.push('(');
            if let Some((last, main)) = args.split_last() {
                for expr in main {
                    format_subexpression(expr, 17, OperatorSide::CommaList, output, context)?;
                    output.push_str(", ");
                }
                format_subexpression(last, 17, OperatorSide::CommaList, output, context)?;
            }
            output.push(')');
        }
        ast::Expression::AmbiguousParseBranch(_) => return Err(FormatError::AmbiguousParseBranch),
    }
    if requires_paren {
        output.push(')')
    }
    Ok(())
}

/// Get the precedence of an expression
/// This is expected to be the same for both RSSL and HLSL
fn get_expression_precedence(expr: &ast::Expression) -> Result<u32, FormatError> {
    let prec = match expr {
        ast::Expression::Literal(_) | ast::Expression::Identifier(_) => 0,
        ast::Expression::UnaryOperation(op, _) => {
            use ast::UnaryOp::*;
            match op {
                PrefixIncrement => 3,
                PrefixDecrement => 3,
                PostfixIncrement => 2,
                PostfixDecrement => 2,
                Plus => 3,
                Minus => 3,
                LogicalNot => 3,
                BitwiseNot => 3,
            }
        }
        ast::Expression::BinaryOperation(op, _, _) => {
            use ast::BinOp::*;
            match op {
                Add => 6,
                Subtract => 6,
                Multiply => 5,
                Divide => 5,
                Modulus => 5,
                LeftShift => 7,
                RightShift => 7,
                BitwiseAnd => 11,
                BitwiseOr => 13,
                BitwiseXor => 12,
                BooleanAnd => 14,
                BooleanOr => 15,
                LessThan => 9,
                LessEqual => 9,
                GreaterThan => 9,
                GreaterEqual => 9,
                Equality => 10,
                Inequality => 10,
                Assignment => 16,
                SumAssignment => 16,
                DifferenceAssignment => 16,
                ProductAssignment => 16,
                QuotientAssignment => 16,
                RemainderAssignment => 16,
                LeftShiftAssignment => 16,
                RightShiftAssignment => 16,
                BitwiseAndAssignment => 16,
                BitwiseOrAssignment => 16,
                BitwiseXorAssignment => 16,
                Sequence => 17,
            }
        }
        ast::Expression::TernaryConditional(_, _, _) => 16,

        ast::Expression::ArraySubscript(_, _) => 2,
        ast::Expression::Member(_, _) => 2,
        ast::Expression::Call(_, _, _) => 2,
        ast::Expression::Cast(_, _) => 3,
        ast::Expression::SizeOf(_) => 3,
        ast::Expression::AmbiguousParseBranch(_) => return Err(FormatError::AmbiguousParseBranch),
    };
    Ok(prec)
}

/// Get the associativity of a precedence level
fn get_precedence_associativity(prec: u32) -> Associativity {
    match prec {
        1 | 2 => Associativity::LeftToRight,
        3 => Associativity::RightToLeft,
        4..=15 => Associativity::LeftToRight,
        16 => Associativity::RightToLeft,
        17 => Associativity::LeftToRight,

        _ => Associativity::None,
    }
}

/// Format an unary op
fn format_unary_op(op: &ast::UnaryOp, output: &mut String) -> Result<(), FormatError> {
    use ast::UnaryOp::*;
    let text = match op {
        PrefixIncrement => "++",
        PrefixDecrement => "--",
        PostfixIncrement => "++",
        PostfixDecrement => "--",
        Plus => "+",
        Minus => "-",
        LogicalNot => "!",
        BitwiseNot => "~",
    };

    output.push_str(text);
    Ok(())
}

/// Format a binary operator
fn format_bin_op(op: &ast::BinOp, output: &mut String) -> Result<(), FormatError> {
    use ast::BinOp::*;
    let text = match op {
        Add => "+",
        Subtract => "-",
        Multiply => "*",
        Divide => "/",
        Modulus => "%",
        LeftShift => "<<",
        RightShift => ">>",
        BitwiseAnd => "&",
        BitwiseOr => "|",
        BitwiseXor => "^",
        BooleanAnd => "&&",
        BooleanOr => "||",
        LessThan => "<",
        LessEqual => "<=",
        GreaterThan => ">",
        GreaterEqual => ">=",
        Equality => "==",
        Inequality => "!=",
        Assignment => "=",
        SumAssignment => "+=",
        DifferenceAssignment => "-=",
        ProductAssignment => "*=",
        QuotientAssignment => "/=",
        RemainderAssignment => "%=",
        LeftShiftAssignment => "<<=",
        RightShiftAssignment => ">>=",
        BitwiseAndAssignment => "&=",
        BitwiseOrAssignment => "|=",
        BitwiseXorAssignment => "^=",
        Sequence => ",",
    };

    output.push_str(text);
    Ok(())
}

/// Format a variable initializer
fn format_initializer(
    init_opt: &Option<ast::Initializer>,
    output: &mut String,
    context: &mut FormatContext,
) -> Result<(), FormatError> {
    if let Some(init) = init_opt {
        output.push_str(" = ");
        format_initializer_inner(init, output, context)?;
    }
    Ok(())
}

/// Internal formatter for variable initializer
fn format_initializer_inner(
    init: &ast::Initializer,
    output: &mut String,
    context: &mut FormatContext,
) -> Result<(), FormatError> {
    match init {
        ast::Initializer::Expression(expr) => format_expression(expr, output, context)?,
        ast::Initializer::Aggregate(exprs) => {
            output.push_str("{ ");
            let (head, tail) = exprs.split_first().unwrap();
            format_initializer_inner(head, output, context)?;
            for expr in tail {
                output.push_str(", ");
                format_initializer_inner(expr, output, context)?;
            }
            output.push_str(" }");
        }
    }
    Ok(())
}

/// Format a template argument list
fn format_template_type_args(
    template_args: &[ast::ExpressionOrType],
    output: &mut String,
    context: &mut FormatContext,
) -> Result<(), FormatError> {
    if let Some((ta_last, ta_main)) = template_args.split_last() {
        output.push('<');
        for ta in ta_main {
            format_expression_or_type(ta, output, context)?;
            output.push_str(", ");
        }
        format_expression_or_type(ta_last, output, context)?;
        output.push('>');
    }
    Ok(())
}

/// Format a struct
fn format_struct(
    def: &ast::StructDefinition,
    output: &mut String,
    context: &mut FormatContext,
) -> Result<(), FormatError> {
    output.push_str("struct ");
    output.push_str(&def.name);

    context.new_line(output);
    output.push('{');
    context.push_indent();

    for entry in &def.members {
        context.new_line(output);

        match entry {
            ast::StructEntry::Variable(member) => {
                format_attributes(&member.attributes, false, output, context)?;
                format_type(&member.ty, output, context)?;
                format_init_declarators(&member.defs, output, context)?;
                output.push(';');
            }
            ast::StructEntry::Method(method) => {
                context.new_line(output);
                format_function(method, output, context)?;
            }
        }
    }

    context.pop_indent();
    context.new_line(output);
    output.push_str("};");

    Ok(())
}

/// Format an enum
fn format_enum(
    def: &ast::EnumDefinition,
    output: &mut String,
    context: &mut FormatContext,
) -> Result<(), FormatError> {
    output.push_str("enum ");
    output.push_str(&def.name);

    context.new_line(output);
    output.push('{');
    context.push_indent();

    for value in &def.values {
        context.new_line(output);
        output.push_str(&value.name);

        if let Some(expr) = &value.value {
            output.push_str(" = ");
            format_expression(expr, output, context)?;
        }

        output.push(',');
    }

    context.pop_indent();
    context.new_line(output);
    output.push_str("};");

    Ok(())
}

/// Format a constant buffer
fn format_constant_buffer(
    def: &ast::ConstantBuffer,
    output: &mut String,
    context: &mut FormatContext,
) -> Result<(), FormatError> {
    format_attributes(&def.attributes, false, output, context)?;

    output.push_str("cbuffer ");
    output.push_str(&def.name);
    format_location_annotations(&def.location_annotations, output)?;

    context.new_line(output);
    output.push('{');
    context.push_indent();

    for member in &def.members {
        context.new_line(output);
        format_type(&member.ty, output, context)?;
        format_init_declarators(&member.defs, output, context)?;
        output.push(';');
    }

    context.pop_indent();
    context.new_line(output);
    output.push('}');

    Ok(())
}

/// Contextual state for the formatter
struct FormatContext {
    indent: u32,
    target: Target,
}

impl FormatContext {
    /// Start a new format state
    fn new(target: Target) -> Self {
        FormatContext { indent: 0, target }
    }

    /// Increase indentation
    fn push_indent(&mut self) {
        self.indent += 1;
    }

    /// Decrease indentation
    fn pop_indent(&mut self) {
        self.indent -= 1;
    }

    /// Begin a new line and indent up to the current level of indentation
    fn new_line(&self, output: &mut String) {
        // Skip new lines when we are starting the file as there is nothing before us to separate from
        if output.is_empty() {
            return;
        }

        // Remove previous indentation on empty lines - or trailing whitespace
        let trimmed = output.trim_end_matches(|c| c == ' ');
        if output.len() != trimmed.len() {
            output.truncate(trimmed.len());
        }

        // Push the newline
        output.push('\n');

        // Indent to current indentation level
        for _ in 0..self.indent {
            output.push_str("    ");
        }
    }
}

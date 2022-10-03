use rssl_ir as ir;
use std::fmt::Write;

/// Error result when exporting to HLSL fails
#[derive(Debug)]
pub enum ExportError {
    NamelessId,
}

/// Export ir module to HLSL
pub fn export_to_hlsl(module: &ir::Module) -> Result<String, ExportError> {
    let mut context = ExportContext::new(module.global_declarations.clone());
    let mut output_string = String::new();

    for root_decl in &module.root_definitions {
        match root_decl {
            ir::RootDefinition::Struct(sd) => {
                export_struct(sd, &mut output_string, &mut context)?;
            }
            ir::RootDefinition::StructTemplate(_) => {
                todo!("RootDefinition::StructTemplate")
            }
            ir::RootDefinition::ConstantBuffer(_) => {
                todo!("RootDefinition::ConstantBuffer")
            }
            ir::RootDefinition::GlobalVariable(decl) => {
                export_global_variable(decl, &mut output_string, &mut context)?;
            }
            ir::RootDefinition::Function(decl) => {
                export_function(decl, &mut output_string, &mut context)?;
                context.new_line(&mut output_string);
            }
        }
    }

    Ok(output_string)
}

/// Export ir global variable to HLSL
fn export_global_variable(
    decl: &ir::GlobalVariable,
    output: &mut String,
    context: &mut ExportContext,
) -> Result<(), ExportError> {
    match decl.global_type.1 {
        ir::GlobalStorage::Extern => output.push_str("extern "),
        ir::GlobalStorage::Static => output.push_str("static "),
        ir::GlobalStorage::GroupShared => output.push_str("groupshared "),
    }

    export_type(&decl.global_type.0, output, context)?;

    if decl.global_type.2.is_some() {
        todo!("Global variable interpolation modifier");
    }

    output.push(' ');

    output.push_str(context.get_global_name(decl.id)?);

    export_initializer(&decl.init, output, context)?;

    output.push_str(";\n");

    Ok(())
}

/// Export ir function to HLSL
fn export_function(
    decl: &ir::FunctionDefinition,
    output: &mut String,
    context: &mut ExportContext,
) -> Result<(), ExportError> {
    if !decl.attributes.is_empty() {
        todo!("Function attributes");
    }

    export_type(&decl.returntype.return_type, output, context)?;
    output.push(' ');

    output.push_str(context.get_function_name(decl.id)?);

    // Scope also contains the names of parameters
    context.push_scope(decl.scope_block.1.clone());
    context.push_indent();

    output.push('(');

    if let Some((last, main)) = decl.params.split_last() {
        for param in main {
            export_function_param(param, output, context)?;
            output.push_str(", ");
        }
        export_function_param(last, output, context)?;
    }

    output.push(')');

    // Function return attribute not supported in ir

    output.push_str(" {");
    for statement in &decl.scope_block.0 {
        export_statement(statement, output, context)?;
    }

    context.pop_scope();
    context.pop_indent();

    if !decl.scope_block.0.is_empty() {
        context.new_line(output);
    }
    output.push('}');

    Ok(())
}

/// Export ir function parameter to HLSL
fn export_function_param(
    param: &ir::FunctionParam,
    output: &mut String,
    context: &mut ExportContext,
) -> Result<(), ExportError> {
    match param.param_type.1 {
        ir::InputModifier::In => {}
        ir::InputModifier::Out => output.push_str("out "),
        ir::InputModifier::InOut => output.push_str("inout "),
    }
    if param.param_type.2.is_some() {
        todo!("Interpolation modifier: {:?}", param.param_type.2);
    }
    export_type(&param.param_type.0, output, context)?;

    output.push(' ');

    output.push_str(context.get_variable_name_direct(param.id)?);

    Ok(())
}

/// Export ir type to HLSL
fn export_type(
    ty: &ir::Type,
    output: &mut String,
    context: &mut ExportContext,
) -> Result<(), ExportError> {
    if !ty.1.is_empty() {
        todo!("Type modifier");
    }

    match ty.0 {
        ir::TypeLayout::Void => write!(output, "void").unwrap(),
        ir::TypeLayout::Scalar(st) => write!(output, "{}", export_scalar_type(st)?).unwrap(),
        ir::TypeLayout::Vector(st, x) => {
            write!(output, "{}{}", export_scalar_type(st)?, x).unwrap()
        }
        ir::TypeLayout::Matrix(st, x, y) => {
            write!(output, "{}{}x{}", export_scalar_type(st)?, x, y).unwrap()
        }
        ir::TypeLayout::Struct(id) => output.push_str(context.get_struct_name(id)?),
        ir::TypeLayout::Object(ir::ObjectType::ByteAddressBuffer) => {
            output.push_str("ByteAddressBuffer");
        }
        _ => todo!("Type layout not implemented"),
    };

    Ok(())
}

/// Export ir scalar type to HLSL
fn export_scalar_type(ty: ir::ScalarType) -> Result<&'static str, ExportError> {
    Ok(match ty {
        ir::ScalarType::Bool => "bool",
        ir::ScalarType::UntypedInt => panic!("Untyped int should not be required on output"),
        ir::ScalarType::Int => "int",
        ir::ScalarType::UInt => "uint",
        ir::ScalarType::Half => "half",
        ir::ScalarType::Float => "float",
        ir::ScalarType::Double => "double",
    })
}

/// Export ir literal to HLSL
fn export_literal(literal: &ir::Literal, output: &mut String) -> Result<(), ExportError> {
    match literal {
        ir::Literal::Bool(true) => output.push_str("true"),
        ir::Literal::Bool(false) => output.push_str("false"),
        // There is no signed integer suffix - but the way we generate code should avoid overload issues with unsigned integers
        ir::Literal::UntypedInt(v) | ir::Literal::Int(v) => write!(output, "{}", v).unwrap(),
        ir::Literal::UInt(v) => write!(output, "{}u", v).unwrap(),
        ir::Literal::Long(v) => write!(output, "{}l", v).unwrap(),
        ir::Literal::Half(v) => write!(output, "{}h", v).unwrap(),
        ir::Literal::Float(v) => write!(output, "{}", v).unwrap(),
        ir::Literal::Double(v) => write!(output, "{}L", v).unwrap(),
    }
    Ok(())
}

/// Export ir statement to HLSL
fn export_statement(
    statement: &ir::Statement,
    output: &mut String,
    context: &mut ExportContext,
) -> Result<(), ExportError> {
    context.new_line(output);
    match statement {
        ir::Statement::Expression(expr) => {
            export_expression(expr, output, context)?;
            output.push(';');
        }
        ir::Statement::Var(def) => {
            export_varible_definition(def, output, context)?;
            output.push(';');
        }
        ir::Statement::Block(block) => {
            enter_scope_block(block, context);
            export_scope_block(block, output, context)?;
            context.pop_scope();
        }
        ir::Statement::If(cond, block) => {
            enter_scope_block(block, context);
            output.push_str("if (");
            export_expression(cond, output, context)?;
            output.push(')');
            context.new_line(output);
            export_scope_block(block, output, context)?;
            context.pop_scope();
        }
        ir::Statement::IfElse(cond, block_true, block_false) => {
            enter_scope_block(block_true, context);
            output.push_str("if (");
            export_expression(cond, output, context)?;
            output.push(')');
            context.new_line(output);
            export_scope_block(block_true, output, context)?;
            context.pop_scope();

            context.new_line(output);
            output.push_str("else");
            context.new_line(output);
            enter_scope_block(block_false, context);
            export_scope_block(block_false, output, context)?;
        }
        ir::Statement::For(init, cond, inc, block) => {
            enter_scope_block(block, context);

            output.push_str("for (");
            export_for_init(init, output, context)?;
            output.push_str("; ");
            export_expression(cond, output, context)?;
            output.push_str("; ");
            export_expression(inc, output, context)?;
            output.push(')');

            context.new_line(output);
            export_scope_block(block, output, context)?;
            context.pop_scope();
        }
        ir::Statement::While(cond, block) => {
            enter_scope_block(block, context);

            output.push_str("while (");
            export_expression(cond, output, context)?;
            output.push(')');

            context.new_line(output);
            export_scope_block(block, output, context)?;
            context.pop_scope();
        }
        ir::Statement::Break => output.push_str("break;"),
        ir::Statement::Continue => output.push_str("continue;"),
        ir::Statement::Return(expr_opt) => {
            output.push_str("return");
            if let Some(expr) = expr_opt {
                output.push(' ');
                export_expression(expr, output, context)?;
            }
            output.push(';');
        }
    }
    Ok(())
}

/// Export ir variable definition to HLSL
fn export_varible_definition(
    def: &ir::VarDef,
    output: &mut String,
    context: &mut ExportContext,
) -> Result<(), ExportError> {
    match def.local_type.1 {
        ir::LocalStorage::Local => {}
        ir::LocalStorage::Static => output.push_str("static "),
    }
    export_type(&def.local_type.0, output, context)?;
    if def.local_type.2.is_some() {
        todo!("Variable interpolation modifier");
    }

    export_varible_definition_no_type(def, output, context)
}

/// Export ir single variable definition to HLSL
fn export_varible_definition_no_type(
    def: &ir::VarDef,
    output: &mut String,
    context: &mut ExportContext,
) -> Result<(), ExportError> {
    output.push(' ');
    output.push_str(context.get_variable_name_direct(def.id)?);

    export_initializer(&def.init, output, context)
}

/// Add scoped variables from a scope block
fn enter_scope_block(block: &ir::ScopeBlock, context: &mut ExportContext) {
    context.push_scope(block.1.clone());
}

/// Export block of ir statements to HLSL
fn export_scope_block(
    block: &ir::ScopeBlock,
    output: &mut String,
    context: &mut ExportContext,
) -> Result<(), ExportError> {
    output.push('{');
    context.push_indent();
    for statement in &block.0 {
        export_statement(statement, output, context)?;
    }
    context.pop_indent();
    context.new_line(output);
    output.push('}');
    Ok(())
}

/// Export ir initialiser expression to HLSL
fn export_for_init(
    init: &ir::ForInit,
    output: &mut String,
    context: &mut ExportContext,
) -> Result<(), ExportError> {
    match init {
        ir::ForInit::Empty => Ok(()),
        ir::ForInit::Expression(expr) => export_expression(expr, output, context),
        ir::ForInit::Definitions(defs) => {
            let (head, tail) = defs.split_first().unwrap();
            export_varible_definition(head, output, context)?;
            for def in tail {
                assert_eq!(head.local_type, def.local_type);
                output.push(',');
                export_varible_definition_no_type(def, output, context)?;
            }
            Ok(())
        }
    }
}

/// Export ir expression to HLSL
fn export_expression(
    expr: &ir::Expression,
    output: &mut String,
    context: &mut ExportContext,
) -> Result<(), ExportError> {
    match expr {
        ir::Expression::Literal(lit) => export_literal(lit, output)?,
        ir::Expression::Variable(v) => output.push_str(context.get_variable_name(*v)?),
        ir::Expression::MemberVariable(name) => output.push_str(name),
        ir::Expression::Global(v) => output.push_str(context.get_global_name(*v)?),
        ir::Expression::Cast(ty, expr) => {
            output.push('(');
            export_type(ty, output, context)?;
            output.push_str(")(");
            export_expression(expr, output, context)?;
            output.push(')');
        }
        ir::Expression::Member(expr, name) => {
            export_expression(expr, output, context)?;
            output.push('.');
            output.push_str(name)
        }
        ir::Expression::Call(id, tys, exprs) => {
            if !tys.is_empty() {
                todo!("Function invocation with type arguments");
            }
            // TODO: Method calls need different syntax when invoked on an object instead of in another method
            output.push_str(context.get_function_name(*id)?);
            output.push('(');
            if let Some((last, main)) = exprs.split_last() {
                for expr in main {
                    export_expression(expr, output, context)?;
                    output.push_str(", ");
                }
                export_expression(last, output, context)?;
            }
            output.push(')');
        }
        ir::Expression::Intrinsic(intrinsic, _, exprs) => {
            enum Form {
                Unary(&'static str),
                UnaryPostfix(&'static str),
                Binary(&'static str),
                Invoke(&'static str),
                Method(&'static str),
            }

            use ir::Intrinsic::*;
            let form = match &intrinsic {
                PrefixIncrement => Form::Unary("++"),
                PrefixDecrement => Form::Unary("--"),
                PostfixIncrement => Form::UnaryPostfix("++"),
                PostfixDecrement => Form::UnaryPostfix("--"),
                Plus => Form::Unary("+"),
                Minus => Form::Unary("-"),
                LogicalNot => Form::Unary("!"),
                BitwiseNot => Form::Unary("~"),

                Add => Form::Binary("+"),
                Subtract => Form::Binary("-"),
                Multiply => Form::Binary("*"),
                Divide => Form::Binary("/"),
                Modulus => Form::Binary("%"),
                LeftShift => Form::Binary("<<"),
                RightShift => Form::Binary(">>"),
                BitwiseAnd => Form::Binary("&"),
                BitwiseOr => Form::Binary("|"),
                BitwiseXor => Form::Binary("^"),
                BooleanAnd => Form::Binary("&&"),
                BooleanOr => Form::Binary("||"),
                LessThan => Form::Binary("<"),
                LessEqual => Form::Binary("<="),
                GreaterThan => Form::Binary(">"),
                GreaterEqual => Form::Binary(">="),
                Equality => Form::Binary("=="),
                Inequality => Form::Binary("!="),
                Assignment => Form::Binary("="),
                SumAssignment => Form::Binary("+="),
                DifferenceAssignment => Form::Binary("-="),
                ProductAssignment => Form::Binary("*="),
                QuotientAssignment => Form::Binary("/="),
                RemainderAssignment => Form::Binary("%="),

                AllMemoryBarrier => Form::Invoke("AllMemoryBarrier"),
                AllMemoryBarrierWithGroupSync => Form::Invoke("AllMemoryBarrierWithGroupSync"),
                DeviceMemoryBarrier => Form::Invoke("DeviceMemoryBarrier"),
                DeviceMemoryBarrierWithGroupSync => {
                    Form::Invoke("DeviceMemoryBarrierWithGroupSync")
                }
                GroupMemoryBarrier => Form::Invoke("GroupMemoryBarrier"),
                GroupMemoryBarrierWithGroupSync => Form::Invoke("GroupMemoryBarrierWithGroupSync"),

                AsInt => Form::Invoke("asint"),
                AsUInt => Form::Invoke("asuint"),
                AsFloat => Form::Invoke("asfloat"),
                AsDouble => Form::Invoke("asdouble"),

                All => Form::Invoke("all"),
                Any => Form::Invoke("any"),

                Abs => Form::Invoke("abs"),

                // Transcendental functions
                Acos => Form::Invoke("acos"),
                Asin => Form::Invoke("asin"),
                Cos => Form::Invoke("cos"),
                Sin => Form::Invoke("sin"),
                Exp => Form::Invoke("exp"),
                Sqrt => Form::Invoke("sqrt"),
                Pow => Form::Invoke("pow"),
                Sincos => Form::Invoke("sincos"),

                F16ToF32 => Form::Invoke("f16tof32"),
                F32ToF16 => Form::Invoke("f32tof16"),

                Floor => Form::Invoke("floor"),

                IsNaN => Form::Invoke("isnan"),

                Length => Form::Invoke("length"),
                Normalize => Form::Invoke("normalize"),

                Saturate => Form::Invoke("saturate"),

                Sign => Form::Invoke("sign"),

                Cross => Form::Invoke("cross"),
                Distance => Form::Invoke("distance"),
                Dot => Form::Invoke("dot"),

                Mul => Form::Invoke("mul"),

                Min => Form::Invoke("min"),
                Max => Form::Invoke("max"),

                Step => Form::Invoke("step"),

                Clamp => Form::Invoke("clamp"),
                Lerp => Form::Invoke("lerp"),
                SmoothStep => Form::Invoke("smoothstep"),

                BufferLoad => Form::Method("Load"),
                RWBufferLoad => Form::Method("Load"),

                StructuredBufferLoad => Form::Method("Load"),
                RWStructuredBufferLoad => Form::Method("Load"),

                ByteAddressBufferLoad => Form::Method("Load"),
                ByteAddressBufferLoad2 => Form::Method("Load2"),
                ByteAddressBufferLoad3 => Form::Method("Load3"),
                ByteAddressBufferLoad4 => Form::Method("Load4"),
                ByteAddressBufferLoadT => Form::Method("Load"),

                RWByteAddressBufferLoad => Form::Method("Load"),
                RWByteAddressBufferLoad2 => Form::Method("Load2"),
                RWByteAddressBufferLoad3 => Form::Method("Load3"),
                RWByteAddressBufferLoad4 => Form::Method("Load4"),
                RWByteAddressBufferStore => Form::Method("Store"),
                RWByteAddressBufferStore2 => Form::Method("Store2"),
                RWByteAddressBufferStore3 => Form::Method("Store3"),
                RWByteAddressBufferStore4 => Form::Method("Store4"),
                RWByteAddressBufferInterlockedAdd => Form::Method("InterlockedAdd"),

                Texture2DLoad => Form::Method("Load"),
                Texture2DSample => Form::Method("Sample"),

                RWTexture2DLoad => Form::Method("Load"),
            };

            match form {
                Form::Unary(s) => {
                    assert_eq!(exprs.len(), 1);
                    output.push_str(s);
                    output.push('(');
                    export_expression(&exprs[0], output, context)?;
                    output.push(')');
                }
                Form::UnaryPostfix(s) => {
                    assert_eq!(exprs.len(), 1);
                    output.push('(');
                    export_expression(&exprs[0], output, context)?;
                    output.push(')');
                    output.push_str(s);
                }
                Form::Binary(s) => {
                    assert_eq!(exprs.len(), 2);
                    output.push('(');
                    export_expression(&exprs[0], output, context)?;
                    output.push_str(") ");
                    output.push_str(s);
                    output.push_str(" (");
                    export_expression(&exprs[1], output, context)?;
                    output.push(')');
                }
                Form::Invoke(s) => {
                    output.push_str(s);
                    output.push('(');
                    if let Some((last, main)) = exprs.split_last() {
                        for expr in main {
                            export_expression(expr, output, context)?;
                            output.push_str(", ");
                        }
                        export_expression(last, output, context)?;
                    }

                    output.push(')');
                }
                Form::Method(s) => {
                    assert!(!exprs.is_empty());
                    export_expression(&exprs[0], output, context)?;
                    output.push('.');
                    output.push_str(s);
                    output.push('(');
                    if let Some((last, main)) = exprs[1..].split_last() {
                        for expr in main {
                            export_expression(expr, output, context)?;
                            output.push_str(", ");
                        }
                        export_expression(last, output, context)?;
                    }

                    output.push(')');
                }
            }
        }
        _ => todo!("Expression: {:?}", expr),
    }
    Ok(())
}

/// Export ir variable initializer to HLSL
fn export_initializer(
    init_opt: &Option<ir::Initializer>,
    output: &mut String,
    context: &mut ExportContext,
) -> Result<(), ExportError> {
    if let Some(init) = init_opt {
        output.push_str(" = ");
        match init {
            ir::Initializer::Expression(expr) => export_expression(expr, output, context)?,
            ir::Initializer::Aggregate(_) => todo!("Aggregate initialisation"),
        }
    }
    Ok(())
}

/// Export ir struct to HLSL
fn export_struct(
    decl: &ir::StructDefinition,
    output: &mut String,
    context: &mut ExportContext,
) -> Result<(), ExportError> {
    output.push_str("struct ");
    output.push_str(context.get_struct_name(decl.id)?);

    context.new_line(output);
    output.push('{');
    context.push_indent();

    for member in &decl.members {
        context.new_line(output);
        export_type(&member.typename, output, context)?;
        output.push(' ');
        output.push_str(&member.name);
        output.push(';');
    }

    for method in &decl.methods {
        context.new_line(output);
        context.new_line(output);
        export_function(method, output, context)?;
    }

    context.pop_indent();
    context.new_line(output);
    output.push_str("};");
    context.new_line(output);

    Ok(())
}

/// Contextual state for exporter
struct ExportContext {
    names: ir::GlobalDeclarations,
    scopes: Vec<ir::ScopedDeclarations>,

    indent: u32,
}

impl ExportContext {
    /// Start a new exporter state
    fn new(global_declarations: ir::GlobalDeclarations) -> Self {
        // TODO: Rename declarations so they are unique
        ExportContext {
            names: global_declarations,
            scopes: Vec::new(),
            indent: 0,
        }
    }

    /// Push a local scope
    fn push_scope(&mut self, scope_declarations: ir::ScopedDeclarations) {
        // TODO: Make names unique
        self.scopes.push(scope_declarations);
    }

    /// Remove a scope previously added with push_scope
    /// Okay to not call this if we hit an error case as we do not expect to recover from errors
    fn pop_scope(&mut self) {
        self.scopes.pop();
    }

    /// Increase indentation
    fn push_indent(&mut self) {
        self.indent += 1;
    }

    /// Decrease indentation
    fn pop_indent(&mut self) {
        self.indent -= 1;
    }

    /// Get the name of a global variable
    fn get_global_name(&self, id: ir::GlobalId) -> Result<&str, ExportError> {
        match self.names.globals.get(&id) {
            Some(name) => Ok(name),
            None => Err(ExportError::NamelessId),
        }
    }

    /// Get the name of a function
    fn get_function_name(&self, id: ir::FunctionId) -> Result<&str, ExportError> {
        match self.names.functions.get(&id) {
            Some(name) => Ok(name),
            None => Err(ExportError::NamelessId),
        }
    }

    /// Get the name of a struct
    fn get_struct_name(&self, id: ir::StructId) -> Result<&str, ExportError> {
        match self.names.structs.get(&id) {
            Some(name) => Ok(name),
            None => Err(ExportError::NamelessId),
        }
    }

    /// Get the name of a local variable
    fn get_variable_name(&self, id_ref: ir::VariableRef) -> Result<&str, ExportError> {
        let scope = &self.scopes[self.scopes.len() - (id_ref.1 .0 as usize) - 1];
        match scope.variables.get(&id_ref.0) {
            Some((name, _)) => Ok(name),
            None => Err(ExportError::NamelessId),
        }
    }

    /// Get the name of a variable declared in the current scope only
    fn get_variable_name_direct(&self, id: ir::VariableId) -> Result<&str, ExportError> {
        self.get_variable_name(ir::VariableRef(id, ir::ScopeRef(0)))
    }

    /// Begin a new line and indent up to the current level of indentation
    fn new_line(&self, output: &mut String) {
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

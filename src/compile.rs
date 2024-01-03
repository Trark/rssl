use crate::*;
use text::CompileErrorExt;

/// Invoke the compiler to build RSSL source into the specified target
pub fn compile(args: CompileArgs) -> Result<Vec<CompiledPipeline>, CompileError> {
    if args.support_buffer_address && !matches!(args.target, Target::HlslForVulkan) {
        return Err(CompileError::InvalidArgs);
    }

    // Make a source manager to control all own all inputs and error locations in the compilation process
    let mut source_manager = text::SourceManager::new();

    let mut defines = Vec::new();

    // We mirror the semantics of HLSL 2021
    defines.push(("__HLSL_VERSION", "2021"));

    defines.push((
        "RSSL_TARGET_HLSL",
        if matches!(args.target, Target::HlslForDirectX | Target::HlslForVulkan) {
            "1"
        } else {
            "0"
        },
    ));

    defines.push((
        "RSSL_TARGET_MSL",
        if matches!(args.target, Target::Msl | Target::MetalBytecode) {
            "1"
        } else {
            "0"
        },
    ));

    // Add user provided defines
    defines.extend(args.defines);

    // Preprocess the file
    let tokens = match preprocess::preprocess(
        args.entry_file_name,
        &mut source_manager,
        args.include_handler,
        &defines,
    ) {
        Ok(tokens) => tokens,
        Err(err) => {
            return Err(CompileError::Text(format!(
                "{}",
                err.display(&source_manager)
            )))
        }
    };

    let tokens = preprocess::prepare_tokens(&tokens);

    // Turn proprocessor tokens into AST
    let pl = match parser::parse(&tokens) {
        Ok(pl) => pl,
        Err(err) => {
            return Err(CompileError::Text(format!(
                "{}",
                err.display(&source_manager)
            )))
        }
    };

    // Turn untyped AST into typed IR
    let ir = match typer::type_check(&pl) {
        Ok(ir) => ir,
        Err(err) => {
            return Err(CompileError::Text(format!(
                "{}",
                err.display(&source_manager)
            )))
        }
    };

    // Pick how we will bind api binding slots based on the target API
    let binding_params = match args.target {
        Target::HlslForDirectX => AssignBindingsParams::default(),
        Target::HlslForVulkan => AssignBindingsParams {
            require_slot_type: false,
            support_buffer_address: args.support_buffer_address,
            metal_slot_layout: false,
        },
        Target::Msl | Target::MetalBytecode => AssignBindingsParams {
            require_slot_type: false,
            support_buffer_address: false,
            metal_slot_layout: true,
        },
    };

    // Process each pipeline declared in the source file separately
    let mut output_pipelines = Vec::new();

    if args.no_pipeline_mode {
        output_pipelines.push(build_pipeline(&args, &ir, &binding_params, None)?);
    } else {
        for pipeline in &ir.pipelines {
            if let Some(name) = args.pipeline_name {
                if pipeline.name.node != name {
                    continue;
                }
            }

            output_pipelines.push(build_pipeline(&args, &ir, &binding_params, Some(pipeline))?);
        }
    }

    if let Some(name) = args.pipeline_name {
        if output_pipelines.len() > 1 {
            panic!("Multiple pipelines with the given name: {}", name);
        }

        if output_pipelines.is_empty() {
            return Err(CompileError::Text(format!(
                "Shader does not contain the pipeline: {}",
                name
            )));
        }
    } else if output_pipelines.is_empty() && !args.no_pipeline_mode {
        return Err(CompileError::Text(String::from(
            "Shader does not contain a single pipeline",
        )));
    }

    Ok(output_pipelines)
}

/// Build a single pipeline in a module
fn build_pipeline(
    args: &CompileArgs,
    ir: &ir::Module,
    binding_params: &AssignBindingsParams,
    pipeline: Option<&ir::PipelineDefinition>,
) -> Result<CompiledPipeline, CompileError> {
    // Clone the ir to process the selected pipeline independently
    // The original ir is still references for pipeline metadata - which we do not expect to change
    let ir = ir.clone();

    let ir = if let Some(pipeline) = pipeline {
        // Select the current pipeline
        ir.select_pipeline(&pipeline.name).unwrap()
    } else {
        ir
    };

    // Set the binding slots for global parameters
    let ir = ir.assign_api_bindings(binding_params);

    let graphics_pipeline_state = if let Some(pipeline) = pipeline {
        pipeline.graphics_pipeline_state.clone()
    } else {
        None
    };

    let thread_group_size = pipeline.and_then(|pipeline| pipeline.thread_group_size);

    let compiled = match args.target {
        Target::HlslForDirectX | Target::HlslForVulkan => {
            let exported_source = match hlsl::export_to_hlsl(&ir) {
                Ok(exported_source) => exported_source,
                Err(err) => panic!("{err:?}"),
            };

            let mut stages = Vec::new();
            if let Some(pipeline) = pipeline {
                for stage in &pipeline.stages {
                    stages.push(CompiledPipelineStage {
                        stage: stage.stage,
                        // TODO: The entry point will probably be generated with the same name but it is not guaranteed
                        entry_point: ir
                            .function_registry
                            .get_function_name(stage.entry_point)
                            .to_string(),
                    });
                }
            }

            CompiledPipeline {
                data: exported_source.source.into_bytes(),
                stages,
                metadata: exported_source.pipeline_description,
                graphics_pipeline_state,
                thread_group_size,
            }
        }
        Target::Msl | Target::MetalBytecode => {
            let exported_source = match msl::export_to_msl(&ir) {
                Ok(exported_source) => exported_source,
                Err(err) => panic!("{err:?}"),
            };

            let mut stages = Vec::new();
            if let Some(pipeline) = pipeline {
                for stage in &pipeline.stages {
                    stages.push(CompiledPipelineStage {
                        stage: stage.stage,
                        // TODO: Avoid relying on internal generation details
                        entry_point: String::from(match stage.stage {
                            ShaderStage::Pixel => "PixelShaderEntry",
                            ShaderStage::Vertex => "VertexShaderEntry",
                            ShaderStage::Compute => "ComputeShaderEntry",
                            ShaderStage::Mesh => "MeshShaderEntry",
                            ShaderStage::Task => "TaskShaderEntry",
                        }),
                    });
                }
            }

            let data = if matches!(args.target, Target::MetalBytecode) {
                let native_compiler = match metal_invoker::MetalCompiler::find() {
                    Ok(compiler) => compiler,
                    Err(err) => return Err(CompileError::MetalCompilerNotFound(err)),
                };

                let execute_result = native_compiler.execute(
                    &exported_source.source,
                    &["-std=metal3.1", "-include", "metal_stdlib"],
                );

                match execute_result {
                    Ok(data) => data,
                    Err(err) => return Err(CompileError::MetalCompilerFailed(err)),
                }
            } else {
                exported_source.source.into_bytes()
            };

            CompiledPipeline {
                data,
                stages,
                metadata: exported_source.pipeline_description,
                graphics_pipeline_state,
                thread_group_size,
            }
        }
    };

    Ok(compiled)
}

/// Output of a compiled shader pipeline
pub struct CompiledPipeline {
    /// Generated source or bytecode for the pipeline
    pub data: Vec<u8>,

    /// Data for each stage in the pipeline
    pub stages: Vec<CompiledPipelineStage>,

    /// Reflection metadata for the pipeline
    pub metadata: PipelineDescription,

    /// State for graphics pipelines
    pub graphics_pipeline_state: Option<ir::GraphicsPipelineState>,

    /// The number of threads per thread group if the shader has a set size
    pub thread_group_size: Option<(u32, u32, u32)>,
}

/// Output for a shader stage in a compiled shader pipeline
pub struct CompiledPipelineStage {
    /// Shader type
    pub stage: ShaderStage,

    /// Name of the entry point
    pub entry_point: String,
}

/// Error for [compile()]
#[derive(Debug)]
pub enum CompileError {
    Text(String),
    InvalidArgs,
    MetalCompilerNotFound(metal_invoker::FindError),
    MetalCompilerFailed(metal_invoker::ExecuteError),
}

/// Arguments for [compile()]
pub struct CompileArgs<'a> {
    entry_file_name: &'a str,
    include_handler: &'a mut dyn text::IncludeHandler,
    defines: &'a [(&'a str, &'a str)],
    target: Target,
    support_buffer_address: bool,
    pipeline_name: Option<&'a str>,
    no_pipeline_mode: bool,
}

impl<'a> CompileArgs<'a> {
    /// Create new args with required arguments
    pub fn new(
        entry_file_name: &'a str,
        include_handler: &'a mut dyn text::IncludeHandler,
        target: Target,
    ) -> Self {
        CompileArgs {
            entry_file_name,
            include_handler,
            defines: &[],
            target,
            support_buffer_address: false,
            pipeline_name: None,
            no_pipeline_mode: false,
        }
    }

    /// Set the initial defines
    pub fn defines(mut self, defines: &'a [(&str, &str)]) -> Self {
        self.defines = defines;
        self
    }

    /// Enable or disable ability to generate native buffer address
    pub fn support_buffer_address(mut self, enabled: bool) -> Self {
        self.support_buffer_address = enabled;
        self
    }

    /// Restrict built pipeline to only the given name if non-none
    pub fn pipeline_name(mut self, name: Option<&'a str>) -> Self {
        self.pipeline_name = name;
        self
    }

    /// Enable mode where shader sources can be generated without any pipeline definitions for testing
    pub fn no_pipeline_mode(mut self) -> Self {
        self.no_pipeline_mode = true;
        self
    }
}

#[derive(Copy, Clone, Default)]
pub enum Target {
    /// Generate HLSL to be compiled with DXC for DirectX
    #[default]
    HlslForDirectX,

    /// Generate HLSL to be compiled with DXC for Vulkan
    HlslForVulkan,

    /// Generate Metal shading language source to be compiled with metal shader compiler
    Msl,

    /// Generate Metal bytecode
    MetalBytecode,
}

impl std::fmt::Display for CompileError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            CompileError::Text(s) => write!(f, "{}", s),
            CompileError::MetalCompilerFailed(metal_invoker::ExecuteError::CompileError(s)) => {
                write!(f, "{}", s)
            }
            _ => write!(f, "{:?}", self),
        }
    }
}

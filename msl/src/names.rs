pub use rssl_ir::name_generator::{NameMap, NameSymbol, ScopedName};

pub const HELPER_NAMESPACE_NAME: &str = "helper";
pub const ENTRY_POINT_NAME_COMPUTE: &str = "ComputeShaderEntry";
pub const ENTRY_POINT_NAME_PIXEL: &str = "PixelShaderEntry";
pub const ENTRY_POINT_NAME_VERTEX: &str = "VertexShaderEntry";
pub const ENTRY_POINT_NAME_MESH: &str = "MeshShaderEntry";
pub const ENTRY_POINT_NAME_TASK: &str = "TaskShaderEntry";
pub const ARGUMENT_BUFFER_0_NAME: &str = "ArgumentBuffer0";
pub const ARGUMENT_BUFFER_1_NAME: &str = "ArgumentBuffer1";
pub const ARGUMENT_BUFFER_2_NAME: &str = "ArgumentBuffer2";
pub const ARGUMENT_BUFFER_3_NAME: &str = "ArgumentBuffer3";
pub const STAGE_OUTPUT_NAME_VERTEX: &str = "VertexOutput";
pub const STAGE_OUTPUT_NAME_PIXEL: &str = "PixelOutput";
pub const STAGE_OUTPUT_NAME_LOCAL: &str = "out";
pub const STAGE_INPUT_NAME_LOCAL: &str = "in";

/// List of names we want to avoid in the generated code
pub const RESERVED_NAMES: &[&str] = &[
    "and",
    "as_type",
    "auto",
    "break",
    "case",
    "catch",
    "char",
    "class",
    "const",
    "const_cast",
    "constexpr",
    "continue",
    "decltype",
    "default",
    "delete",
    "do",
    "dynamic_cast",
    "else",
    "enum",
    "explicit",
    "extern",
    "false",
    "float",
    "float16_t",
    "friend",
    "goto",
    "half",
    "if",
    "inline",
    "int",
    "int64_t",
    "long",
    "main",
    "mutable",
    "namespace",
    "new",
    "operator",
    "or",
    "private",
    "protected",
    "public",
    "reinterpret_cast",
    "return",
    "short",
    "signed",
    "sizeof",
    "static",
    "static_cast",
    "struct",
    "switch",
    "template",
    "this",
    "throw",
    "true",
    "try",
    "typedef",
    "typename",
    "uint",
    "uint64_t",
    "union",
    "unsigned",
    "using",
    "virtual",
    "void",
    "volatile",
    "while",
    // Keywords to mark entry points (although modern code uses attributes these still exist)
    "kernel",
    "vertex",
    "fragment",
    // Reserve the name of the standard library namespace everywhere
    "metal",
    // Names used by symbols generated in the generator
    HELPER_NAMESPACE_NAME,
    // Names used for entry point generation
    ENTRY_POINT_NAME_COMPUTE,
    ENTRY_POINT_NAME_PIXEL,
    ENTRY_POINT_NAME_VERTEX,
    ENTRY_POINT_NAME_MESH,
    ENTRY_POINT_NAME_TASK,
    ARGUMENT_BUFFER_0_NAME,
    ARGUMENT_BUFFER_1_NAME,
    ARGUMENT_BUFFER_2_NAME,
    ARGUMENT_BUFFER_3_NAME,
    STAGE_OUTPUT_NAME_VERTEX,
    STAGE_OUTPUT_NAME_PIXEL,
    STAGE_OUTPUT_NAME_LOCAL,
    STAGE_INPUT_NAME_LOCAL,
];

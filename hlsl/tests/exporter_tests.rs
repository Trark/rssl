mod shared;
use shared::*;

#[test]
fn check_static_primitive_variables() {
    check_rssl_to_hlsl("int x = 0;", "int x = 0;\n");
    check_rssl_to_hlsl("static int x = 1;", "static int x = 1;\n");
    check_rssl_to_hlsl("static const int x = 1;", "static const int x = 1;\n");
    check_rssl_to_hlsl(
        "static const int x = 1.0;",
        "static const int x = (int)1.0;\n",
    );
    check_rssl_to_hlsl("extern int x = -1;", "int x = (int)-1;\n");
    check_rssl_to_hlsl("static float x[4];", "static float x[4];\n");
    check_rssl_to_hlsl(
        "static float x[4] = { 0.0, 1.0, 2.0, 3.0 };",
        "static float x[4] = { 0.0, 1.0, 2.0, 3.0 };\n",
    );
    check_rssl_to_hlsl(
        "static float x[2][3], y[3][4];",
        "static float x[2][3];\nstatic float y[3][4];\n",
    );
    check_rssl_to_hlsl(
        "static const int data[2][3] = { { 0, 1 }, { 2, 3 }, { 4, 5 } };",
        "static const int data[2][3] = { { 0, 1 }, { 2, 3 }, { 4, 5 } };\n",
    );
}

#[test]
fn check_primitive_types() {
    check_rssl_to_hlsl("float x;", "float x;\n");
    check_rssl_to_hlsl("float1 x;", "float1 x;\n");
    check_rssl_to_hlsl("float2 x;", "float2 x;\n");
    check_rssl_to_hlsl("float3 x;", "float3 x;\n");
    check_rssl_to_hlsl("float4 x;", "float4 x;\n");

    check_rssl_to_hlsl("float1x1 x;", "float1x1 x;\n");
    check_rssl_to_hlsl("float2x1 x;", "float2x1 x;\n");
    check_rssl_to_hlsl("float3x1 x;", "float3x1 x;\n");
    check_rssl_to_hlsl("float4x1 x;", "float4x1 x;\n");

    check_rssl_to_hlsl("float1x2 x;", "float1x2 x;\n");
    check_rssl_to_hlsl("float2x2 x;", "float2x2 x;\n");
    check_rssl_to_hlsl("float3x2 x;", "float3x2 x;\n");
    check_rssl_to_hlsl("float4x2 x;", "float4x2 x;\n");

    check_rssl_to_hlsl("float1x3 x;", "float1x3 x;\n");
    check_rssl_to_hlsl("float2x3 x;", "float2x3 x;\n");
    check_rssl_to_hlsl("float3x3 x;", "float3x3 x;\n");
    check_rssl_to_hlsl("float4x3 x;", "float4x3 x;\n");

    check_rssl_to_hlsl("float1x4 x;", "float1x4 x;\n");
    check_rssl_to_hlsl("float2x4 x;", "float2x4 x;\n");
    check_rssl_to_hlsl("float3x4 x;", "float3x4 x;\n");
    check_rssl_to_hlsl("float4x4 x;", "float4x4 x;\n");

    check_rssl_to_hlsl("int x;", "int x;\n");
    check_rssl_to_hlsl("int1 x;", "int1 x;\n");
    check_rssl_to_hlsl("int2 x;", "int2 x;\n");
    check_rssl_to_hlsl("int3 x;", "int3 x;\n");
    check_rssl_to_hlsl("int4 x;", "int4 x;\n");

    check_rssl_to_hlsl("uint x;", "uint x;\n");
    check_rssl_to_hlsl("uint1 x;", "uint1 x;\n");
    check_rssl_to_hlsl("uint2 x;", "uint2 x;\n");
    check_rssl_to_hlsl("uint3 x;", "uint3 x;\n");
    check_rssl_to_hlsl("uint4 x;", "uint4 x;\n");

    check_rssl_to_hlsl("half x;", "half x;\n");
    check_rssl_to_hlsl("half1 x;", "half1 x;\n");
    check_rssl_to_hlsl("half2 x;", "half2 x;\n");
    check_rssl_to_hlsl("half3 x;", "half3 x;\n");
    check_rssl_to_hlsl("half4 x;", "half4 x;\n");

    check_rssl_to_hlsl("vector<float, 1> x;", "float1 x;\n");
    check_rssl_to_hlsl("vector<float, 2> x;", "float2 x;\n");
    check_rssl_to_hlsl("vector<float, 3> x;", "float3 x;\n");
    check_rssl_to_hlsl("vector<float, 4> x;", "float4 x;\n");

    check_rssl_to_hlsl("matrix<uint, 1, 1> x;", "uint1x1 x;\n");
    check_rssl_to_hlsl("matrix<uint, 2, 1> x;", "uint2x1 x;\n");
    check_rssl_to_hlsl("matrix<uint, 3, 1> x;", "uint3x1 x;\n");
    check_rssl_to_hlsl("matrix<uint, 4, 1> x;", "uint4x1 x;\n");

    check_rssl_to_hlsl("matrix<int, 1, 2> x;", "int1x2 x;\n");
    check_rssl_to_hlsl("matrix<int, 2, 2> x;", "int2x2 x;\n");
    check_rssl_to_hlsl("matrix<int, 3, 2> x;", "int3x2 x;\n");
    check_rssl_to_hlsl("matrix<int, 4, 2> x;", "int4x2 x;\n");

    check_rssl_to_hlsl("matrix<half, 1, 3> x;", "half1x3 x;\n");
    check_rssl_to_hlsl("matrix<half, 2, 3> x;", "half2x3 x;\n");
    check_rssl_to_hlsl("matrix<half, 3, 3> x;", "half3x3 x;\n");
    check_rssl_to_hlsl("matrix<half, 4, 3> x;", "half4x3 x;\n");

    check_rssl_to_hlsl("matrix<double, 1, 4> x;", "double1x4 x;\n");
    check_rssl_to_hlsl("matrix<double, 2, 4> x;", "double2x4 x;\n");
    check_rssl_to_hlsl("matrix<double, 3, 4> x;", "double3x4 x;\n");
    check_rssl_to_hlsl("matrix<double, 4, 4> x;", "double4x4 x;\n");
}

#[test]
fn check_float_literal() {
    check_rssl_to_hlsl("static float x = 0;", "static float x = 0.0;\n");
    check_rssl_to_hlsl("static float x = 1;", "static float x = 1.0;\n");
    check_rssl_to_hlsl("static float x = -1.0;", "static float x = -1.0;\n");
    check_rssl_to_hlsl("static float x = 0.5;", "static float x = 0.5;\n");
    check_rssl_to_hlsl(
        "static float x = 3.402823466e+38f;",
        "static float x = 340282350000000000000000000000000000000.0;\n",
    );
    check_rssl_to_hlsl(
        "static float x = -3.402823466e+38f;",
        "static float x = -340282350000000000000000000000000000000.0;\n",
    );
    check_rssl_to_hlsl(
        "static float x = 1.175494351e-38f;",
        "static float x = 0.000000000000000000000000000000000000011754944;\n",
    );
    check_rssl_to_hlsl(
        "static float x = -1.175494351e-38f;",
        "static float x = -0.000000000000000000000000000000000000011754944;\n",
    );
}

#[test]
fn check_functions() {
    check_rssl_to_hlsl("void f() {}", "void f() {}\n");
    check_rssl_to_hlsl("void f(int x) {}", "void f(int x) {}\n");
    check_rssl_to_hlsl(
        "float f(int x, float y) { return x + y; }",
        "float f(int x, float y) {
    return (float)x + y;
}
",
    );
}

#[test]
fn check_function_attributes() {
    check_rssl_to_hlsl(
        "[numthreads(64, 1, 1)] void Main() {}",
        "[numthreads(64, 1, 1)]\nvoid Main() {}\n",
    );

    check_rssl_to_hlsl(
        "[WaveSize(64)] void Main() {}",
        "[WaveSize(64)]\nvoid Main() {}\n",
    );

    check_rssl_to_hlsl(
        "[wavesize(64)] void Main() {}",
        "[WaveSize(64)]\nvoid Main() {}\n",
    );

    check_rssl_to_hlsl(
        "[WaveSize(64)] [numthreads(64, 1, 1)] void Main() {}",
        "[WaveSize(64)]\n[numthreads(64, 1, 1)]\nvoid Main() {}\n",
    );
}

#[test]
fn check_expressions() {
    check_rssl_to_hlsl(
        "void f() {
    float4 v0 = float4(1, 2, 3, 4);
    float4 v1 = true ? v0 : 0;
    float4x4 m = float4x4(v0, v0, v1, v1);
    float4 v2 = mul(m, v0);
}
",
        "void f() {
    float4 v0 = float4(1.0, 2.0, 3.0, 4.0);
    float4 v1 = true ? v0 : (float4)0;
    float4x4 m = float4x4(v0, v0, v1, v1);
    float4 v2 = mul(m, v0);
}
",
    );

    check_rssl_to_hlsl(
        "void f() {
    uint a[2];
    a[0];
    a[0] = a[1];
}",
        "void f() {
    uint a[2];
    a[0u];
    a[0u] = a[1u];
}
",
    );

    // Check ?: chain emits with the same lack of parenthesis
    check_rssl_to_hlsl(
        "void f() {
    bool a, b, c, d, e, f, g;
    a ? b ? c : d : e ? f : g;
}
",
        "void f() {
    bool a;
    bool b;
    bool c;
    bool d;
    bool e;
    bool f;
    bool g;
    a ? b ? c : d : e ? f : g;
}
",
    );

    // Check comma operator exports with correct precedence
    check_rssl_to_hlsl(
        "void g(float x) {}
void f() {
    float a, b, c;
    a, b, c;
    (a, b), c;
    a, (b, c);
    g((a, b, c));
    g(((a, b), c));
    g((a, (b, c)));
}
",
        "void g(float x) {}

void f() {
    float a;
    float b;
    float c;
    a, b, c;
    a, b, c;
    a, (b, c);
    g((a, b, c));
    g((a, b, c));
    g((a, (b, c)));
}
",
    );

    // Check various arithmetic chains give the expected optimal lack of parenthesis
    check_rssl_to_hlsl(
        "void f() {
    uint a, b, c, d, e, f, g;
    a + b * c / e % f - g;
    (a + b) * c / (e % f) - g;
    a + b * (c / e) % (f - g);
    a + (b * c) / e % (f - g);
    (a + (((b * c) / e) % f)) - g;
    a >> b & c;
    a >> (b & c);
    (a >> b) & c;
    a += b += c * d;
    a += (b += c * d);
    (a += b) += c * d;
    a += (b += c) * d;
}
",
        "void f() {
    uint a;
    uint b;
    uint c;
    uint d;
    uint e;
    uint f;
    uint g;
    a + b * c / e % f - g;
    (a + b) * c / (e % f) - g;
    a + b * (c / e) % (f - g);
    a + b * c / e % (f - g);
    a + b * c / e % f - g;
    a >> b & c;
    a >> (b & c);
    a >> b & c;
    a += b += c * d;
    a += b += c * d;
    (a += b) += c * d;
    a += (b += c) * d;
}
",
    );
}

#[test]
fn check_expression_vector_swizzle() {
    check_rssl_to_hlsl(
        "void f() {
    float4 v0 = float4(1, 2, 3, 4);
    float4 v1 = v0.xyzw;
    float4 v2 = v0.wwww;
}
",
        "void f() {
    float4 v0 = float4(1.0, 2.0, 3.0, 4.0);
    float4 v1 = v0.xyzw;
    float4 v2 = v0.wwww;
}
",
    );
}

#[test]
fn check_expression_vector_index() {
    check_rssl_to_hlsl(
        "void f() {
    float4 v0 = float4(1, 2, 3, 4);
    float v1 = v0[0];
    float v2 = v0[3];
}
",
        "void f() {
    float4 v0 = float4(1.0, 2.0, 3.0, 4.0);
    float v1 = v0[0u];
    float v2 = v0[3u];
}
",
    );
}

#[test]
fn check_statement_block() {
    check_rssl_to_hlsl(
        "void f() {
    float x = 0.0;
    {
        float y = 6.0;
        float z = y + x;
    }
}",
        "void f() {
    float x = 0.0;
    {
        float y = 6.0;
        float z = y + x;
    }
}
",
    );
}

#[test]
fn check_statement_if() {
    check_rssl_to_hlsl(
        "float f() {
    float x = 0.0;
    if (x)
    {
        return 1.0;
    }
    return 2.0;
}",
        "float f() {
    float x = 0.0;
    if (x)
    {
        return 1.0;
    }
    return 2.0;
}
",
    );
}

#[test]
fn check_statement_if_else() {
    check_rssl_to_hlsl(
        "float f() {
    float x = 0.0;
    if (x)
    {
        return 1.0;
    }
    else
    {
        return 3.0;
    }
    return 2.0;
}",
        "float f() {
    float x = 0.0;
    if (x)
    {
        return 1.0;
    }
    else
    {
        return 3.0;
    }
    return 2.0;
}
",
    );
}

#[test]
fn check_statement_if_else_if_else() {
    check_rssl_to_hlsl(
        "float f() {
    float x = 0.0;
    if (x)
    {
        return 1.0;
    }
    else if (x > 2.0)
    {
        return 4.0;
    }
    else
    {
        return 3.0;
    }
    return 2.0;
}",
        "float f() {
    float x = 0.0;
    if (x)
    {
        return 1.0;
    }
    else
    {
        if (x > 2.0)
        {
            return 4.0;
        }
        else
        {
            return 3.0;
        }
    }
    return 2.0;
}
",
    );
}

#[test]
fn check_statement_for() {
    check_rssl_to_hlsl(
        "void f() {
    for (int x = 1, y = 2; x < 10; ++x)
    {
        return;
    }
}",
        "void f() {
    for (int x = 1, y = 2; x < 10; ++x)
    {
        return;
    }
}
",
    );

    check_rssl_to_hlsl(
        "void f() {
    for (int x[2] = { 1, 5 }, y[3][2] = { { 2, 3, 4 }, { 5, 6, 7 } }; true; true)
    {
        break;
    }
}",
        "void f() {
    for (int x[2] = { 1, 5 }, y[3][2] = { { 2, 3, 4 }, { 5, 6, 7 } }; true; true)
    {
        break;
    }
}
",
    );
}

#[test]
fn check_statement_while() {
    check_rssl_to_hlsl(
        "void f() {
    while (true)
    {
        continue;
        break;
        discard;
        return;
    }
}",
        "void f() {
    while (true)
    {
        continue;
        break;
        discard;
        return;
    }
}
",
    );
}

#[test]
fn check_struct() {
    check_rssl_to_hlsl(
        "struct S
{
    int x;
    float y : USER0;
    float z[2], w[3][4] : USER1;

    void f(int z) {}
    void g() { f(x); }
};

void main() {
    S s;
    s.x = 5;
    s.::S::x = 4;
    s.g();
}
",
        "struct S
{
    int x;
    float y : USER0;
    float z[2];
    float w[3][4] : USER1;

    void f(int z) {}

    void g() {
        f(x);
    }
};

void main() {
    S s;
    s.x = 5;
    s.x = 4;
    s.g();
}
",
    );
}

#[test]
fn check_typedef() {
    // Check we can declare and use a typedef - the type will be substituted at site of use.
    check_rssl_to_hlsl("typedef int CustomInt; CustomInt x = 0;", "int x = 0;\n");

    // Check this works correctly with array types
    check_rssl_to_hlsl(
        "typedef int CustomInt[4]; CustomInt x = { 0, 0, 0, 0 };",
        "int x[4] = { 0, 0, 0, 0 };\n",
    );
}

#[test]
fn check_intrinsic_functions() {
    // Check various intrinsic functions
    // As these are basically the same between RSSL and HLSL and there are no resources
    // We expect the source file and output files to be the same

    check_rssl_to_hlsl(
        include_str!("intrinsic_functions.rssl"),
        include_str!("intrinsic_functions.rssl"),
    );

    check_rssl_to_hlsl_vk(
        include_str!("intrinsic_functions.rssl"),
        include_str!("intrinsic_functions.rssl"),
    );
}

#[test]
fn check_constant_buffer() {
    check_rssl_to_hlsl(
        "cbuffer GlobalConstants
{
    float4 v0;
    uint4 v1;
    float4x4 m2;
    float a3[2], a4[3][4];
}

void main() {
    v0;
    v1.wwww;
    m2;
}
",
        "cbuffer GlobalConstants
{
    float4 v0;
    uint4 v1;
    float4x4 m2;
    float a3[2];
    float a4[3][4];
}

void main() {
    v0;
    v1.wwww;
    m2;
}
",
    );
}

#[test]
fn check_object_types() {
    check_rssl_to_hlsl(
        include_str!("object_types.rssl"),
        include_str!("object_types.hlsl"),
    );

    check_rssl_to_hlsl_vk(
        include_str!("object_types.rssl"),
        include_str!("object_types.vk.hlsl"),
    );
}

#[test]
fn check_unorm_snorm() {
    check_rssl_to_hlsl(
        "Buffer<unorm float4> g_buffer;",
        "Buffer<unorm float4> g_buffer;\n",
    );

    check_rssl_to_hlsl(
        "Buffer<snorm float4> g_buffer;",
        "Buffer<snorm float4> g_buffer;\n",
    );
}

#[test]
fn check_object_intrinsics() {
    check_rssl_to_hlsl(
        "ByteAddressBuffer g_buffer; void f() { g_buffer.Load(0); }",
        "ByteAddressBuffer g_buffer;

void f() {
    g_buffer.Load(0u);
}
",
    );

    check_rssl_to_hlsl(
        "const ByteAddressBuffer g_buffer; void f() { g_buffer.Load(0); }",
        "ByteAddressBuffer g_buffer;

void f() {
    g_buffer.Load(0u);
}
",
    );

    check_rssl_to_hlsl(
        "BufferAddress g_buffer; void f() { g_buffer.Load<uint>(0); }",
        "ByteAddressBuffer g_buffer;

void f() {
    g_buffer.Load<uint>(0u);
}
",
    );

    check_rssl_to_hlsl(
        "RWStructuredBuffer<uint> g_buffer; void f() { g_buffer[0]; g_buffer[0] = 1; }",
        "RWStructuredBuffer<uint> g_buffer;

void f() {
    g_buffer[0u];
    g_buffer[0u] = 1u;
}
",
    );
}

#[test]
fn check_byte_buffer() {
    check_rssl_to_hlsl(
        include_str!("byte_buffer.rssl"),
        include_str!("byte_buffer.hlsl"),
    );
}

#[test]
fn check_buffer_address() {
    check_rssl_to_hlsl(
        include_str!("buffer_address.rssl"),
        include_str!("buffer_address.hlsl"),
    );

    check_rssl_to_hlsl_vk(
        include_str!("buffer_address.rssl"),
        include_str!("buffer_address.vk.hlsl"),
    );
}

#[test]
fn check_structured_buffer() {
    check_rssl_to_hlsl(
        include_str!("structured_buffer.rssl"),
        include_str!("structured_buffer.hlsl"),
    );
}

#[test]
fn check_texel_buffer() {
    check_rssl_to_hlsl(
        include_str!("texel_buffer.rssl"),
        include_str!("texel_buffer.hlsl"),
    );
}

#[test]
fn check_texture2d() {
    check_rssl_to_hlsl(
        include_str!("texture2d.rssl"),
        include_str!("texture2d.hlsl"),
    );
}

#[test]
fn check_semantics() {
    check_rssl_to_hlsl(
        "float4 main(uint id : SV_VertexID) : SV_Position { return 0; }",
        "float4 main(uint id : SV_VertexID) : SV_Position {
    return (float4)0;
}
",
    );

    check_rssl_to_hlsl(
        "float4 main(float4 coord : SV_Position) : SV_Target { return 0; }",
        "float4 main(float4 coord : SV_Position) : SV_Target0 {
    return (float4)0;
}
",
    );
}

#[test]
fn check_namespaces() {
    check_rssl_to_hlsl(
        include_str!("namespace.rssl"),
        include_str!("namespace.hlsl"),
    );
}

#[test]
fn check_cluster_expand() {
    check_rssl_to_hlsl(
        include_str!("cluster_expand.rssl"),
        include_str!("cluster_expand.hlsl"),
    );

    check_rssl_to_hlsl_vk(
        include_str!("cluster_expand.rssl"),
        include_str!("cluster_expand.vk.hlsl"),
    );
}

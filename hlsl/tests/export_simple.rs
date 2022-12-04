mod shared;
use shared::*;

#[test]
fn check_static_primitive_variables() {
    check_rssl_to_hlsl("int x = 0;", "extern int x = 0;\n");
    check_rssl_to_hlsl("static int x = 1;", "static int x = 1;\n");
    check_rssl_to_hlsl("static const int x = 1;", "static const int x = 1;\n");
    check_rssl_to_hlsl(
        "static const int x = 1.0;",
        "static const int x = (int)1.0;\n",
    );
    check_rssl_to_hlsl("extern int x = -1;", "extern int x = (int)-1;\n");
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
    check_rssl_to_hlsl("float x;", "extern float x;\n");
    check_rssl_to_hlsl("float1 x;", "extern float1 x;\n");
    check_rssl_to_hlsl("float2 x;", "extern float2 x;\n");
    check_rssl_to_hlsl("float3 x;", "extern float3 x;\n");
    check_rssl_to_hlsl("float4 x;", "extern float4 x;\n");

    check_rssl_to_hlsl("float1x1 x;", "extern float1x1 x;\n");
    check_rssl_to_hlsl("float2x1 x;", "extern float2x1 x;\n");
    check_rssl_to_hlsl("float3x1 x;", "extern float3x1 x;\n");
    check_rssl_to_hlsl("float4x1 x;", "extern float4x1 x;\n");

    check_rssl_to_hlsl("float1x2 x;", "extern float1x2 x;\n");
    check_rssl_to_hlsl("float2x2 x;", "extern float2x2 x;\n");
    check_rssl_to_hlsl("float3x2 x;", "extern float3x2 x;\n");
    check_rssl_to_hlsl("float4x2 x;", "extern float4x2 x;\n");

    check_rssl_to_hlsl("float1x3 x;", "extern float1x3 x;\n");
    check_rssl_to_hlsl("float2x3 x;", "extern float2x3 x;\n");
    check_rssl_to_hlsl("float3x3 x;", "extern float3x3 x;\n");
    check_rssl_to_hlsl("float4x3 x;", "extern float4x3 x;\n");

    check_rssl_to_hlsl("float1x4 x;", "extern float1x4 x;\n");
    check_rssl_to_hlsl("float2x4 x;", "extern float2x4 x;\n");
    check_rssl_to_hlsl("float3x4 x;", "extern float3x4 x;\n");
    check_rssl_to_hlsl("float4x4 x;", "extern float4x4 x;\n");

    check_rssl_to_hlsl("int x;", "extern int x;\n");
    check_rssl_to_hlsl("int1 x;", "extern int1 x;\n");
    check_rssl_to_hlsl("int2 x;", "extern int2 x;\n");
    check_rssl_to_hlsl("int3 x;", "extern int3 x;\n");
    check_rssl_to_hlsl("int4 x;", "extern int4 x;\n");

    check_rssl_to_hlsl("uint x;", "extern uint x;\n");
    check_rssl_to_hlsl("uint1 x;", "extern uint1 x;\n");
    check_rssl_to_hlsl("uint2 x;", "extern uint2 x;\n");
    check_rssl_to_hlsl("uint3 x;", "extern uint3 x;\n");
    check_rssl_to_hlsl("uint4 x;", "extern uint4 x;\n");

    check_rssl_to_hlsl("half x;", "extern half x;\n");
    check_rssl_to_hlsl("half1 x;", "extern half1 x;\n");
    check_rssl_to_hlsl("half2 x;", "extern half2 x;\n");
    check_rssl_to_hlsl("half3 x;", "extern half3 x;\n");
    check_rssl_to_hlsl("half4 x;", "extern half4 x;\n");

    check_rssl_to_hlsl("vector<float, 1> x;", "extern float1 x;\n");
    check_rssl_to_hlsl("vector<float, 2> x;", "extern float2 x;\n");
    check_rssl_to_hlsl("vector<float, 3> x;", "extern float3 x;\n");
    check_rssl_to_hlsl("vector<float, 4> x;", "extern float4 x;\n");

    check_rssl_to_hlsl("matrix<uint, 1, 1> x;", "extern uint1x1 x;\n");
    check_rssl_to_hlsl("matrix<uint, 2, 1> x;", "extern uint2x1 x;\n");
    check_rssl_to_hlsl("matrix<uint, 3, 1> x;", "extern uint3x1 x;\n");
    check_rssl_to_hlsl("matrix<uint, 4, 1> x;", "extern uint4x1 x;\n");

    check_rssl_to_hlsl("matrix<int, 1, 2> x;", "extern int1x2 x;\n");
    check_rssl_to_hlsl("matrix<int, 2, 2> x;", "extern int2x2 x;\n");
    check_rssl_to_hlsl("matrix<int, 3, 2> x;", "extern int3x2 x;\n");
    check_rssl_to_hlsl("matrix<int, 4, 2> x;", "extern int4x2 x;\n");

    check_rssl_to_hlsl("matrix<half, 1, 3> x;", "extern half1x3 x;\n");
    check_rssl_to_hlsl("matrix<half, 2, 3> x;", "extern half2x3 x;\n");
    check_rssl_to_hlsl("matrix<half, 3, 3> x;", "extern half3x3 x;\n");
    check_rssl_to_hlsl("matrix<half, 4, 3> x;", "extern half4x3 x;\n");

    check_rssl_to_hlsl("matrix<double, 1, 4> x;", "extern double1x4 x;\n");
    check_rssl_to_hlsl("matrix<double, 2, 4> x;", "extern double2x4 x;\n");
    check_rssl_to_hlsl("matrix<double, 3, 4> x;", "extern double3x4 x;\n");
    check_rssl_to_hlsl("matrix<double, 4, 4> x;", "extern double4x4 x;\n");
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
    float y;
    float z[2], w[3][4];

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
    float y;
    float z[2];
    float w[3][4];

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
fn check_object_intrinsics() {
    check_rssl_to_hlsl(
        "ByteAddressBuffer g_buffer; void f() { g_buffer.Load(0); }",
        "extern ByteAddressBuffer g_buffer;

void f() {
    g_buffer.Load(0u);
}
",
    );

    check_rssl_to_hlsl(
        "const ByteAddressBuffer g_buffer; void f() { g_buffer.Load(0); }",
        "extern const ByteAddressBuffer g_buffer;

void f() {
    g_buffer.Load(0u);
}
",
    );

    check_rssl_to_hlsl(
        "BufferAddress g_buffer; void f() { g_buffer.Load<uint>(0); }",
        "extern ByteAddressBuffer g_buffer;

void f() {
    g_buffer.Load<uint>(0u);
}
",
    );

    check_rssl_to_hlsl(
        "RWStructuredBuffer<uint> g_buffer; void f() { g_buffer[0]; g_buffer[0] = 1; }",
        "extern RWStructuredBuffer<uint> g_buffer;

void f() {
    g_buffer[0u];
    g_buffer[0u] = 1u;
}
",
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

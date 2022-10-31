mod shared;
use shared::*;

#[test]
fn check_static_primitive_variables() {
    check_rssl_to_hlsl("int x = 0;", "extern int x = 0;\n");
    check_rssl_to_hlsl("static int x = 1;", "static int x = 1;\n");
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
        "ByteAddressBuffer g_buffer; void f() { g_buffer.Load(0); }",
        "extern ByteAddressBuffer g_buffer;

void f() {
    g_buffer.Load(0u);
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
        return;
    }
}",
        "void f() {
    while (true)
    {
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
}

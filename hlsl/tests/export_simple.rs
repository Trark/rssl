mod shared;
use shared::*;

#[test]
fn check_static_primitive_variables() {
    check_rssl_to_hlsl("int x = 0;", "extern int x = (int)(0);\n");
    check_rssl_to_hlsl("static int x = 1;", "static int x = (int)(1);\n");
    check_rssl_to_hlsl("extern int x = -1;", "extern int x = (int)(-(1));\n");
}

#[test]
fn check_functions() {
    check_rssl_to_hlsl("void f() {}", "void f() {}\n");
    check_rssl_to_hlsl("void f(int x) {}", "void f(int x) {}\n");
    check_rssl_to_hlsl(
        "float f(int x, float y) { return x + y; }",
        "float f(int x, float y) {
    return ((float)(x)) + (y);
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
    float4 v0 = float4((float)(1), (float)(2), (float)(3), (float)(4));
    float4 v1 = (true) ? (v0) : ((float4)(0));
    float4x4 m = float4x4(v0, v0, v1, v1);
    float4 v2 = mul(m, v0);
}
",
    );

    check_rssl_to_hlsl(
        "ByteAddressBuffer g_buffer; void f() { g_buffer.Load(0); }",
        "extern ByteAddressBuffer g_buffer;
void f() {
    g_buffer.Load((uint)(0));
}
",
    );

    check_rssl_to_hlsl(
        "RWStructuredBuffer<uint> g_buffer; void f() { g_buffer[0]; g_buffer[0] = 1; }",
        "extern RWStructuredBuffer<uint> g_buffer;
void f() {
    (g_buffer)[(uint)(0)];
    ((g_buffer)[(uint)(0)]) = ((uint)(1));
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
    float x = 0;
    {
        float y = 6;
        float z = (y) + (x);
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
    float x = 0;
    if (x)
    {
        return 1;
    }
    return 2;
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
    float x = 0;
    if (x)
    {
        return 1;
    }
    else
    {
        return 3;
    }
    return 2;
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
    float x = 0;
    if (x)
    {
        return 1;
    }
    else
    {
        if ((x) > (2))
        {
            return 4;
        }
        else
        {
            return 3;
        }
    }
    return 2;
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
    for (int x = (int)(1), y = (int)(2); (x) < ((int)(10)); ++(x))
    {
        return;
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

    void f(int z) {}

    void g() {
        f(x);
    }
};
void main() {
    S s;
    (s.x) = ((int)(5));
    (s).g();
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
};
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
};
void main() {
    v0;
    (v1).wwww;
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
    return (float4)(0);
}
",
    );

    check_rssl_to_hlsl(
        "float4 main(float4 coord : SV_Position) : SV_Target { return 0; }",
        "float4 main(float4 coord : SV_Position) : SV_Target0 {
    return (float4)(0);
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

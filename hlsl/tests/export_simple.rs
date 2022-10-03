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
    check_rssl_to_hlsl("void f() {}", "void f() {}");
    check_rssl_to_hlsl("void f(int x) {}", "void f(int x) {}");
    check_rssl_to_hlsl(
        "float f(int x, float y) { return x + y; }",
        "float f(int x, float y) {
    return ((float)(x)) + (y);
}",
    );
}

#[test]
fn check_expressions() {
    check_rssl_to_hlsl(
        "void f() { float4x4 m; float4 v0; float4 v1 = mul(m, v0); }",
        "void f() {
    float4x4 m;
    float4 v0;
    float4 v1 = mul(m, v0);
}",
    );

    check_rssl_to_hlsl(
        "ByteAddressBuffer g_buffer; void f() { g_buffer.Load(0); }",
        "extern ByteAddressBuffer g_buffer;
void f() {
    g_buffer.Load((uint)(0));
}",
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
}",
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
}",
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
}",
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
}",
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
}",
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
}",
    );
}

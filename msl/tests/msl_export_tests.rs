mod shared;
use shared::*;

#[test]
fn check_static_primitive_variables() {
    // Mutable variables do not generate global definitions
    check("int x = 0;", "");
    check("static int x = 1;", "");
    check("static const int x = 1;", "constant int x = 1;\n");
    check("static const int x = 1.0;", "constant int x = 1;\n");
    check("extern int x = -1;", "");
    check("static float x[4];", "");
    check("static float x[4] = { 0.0, 1.0, 2.0, 3.0 };", "");
    check("static float x[2][3], y[3][4];", "");
    check(
        "static const int data[3][2] = { { 0, 1 }, { 2, 3 }, { 4, 5 } };",
        "constant int data[3][2] = { { 0, 1 }, { 2, 3 }, { 4, 5 } };\n",
    );
}

#[test]
fn check_primitive_types() {
    check("float x() {}", "float x() {}\n");
    check("float1 x() {}", "float x() {}\n");
    check("float2 x() {}", "float2 x() {}\n");
    check("float3 x() {}", "float3 x() {}\n");
    check("float4 x() {}", "float4 x() {}\n");

    expect_generate_fail("float1x1 x() {}", GenerateError::UnsupportedUnitMatrix);
    expect_generate_fail("float2x1 x() {}", GenerateError::UnsupportedUnitMatrix);
    expect_generate_fail("float3x1 x() {}", GenerateError::UnsupportedUnitMatrix);
    expect_generate_fail("float4x1 x() {}", GenerateError::UnsupportedUnitMatrix);

    expect_generate_fail("float1x2 x() {}", GenerateError::UnsupportedUnitMatrix);
    check("float2x2 x() {}", "metal::float2x2 x() {}\n");
    check("float3x2 x() {}", "metal::float2x3 x() {}\n");
    check("float4x2 x() {}", "metal::float2x4 x() {}\n");

    expect_generate_fail("float1x3 x() {}", GenerateError::UnsupportedUnitMatrix);
    check("float2x3 x() {}", "metal::float3x2 x() {}\n");
    check("float3x3 x() {}", "metal::float3x3 x() {}\n");
    check("float4x3 x() {}", "metal::float3x4 x() {}\n");

    expect_generate_fail("float1x4 x() {}", GenerateError::UnsupportedUnitMatrix);
    check("float2x4 x() {}", "metal::float4x2 x() {}\n");
    check("float3x4 x() {}", "metal::float4x3 x() {}\n");
    check("float4x4 x() {}", "metal::float4x4 x() {}\n");

    check("int x() {}", "int x() {}\n");
    check("int1 x() {}", "int x() {}\n");
    check("int2 x() {}", "int2 x() {}\n");
    check("int3 x() {}", "int3 x() {}\n");
    check("int4 x() {}", "int4 x() {}\n");

    check("uint x() {}", "uint x() {}\n");
    check("uint1 x() {}", "uint x() {}\n");
    check("uint2 x() {}", "uint2 x() {}\n");
    check("uint3 x() {}", "uint3 x() {}\n");
    check("uint4 x() {}", "uint4 x() {}\n");

    check("half x() {}", "half x() {}\n");
    check("half1 x() {}", "half x() {}\n");
    check("half2 x() {}", "half2 x() {}\n");
    check("half3 x() {}", "half3 x() {}\n");
    check("half4 x() {}", "half4 x() {}\n");

    check("vector<float, 1> x() {}", "float x() {}\n");
    check("vector<float, 2> x() {}", "float2 x() {}\n");
    check("vector<float, 3> x() {}", "float3 x() {}\n");
    check("vector<float, 4> x() {}", "float4 x() {}\n");

    expect_generate_fail(
        "matrix<uint, 1, 1> x() {}",
        GenerateError::UnsupportedNonFloatMatrix,
    );
    expect_generate_fail(
        "matrix<uint, 2, 1> x() {}",
        GenerateError::UnsupportedNonFloatMatrix,
    );
    expect_generate_fail(
        "matrix<uint, 3, 1> x() {}",
        GenerateError::UnsupportedNonFloatMatrix,
    );
    expect_generate_fail(
        "matrix<uint, 4, 1> x() {}",
        GenerateError::UnsupportedNonFloatMatrix,
    );

    expect_generate_fail(
        "matrix<int, 1, 2> x() {}",
        GenerateError::UnsupportedNonFloatMatrix,
    );
    expect_generate_fail(
        "matrix<int, 2, 2> x() {}",
        GenerateError::UnsupportedNonFloatMatrix,
    );
    expect_generate_fail(
        "matrix<int, 3, 2> x() {}",
        GenerateError::UnsupportedNonFloatMatrix,
    );
    expect_generate_fail(
        "matrix<int, 4, 2> x() {}",
        GenerateError::UnsupportedNonFloatMatrix,
    );

    expect_generate_fail(
        "matrix<half, 1, 3> x() {}",
        GenerateError::UnsupportedUnitMatrix,
    );
    check("matrix<half, 2, 3> x() {}", "metal::half3x2 x() {}\n");
    check("matrix<half, 3, 3> x() {}", "metal::half3x3 x() {}\n");
    check("matrix<half, 4, 3> x() {}", "metal::half3x4 x() {}\n");

    expect_generate_fail(
        "matrix<double, 1, 4> x() {}",
        GenerateError::UnsupportedNonFloatMatrix,
    );
    expect_generate_fail(
        "matrix<double, 2, 4> x() {}",
        GenerateError::UnsupportedNonFloatMatrix,
    );
    expect_generate_fail(
        "matrix<double, 3, 4> x() {}",
        GenerateError::UnsupportedNonFloatMatrix,
    );
    expect_generate_fail(
        "matrix<double, 4, 4> x() {}",
        GenerateError::UnsupportedNonFloatMatrix,
    );
}

#[test]
fn check_float_literal() {
    check("static const float x = 0;", "constant float x = 0.0f;\n");
    check("static const float x = 1;", "constant float x = 1.0f;\n");
    check(
        "static const float x = -1.0f;",
        "constant float x = -1.0f;\n",
    );
    check("static const float x = 0.5;", "constant float x = 0.5f;\n");
    check(
        "static const float x = 3.402823466e+38f;",
        "constant float x = 340282350000000000000000000000000000000.0f;\n",
    );
    check(
        "static const float x = -3.402823466e+38f;",
        "constant float x = -340282350000000000000000000000000000000.0f;\n",
    );
    check(
        "static const float x = 1.175494351e-38f;",
        "constant float x = 0.000000000000000000000000000000000000011754944f;\n",
    );
    check(
        "static const float x = -1.175494351e-38f;",
        "constant float x = -0.000000000000000000000000000000000000011754944f;\n",
    );
    check(
        "static const float x = 1.#INF;",
        "constant float x = INFINITY;\n",
    );
    check(
        "static const float x = -1.#INF;",
        "constant float x = -INFINITY;\n",
    );
    check(
        "static const half x = 1.#INF;",
        "constant half x = INFINITY;\n",
    );
    check(
        "static const half x = -1.#INF;",
        "constant half x = -INFINITY;\n",
    );
    check(
        "static const float x = -1.#INF - 1.#INF;",
        "constant float x = (float)(-INFINITY - INFINITY);\n",
    );
}

#[test]
fn check_functions() {
    check("void f() {}", "void f() {}\n");
    check("void f(int x) {}", "void f(int x) {}\n");
    check(
        "float f(int x, float y) { return (float)x + y; }",
        "float f(int x, float y) {
    return (float)x + y;
}
",
    );

    // Declared and defined functions - in a namespace to add some scoping
    check(
        "namespace M {

void f();

void g();

void f() {}

void g() {}

} // namespace M
",
        "namespace M {

void f();

void g();

void f() {}

void g() {}

} // namespace M
",
    );

    // main is a reserved function name in metal
    check("void main() {}", "void main_0() {}\n");
}

#[test]
fn check_function_attributes() {
    // Thread count is customisable at runtime
    // TODO: Ensure we have reflection metadata to get the requested thread group size
    check("[numthreads(64, 1, 1)] void Main() {}", "void Main() {}\n");

    expect_generate_fail(
        "[WaveSize(64)] void Main() {}",
        GenerateError::UnsupportedWaveSize,
    );

    expect_generate_fail(
        "[outputtopology(\"triangle\")] void Main() {}",
        GenerateError::UnimplementedMeshShader,
    );
}

#[test]
fn check_function_param_interp_modifiers() {
    expect_generate_fail(
        "void VSMAIN(out float4 x : TEXCOORD) {}",
        GenerateError::UnimplementedOutParameters,
    );

    expect_generate_fail(
        "void VSMAIN(out linear float4 x : TEXCOORD) {}",
        GenerateError::UnimplementedOutParameters,
    );

    expect_generate_fail(
        "void VSMAIN(out centroid float4 x : TEXCOORD) {}",
        GenerateError::UnimplementedOutParameters,
    );

    expect_generate_fail(
        "void VSMAIN(out nointerpolation float4 x : TEXCOORD) {}",
        GenerateError::UnimplementedOutParameters,
    );

    expect_generate_fail(
        "void VSMAIN(out noperspective float4 x : TEXCOORD) {}",
        GenerateError::UnimplementedOutParameters,
    );

    expect_generate_fail(
        "void VSMAIN(out sample float4 x : TEXCOORD) {}",
        GenerateError::UnimplementedOutParameters,
    );
}

#[test]
fn check_function_param_system_semantics() {
    expect_generate_fail(
        "void VSMAIN(out float4 pos : SV_Position) {}",
        GenerateError::UnimplementedOutParameters,
    );

    expect_generate_fail(
        "void VSMAIN(out precise float4 pos : SV_Position) {}",
        GenerateError::UnimplementedOutParameters,
    );
}

#[test]
fn check_function_param_default_value() {
    check("void f(float x = 1.5f) {}", "void f(float x = 1.5f) {}\n");
}

#[test]
fn check_function_templates() {
    check(
        "template<typename T, T A>
T f(T t) { return t + A; }

void entry() {
    uint x = f<uint, 1u>(0u);
    int y = f<int, -2>((int)0);
}
",
        "template<typename, uint>
uint f_0(uint t) {
    return t + 1u;
}

template<typename, int>
int f_1(int t) {
    return t + -2;
}

void entry() {
    uint x = f_0<uint, 1u>(0u);
    int y = f_1<int, -2>((int)0);
}
",
    );
}

#[test]
fn check_local_variable_modifiers() {
    check(
        "void f() { float x; const float y = 0.0f; }",
        "void f() {
    float x;
    const float y = 0.0f;
}
",
    );

    expect_generate_fail(
        "void f() { const float y; }",
        GenerateError::UninitializedConstant,
    );
}

#[test]
fn check_expressions() {
    check(
        "void f() {
    float4 v0 = float4(1, 2, 3, 4);
    float4 v1 = true ? v0 : 0;
    float4x3 m = float4x3(v0, v0, v1);
    float4 v2 = mul(m, v0.xyz);
}
",
        "void f() {
    float4 v0 = float4(1.0f, 2.0f, 3.0f, 4.0f);
    float4 v1 = true ? v0 : (float4)0;
    metal::float3x4 m = metal::float3x4(v0, v0, v1);
    float4 v2 = m * v0.xyz;
}
",
    );

    check(
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
    check(
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
    check(
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
    check(
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
    check(
        "void f() {
    float4 v0 = float4(1, 2, 3, 4);
    float4 v1 = v0.xyzw;
    float4 v2 = v0.wwww;
}
",
        "void f() {
    float4 v0 = float4(1.0f, 2.0f, 3.0f, 4.0f);
    float4 v1 = v0.xyzw;
    float4 v2 = v0.wwww;
}
",
    );
}

#[test]
fn check_expression_matrix_swizzle() {
    expect_generate_fail(
        "void f() {
    float4x4 m;
    float v1 = m._m00;
    float v2 = m._11;
    float4 v3 = m._m00_m00_m00_m00;
}
",
        GenerateError::UnimplementedMatrixSwizzle,
    );
}

#[test]
fn check_expression_vector_index() {
    check(
        "void f() {
    float4 v0 = float4(1, 2, 3, 4);
    float v1 = v0[0];
    float v2 = v0[3];
}
",
        "void f() {
    float4 v0 = float4(1.0f, 2.0f, 3.0f, 4.0f);
    float v1 = v0[0u];
    float v2 = v0[3u];
}
",
    );
}

#[test]
fn check_expression_matrix_index() {
    expect_generate_fail(
        "void f() {
    float2x4 m0;
    float4 v1 = m0[0];
    float v2 = m0[1][2];
}
",
        GenerateError::UnimplementedMatrixIndex,
    );
}

#[test]
fn check_expression_literal_operators() {
    check(
        "void f() {
    true * false / true;
    1 * 1 / 1;
    1 * 1 / 1.0;
    1 * 1.0 / 1;
    1 * 1.0 / 1.0;
    1.0 * 1 / 1;
    1.0 * 1 / 1.0;
    1.0 * 1.0 / 1;
    1.0 * 1.0 / 1.0;
    true + 0;
    true + 0.0;
    0 + true;
    0.0 + true;
}
",
        "void f() {
    (int)true * (int)false / (int)true;
    1 * 1 / 1;
    1 * 1 / 1.0;
    1 * 1.0 / 1;
    1 * 1.0 / 1.0;
    1.0 * 1 / 1;
    1.0 * 1 / 1.0;
    1.0 * 1.0 / 1;
    1.0 * 1.0 / 1.0;
    true + 0;
    true + 0.0;
    0 + true;
    0.0 + true;
}
",
    );
}

#[test]
fn check_statement_block() {
    check(
        "void f() {
    float x = 0.0;
    {
        float y = 6.0;
        float z = y + x;
    }
}",
        "void f() {
    float x = 0.0f;
    {
        float y = 6.0f;
        float z = y + x;
    }
}
",
    );
}

#[test]
fn check_statement_if() {
    check(
        "float f() {
    float x = 0.0;
    if (x)
    {
        return 1.0;
    }
    return 2.0;
}",
        "float f() {
    float x = 0.0f;
    if (x)
    {
        return 1.0f;
    }
    return 2.0f;
}
",
    );
}

#[test]
fn check_statement_if_else() {
    check(
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
    float x = 0.0f;
    if (x)
    {
        return 1.0f;
    }
    else
    {
        return 3.0f;
    }
    return 2.0f;
}
",
    );
}

#[test]
fn check_statement_if_else_if_else() {
    check(
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
    float x = 0.0f;
    if (x)
    {
        return 1.0f;
    }
    else
    {
        if (x > 2.0f)
        {
            return 4.0f;
        }
        else
        {
            return 3.0f;
        }
    }
    return 2.0f;
}
",
    );
}

#[test]
fn check_statement_if_branch() {
    check(
        "void f() {
    [branch]
    if (true)
    {
    }
}",
        "void f() {
    if (true)
    {
    }
}
",
    );
}

#[test]
fn check_statement_if_flatten() {
    check(
        "void f() {
    [flatten]
    if (true)
    {
    }
}",
        "void f() {
    if (true)
    {
    }
}
",
    );
}

#[test]
fn check_statement_for() {
    check(
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

    check(
        "void f() {
    for (int x[2] = { 1, 5 }, y[2][3] = { { 2, 3, 4 }, { 5, 6, 7 } }; true; true)
    {
        break;
    }
}",
        "void f() {
    for (int x[2] = { 1, 5 }, y[2][3] = { { 2, 3, 4 }, { 5, 6, 7 } }; true; true)
    {
        break;
    }
}
",
    );

    check(
        "void f() {
    for (;;);
}",
        "void f() {
    for (;;)
    {
    }
}
",
    );
}

#[test]
fn check_statement_for_unroll() {
    check(
        "void f() {
    [unroll]
    for (int x = 0; x < 10; ++x)
    {
    }
}",
        "void f() {
    for (int x = 0; x < 10; ++x)
    {
    }
}
",
    );

    check(
        "void f() {
    [unroll(4)]
    for (int x = 0; x < 10; ++x)
    {
    }
}",
        "void f() {
    for (int x = 0; x < 10; ++x)
    {
    }
}
",
    );
}

#[test]
fn check_statement_for_loop() {
    check(
        "void f() {
    [loop]
    for (int x = 0; x < 10; ++x)
    {
    }
}",
        "void f() {
    for (int x = 0; x < 10; ++x)
    {
    }
}
",
    );
}

#[test]
fn check_statement_for_fastopt() {
    check(
        "void f() {
    [fastopt]
    for (int x = 0; x < 10; ++x)
    {
    }
}",
        "void f() {
    for (int x = 0; x < 10; ++x)
    {
    }
}
",
    );
}

#[test]
fn check_statement_for_allow_uav_condition() {
    check(
        "void f() {
    [allow_uav_condition]
    for (int x = 0; x < 10; ++x)
    {
    }
}",
        "void f() {
    for (int x = 0; x < 10; ++x)
    {
    }
}
",
    );
}

#[test]
fn check_statement_while() {
    check(
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
        metal::discard_fragment();
        return;
    }
}
",
    );
}

#[test]
fn check_statement_while_unroll() {
    check(
        "void f() {
    [unroll]
    while (true)
    {
    }
}",
        "void f() {
    while (true)
    {
    }
}
",
    );

    check(
        "void f() {
    [unroll(4)]
    while (true)
    {
    }
}",
        "void f() {
    while (true)
    {
    }
}
",
    );
}

#[test]
fn check_statement_while_loop() {
    check(
        "void f() {
    [loop]
    while (true)
    {
    }
}",
        "void f() {
    while (true)
    {
    }
}
",
    );
}

#[test]
fn check_statement_while_fastopt() {
    check(
        "void f() {
    [fastopt]
    while (true)
    {
    }
}",
        "void f() {
    while (true)
    {
    }
}
",
    );
}

#[test]
fn check_statement_while_allow_uav_condition() {
    check(
        "void f() {
    [allow_uav_condition]
    while (true)
    {
    }
}",
        "void f() {
    while (true)
    {
    }
}
",
    );
}

#[test]
fn check_statement_do_while() {
    check(
        "void f() {
    do
    {
        continue;
        break;
        discard;
        return;
    }
    while (true);
}",
        "void f() {
    do
    {
        continue;
        break;
        metal::discard_fragment();
        return;
    }
    while (true);
}
",
    );
}

#[test]
fn check_statement_switch() {
    // Formatting for non-scoped cases is non standard - but should function
    check(
        "void f() {
    switch (7)
    {
        case 0: break;
        case 7: return;
        default: { break; }
    }
}",
        "void f() {
    switch (7)
    {
        case 0:
        break;
        case 7:
        return;
        default:
        {
            break;
        }
    }
}
",
    );
}

#[test]
fn check_struct() {
    check(
        "struct S
{
    int x;
    float y : USER0;
    float z[2], w[3][4] : USER1;

    void f(int z) {}
    void g() { f(x); }
};

void entry() {
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

void entry() {
    S s;
    s.x = 5;
    s.x = 4;
    s.g();
}
",
    );
}

#[test]
fn check_struct_member_interp_modifiers() {
    check(
        "struct S
{
    int m0 : USER0;
    linear float m1 : USER1;
    centroid float m2;
    nointerpolation float m3, m4 : USER4;
    noperspective float m5;
    sample float m6;
};
",
        "struct S
{
    int m0;
    [[center_perspective]] float m1;
    [[centroid_perspective]] float m2;
    [[flat]] float m3;
    [[flat]] float m4;
    [[center_no_perspective]] float m5;
    [[sample_perspective]] float m6;
};
",
    );

    // Arrays can not have interpolation modes
    expect_generate_fail(
        "struct S { nointerpolation float m3[2]; };",
        GenerateError::UnsupportedArrayInterpolator,
    );
}

#[test]
fn check_struct_member_const() {
    // const is not valid on a struct member so it can't end up in the final output
    check(
        "typedef const float X; struct S
{
    X x;
};
",
        "struct S
{
    float x;
};
",
    );
}

#[test]
fn check_struct_member_volatile() {
    // volatile is not valid on a struct member so it can't end up in the final output
    check(
        "typedef volatile float X; struct S
{
    X x;
};
",
        "struct S
{
    float x;
};
",
    );
}

#[test]
fn check_struct_member_precise() {
    expect_generate_fail(
        "struct S
{
    precise float x;
    precise float2 y, z;
};
",
        GenerateError::UnsupportedPrecise,
    );
}

#[test]
fn check_enum() {
    check(
        "enum E
{
    A = false,
    B = 1,
    C = 2u,
};

void entry() {
    E v = A;
    E enumMask = B | C;
    int underlyingMask = B | C;
}
",
        "enum E
{
    A = 0,
    B = 1,
    C = 2,
};

void entry() {
    E v = E::A;
    E enumMask = (E)(E::B | E::C);
    int underlyingMask = (int)(E)(E::B | E::C);
}
",
    );
}

#[test]
fn check_enum_switch() {
    check(
        "enum E
{
    A = 0,
    B = 1,
};

void entry() {
    E v = A;
    switch (v)
    {
        case A:
        {
        }
        case B:
        {
        }
        case (E)2:
        {
        }
    }
}
",
        "enum E
{
    A = 0,
    B = 1,
};

void entry() {
    E v = E::A;
    switch (v)
    {
        case E::A:
        {
        }
        case E::B:
        {
        }
        case (E)2:
        {
        }
    }
}
",
    );
}

#[test]
fn check_typedef() {
    // Check we can declare and use a typedef - the type will be substituted at site of use.
    check(
        "typedef int CustomInt; static const CustomInt x = 0;",
        "constant int x = 0;\n",
    );

    // Check this works correctly with array types
    check(
        "typedef const int CustomInt[4]; static CustomInt x = { 0, 0, 0, 0 };",
        "constant int x[4] = { 0, 0, 0, 0 };\n",
    );
}

#[test]
fn check_global_function_parameters() {
    let rssl = "static const uint c_immutable = 4;
static uint s_globalState = 1;
groupshared float lds_x;
StructuredBuffer<uint4> g_buffer;

void sub_constant() {
    c_immutable;
}

void sub_static() {
    s_globalState += 1u;
}

void sub_buffer() {
    g_buffer[0];
}

void sub_groupshared() {
    lds_x;
}

void sub_all() {
    sub_static();
    sub_buffer();
    sub_groupshared();
}

void entry() {
    sub_all();
}
";

    let msl = "constant uint c_immutable = 4u;

void sub_constant() {
    c_immutable;
}

void sub_static(thread uint& s_globalState) {
    s_globalState += 1u;
}

void sub_buffer(device const uint4* const g_buffer) {
    g_buffer[0u];
}

void sub_groupshared(threadgroup float& lds_x) {
    lds_x;
}

void sub_all(thread uint& s_globalState, threadgroup float& lds_x, device const uint4* const g_buffer) {
    sub_static(s_globalState);
    sub_buffer(g_buffer);
    sub_groupshared(lds_x);
}

void entry(thread uint& s_globalState, threadgroup float& lds_x, device const uint4* const g_buffer) {
    sub_all(s_globalState, lds_x, g_buffer);
}
";

    check(rssl, msl);
}

#[test]
fn check_intrinsic_functions() {
    check(
        "void entry() {
    float x = -1.0f;
    float y = abs(x);
}
",
        "void entry() {
    float x = -1.0f;
    float y = metal::abs(x);
}
",
    );
}

#[test]
fn check_constant_buffer() {
    expect_generate_fail(
        "cbuffer GlobalConstants
{
    float4 v0;
    uint4 v1;
    float4x4 m2;
    float a3[2], a4[3][4];
}

void entry() {
    v0;
    v1.wwww;
    m2;
}
",
        GenerateError::UnimplementedConstantBuffer,
    );

    check(
        "struct MyStruct
{
    uint m;
};

const ConstantBuffer<MyStruct> g_input : register(b0);

void test() {
    const MyStruct s1 = (MyStruct)g_input;
    const uint m1 = g_input.m;
}
",
        "struct MyStruct
{
    uint m;
};

void test(constant MyStruct& g_input) {
    const MyStruct s1 = (MyStruct)g_input;
    const uint m1 = g_input.m;
}
",
    );
}

#[test]
fn check_object_array() {
    // Not an array base case
    check(
        "Texture2D<float4> g_texture;

void entry() {
    g_texture;
}
",
        "void entry(const metal::texture2d<float> g_texture) {
    g_texture;
}
",
    );

    // Specific size
    check(
        "Texture2D<float4> g_textures[6];

void entry() {
    g_textures[0];
}
",
        "void entry(metal::array<const metal::texture2d<float>, 6> g_textures) {
    g_textures[0u];
}
",
    );

    // Unbounded
    check(
        "Texture2D<float4> g_textures[];

void entry() {
    g_textures[0];
}
",
        "void entry(metal::array_ref<const metal::texture2d<float>> g_textures) {
    g_textures[0u];
}
",
    );
}

#[test]
fn check_structured_buffer() {
    check(
        include_str!("structured_buffer.rssl"),
        include_str!("structured_buffer.metal"),
    );
}

#[test]
fn check_texture2d() {
    check(
        include_str!("texture2d.rssl"),
        include_str!("texture2d.metal"),
    );
}

#[test]
fn check_texture2darray() {
    check(
        include_str!("texture2darray.rssl"),
        include_str!("texture2darray.metal"),
    );
}

#[test]
fn check_texturecube() {
    check(
        include_str!("texturecube.rssl"),
        include_str!("texturecube.metal"),
    );
}

#[test]
fn check_texture3d() {
    check(
        include_str!("texture3d.rssl"),
        include_str!("texture3d.metal"),
    );
}

#[test]
fn check_semantics() {
    expect_generate_fail(
        "float4 entry(uint id : SV_VertexID) : SV_Position { return 0; }",
        GenerateError::UnimplementedFunctionReturnWithSemantic,
    );

    expect_generate_fail(
        "float4 entry(float4 coord : SV_Position) : SV_Target { return 0; }",
        GenerateError::UnimplementedFunctionReturnWithSemantic,
    );
}

#[test]
fn check_compute_pipeline() {
    check(
        include_str!("compute_pipeline.rssl"),
        include_str!("compute_pipeline.metal"),
    );
}

#[test]
fn check_vertex_pixel_pipeline() {
    check(
        include_str!("vertex_pixel_pipeline.rssl"),
        include_str!("vertex_pixel_pipeline.metal"),
    );
}

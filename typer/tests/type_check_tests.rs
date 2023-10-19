mod shared;
use shared::*;

#[test]
fn check_global_primitive_variables() {
    check_types("static uint x = 4;");
    check_types("groupshared float lds_data[7];");
}

#[test]
fn check_primitive_types() {
    check_types("float x;");
    check_types("float1 x;");
    check_types("float2 x;");
    check_types("float3 x;");
    check_types("float4 x;");

    check_types("float1x1 x;");
    check_types("float2x1 x;");
    check_types("float3x1 x;");
    check_types("float4x1 x;");

    check_types("float1x2 x;");
    check_types("float2x2 x;");
    check_types("float3x2 x;");
    check_types("float4x2 x;");

    check_types("float1x3 x;");
    check_types("float2x3 x;");
    check_types("float3x3 x;");
    check_types("float4x3 x;");

    check_types("float1x4 x;");
    check_types("float2x4 x;");
    check_types("float3x4 x;");
    check_types("float4x4 x;");

    check_types("int x;");
    check_types("int1 x;");
    check_types("int2 x;");
    check_types("int3 x;");
    check_types("int4 x;");

    check_types("uint x;");
    check_types("uint1 x;");
    check_types("uint2 x;");
    check_types("uint3 x;");
    check_types("uint4 x;");

    check_types("half x;");
    check_types("half1 x;");
    check_types("half2 x;");
    check_types("half3 x;");
    check_types("half4 x;");

    check_types("vector<float, 1> x;");
    check_types("vector<float, 2> x;");
    check_types("vector<float, 3> x;");
    check_types("vector<float, 4> x;");

    check_types("matrix<uint, 1, 1> x;");
    check_types("matrix<uint, 2, 1> x;");
    check_types("matrix<uint, 3, 1> x;");
    check_types("matrix<uint, 4, 1> x;");

    check_types("matrix<int, 1, 2> x;");
    check_types("matrix<int, 2, 2> x;");
    check_types("matrix<int, 3, 2> x;");
    check_types("matrix<int, 4, 2> x;");

    check_types("matrix<half, 1, 3> x;");
    check_types("matrix<half, 2, 3> x;");
    check_types("matrix<half, 3, 3> x;");
    check_types("matrix<half, 4, 3> x;");

    check_types("matrix<double, 1, 4> x;");
    check_types("matrix<double, 2, 4> x;");
    check_types("matrix<double, 3, 4> x;");
    check_types("matrix<double, 4, 4> x;");

    check_fail("vector<float, 0> x;");
    check_fail("vector<float, 5> x;");
    check_fail("matrix<float, 0, 4> x;");
    check_fail("matrix<float, 4, 0> x;");
    check_fail("matrix<float, 0, 0> x;");
    check_fail("matrix<float, 5, 4> x;");
    check_fail("matrix<float, 4, 5> x;");
    check_fail("matrix<float, 5, 5> x;");

    // We do not support const types as type args here
    // This also does not work as expected with HLSL / DXC
    check_fail("vector<const float, 4> x;");
    check_fail("matrix<const float, 4, 4> x;");
}

#[test]
fn check_global_type_modifiers() {
    // trivial case
    check_types("float x;");

    // all storage types are allowed
    check_types("static float x;");
    check_types("extern float x;");
    check_types("groupshared float x;");
    check_types("static static float x;");
    check_types("extern extern float x;");
    check_types("groupshared groupshared float x;");
    check_fail("static extern float x;");
    check_fail("extern groupshared float x;");
    check_fail("groupshared static float x;");

    // const is allowed
    check_types("const float x;");
    check_types("static const float x;");
    check_types("extern const float x;");
    check_types("groupshared const float x;");

    // volatile is not allowed
    check_fail("volatile float x;");

    // Matrix orderings are allowed (on matrix types)
    check_types("row_major float4x4 x;");
    check_types("column_major float4x4 x;");
    check_fail("row_major column_major float4x4 x;");
    check_fail("row_major float4 x;");
    check_fail("column_major float4 x;");

    // parameter output types are not allowed
    check_fail("in float x;");
    check_fail("out float x;");
    check_fail("inout float x;");

    // precise is not allowed
    // This is allowed in HLSL but does not do anything
    check_fail("precise float x;");

    // Interpolation modifiers are not allowed
    check_fail("nointerpolation float x;");
    check_fail("linear float x;");
    check_fail("centroid float x;");
    check_fail("noperspective float x;");
    check_fail("sample float x;");

    // Geometry shader primitive types are not allowed
    check_fail("point float x;");
    check_fail("line float x;");
    check_fail("triangle float x;");
    check_fail("lineadj float x;");
    check_fail("triangleadj float x;");

    // Mesh shader output modifiers are not allowed
    check_fail("vertices float x;");
    check_fail("primitives float x;");
    check_fail("indices uint3 x;");
    check_fail("payload float x;");
}

#[test]
fn check_global_variable_with_contextual_keyword_names() {
    check_types("uint precise;");
    check_fail("uint nointerpolation;");
    check_fail("uint linear;");
    check_fail("uint centroid;");
    check_fail("uint noperspective;");
    check_types("uint sample;");
    check_fail("uint point;");
    check_fail("uint line;");
    check_fail("uint triangle;");
    check_fail("uint lineadj;");
    check_fail("uint triangleadj;");
    check_types("uint vertices;");
    check_types("uint primitives;");
    check_types("uint indices;");
    check_types("uint payload;");
}

#[test]
fn check_void() {
    check_fail("void x;");
    check_fail("const void x;");
    check_fail("void f() { void x; }");
    check_fail("void f() { const void x; }");
    check_fail("void f(void x) {}");
    check_fail("void f(const void x) {}");
    check_fail("cbuffer VoidTest { void x; }");
    check_fail("cbuffer VoidTest { const void x; }");
    check_fail("struct VoidTest { void x; };");
    check_fail("struct VoidTest { const void x; };");
}

#[test]
fn check_array_dimension_arguments() {
    // Literal size argument
    check_types("static float4 g_myArray[2];");

    // #define argument
    check_types("#define SIZE 2\nstatic float4 g_myArray[SIZE];");

    // static const argument
    check_types("static const uint c_size = 2; static float4 g_myArray[c_size];");

    // static non-const argument fails
    check_fail("static uint c_size = 2; static float4 g_myArray[c_size];");

    // zero sized array fails
    check_fail("static const uint c_size = 0; static float4 g_myArray[c_size];");

    // inferred zero sized array fails
    check_fail("static float4 g_myArray[] = {};");
}

#[test]
fn reject_invalid_initialisers() {
    check_fail("static uint3 x = float2(1, 2);");
    check_fail("static float4 c[2] = float4(0, 1, 2, 3);");
}

#[test]
fn check_aggregate_initializers_primitives() {
    // Initialise an array with all the elements
    check_types("static float g_myArray[2] = { 1.0, 2.0 };");

    // Fail if there are too many elements
    check_fail("static float g_myArray[2] = { 1.0, 2.0, 3.0 };");

    // Fail if there are too few elements - we do not support initializing the unspecified members
    check_fail("static float g_myArray[2] = { 1.0 };");

    // Check that we can infer the array arguments from the initializer if required
    // TODO: Trying this with multidimensional arrays is untested
    check_types("static float g_myArray[] = { 1.0, 2.0 };");

    // Check aggregate initializing float4 inside an array
    // As above, the float4 must list all elements
    // A single scalar outside an inner {} block is also valid as this casts up
    check_types(
        "static float4 g_myArray[3] = { { 1.0, 2.0, 3.0, 4.0 }, 3.0, { 1.0, 2.0, 3.0, 4.0 } };",
    );

    // Initialise a vector
    check_types("static float2 g_vec = { 1.0, 2.0 };");

    // Fail if there are too many elements
    check_fail("static float2 g_vec = { 1.0, 2.0, 3.0 };");

    // Fail if there are too few elements - we do not support initializing the unspecified members
    check_fail("static float2 g_vec = { 1.0 };");
}

#[test]
fn check_aggregate_initializers_structs() {
    // Aggregate construct a struct with no members
    check_types("struct S {}; static S g_test = {};");

    // Wrong number of parameters fails
    check_fail("struct S {}; static S g_test = { false };");

    // Aggregate construct a struct with a single members
    check_types("struct S { uint x; }; static S g_test = { 6u };");

    // Wrong number of parameters fails
    check_fail("struct S { uint x; }; static S g_test = {};");

    // Aggregate construct a struct with multiple members
    check_types(
        "struct S { uint x; float3 y; uint z; }; static S g_test = { 6u, { 8.0, 4.0, 2.0 }, 4u };",
    );

    // Wrong number of parameters fails
    check_fail("struct S { uint x; float y; uint z; }; static S g_test = { 6u };");

    // Wrong typed member (which can not cast) fails
    check_fail(
        "struct S { uint x; float3 y; uint z; }; static S g_test = { 6u, float2(1, 2), 4u };",
    );
}

#[test]
fn check_statements() {
    check_types("void f() { 7; }");
    check_types("int f() { int x = 5; int y = 3; int z = x + y; return z; }");
    check_types("int f() { { int x; } { int x; } }");
}

#[test]
fn check_return() {
    check_types("void f() { return; }");
    check_types("float f() { return 0.0; }");
    check_fail("float f() { return; }");

    // Currently no checking against missing return
}

#[test]
fn check_function_calls() {
    // Check calling a simple function works
    check_types("void subroutine() {} void main() { subroutine(); }");

    // Check calling an unknown function fails
    check_fail("void main() { subroutine(); }");

    // Check we can call multiple functions and they can have parameters
    check_types("void f1() {} void f2(float x) {} void main() { f1(); f2(0); }");

    // Check overloads pick the right overload
    check_types("void f(int4 x) {} void f(int3 x) {} void main() { f(int4(0, 0, 0, 0)); }");
    check_types("void f(int4 x) {} void f(int3 x) {} void main() { f(int3(0, 0, 0)); }");

    // Check that no overloads are valid
    check_fail("void f(int4 x) {} void f(int3 x) {} void main() { f(int2(0, 0)); }");

    // And ensure that gives a certain pretty error
    check_fail_message(
        "void f(int4 x) {} void f(int3 x) {} void main() { f(int2(0, 0)); }",
        "type_test.rssl:1:51: error: no matching function for call to f(int2)
void f(int4 x) {} void f(int3 x) {} void main() { f(int2(0, 0)); }
                                                  ^
type_test.rssl:1:6: note: candidate function not viable: void f(in int4)
void f(int4 x) {} void f(int3 x) {} void main() { f(int2(0, 0)); }
     ^
type_test.rssl:1:24: note: candidate function not viable: void f(in int3)
void f(int4 x) {} void f(int3 x) {} void main() { f(int2(0, 0)); }
                       ^
",
    );

    // Check that we fail if multiple overloads are valid
    check_fail("void f(int2 x) {} void f(int3 x) {} void main() { f(int4(0, 0, 0, 0)); }");

    // Check a function can call itself
    check_types("void f() { f(); }");
    // And that overload resolution works with itself
    check_types("void f(int3 x) {} void f(int2 x) { f(int2(3, 4)); }");
    // And that ambiguous overloads fail with itself
    check_fail("void f(int3 x) {} void f(int2 x) { f(1); }");

    // Check that a function can have a local variable that is an array
    check_types("void f() { int x[2]; } void main() { f(); }");
    // Check that a function can have a function parameter that is an array
    check_types("void f(out int x[2]) {} void main() { int a[2]; f(a); }");

    // Check that we can not define a function that is the same as an intrinsic
    // HLSL allows this but we do not
    check_fail("void mul(float4x4 x, float4 y) {}");

    // Check that we can define functions that are overloads of intrinsic functions
    check_types("struct S {}; void mul(S s) {} void main() { S s; mul(s); }");

    // Ensure the failure message for overloads with both user functions and intrinsics functions
    // Tests the printed names for functions, intrinsics, and structs displays the correct name
    check_fail_message(
        "struct S {}; struct A {}; void mul(S s) {} void main() { A a; mul(a); }",
        "type_test.rssl:1:63: error: no matching function for call to mul(A)
struct S {}; struct A {}; void mul(S s) {} void main() { A a; mul(a); }
                                                              ^
note: candidate function not viable: float3 mul(in float3x3, in float3)
note: candidate function not viable: float4 mul(in float4x4, in float4)
note: candidate function not viable: float3 mul(in float3, in float3x3)
note: candidate function not viable: float4 mul(in float4, in float4x4)
type_test.rssl:1:32: note: candidate function not viable: void mul(in S)
struct S {}; struct A {}; void mul(S s) {} void main() { A a; mul(a); }
                               ^
",
    );
}

#[test]
fn check_function_declare() {
    // Trivial declare with no implementation
    check_types("void f(float x);");

    // Declare a function then defined it
    check_types("void f(float x); void f(float x) {}");

    // Define a function then declare it again
    check_types("void f(float x) {} void f(float x);");

    // Declare multiple times before and after the definition
    check_types(
        "void f(float x); void f(float x); void f(float x) {} void f(float x); void f(float x);",
    );

    // Check that we can not declare a function that is the same as an intrinsic
    // HLSL allows this but we do not
    check_fail("void mul(float4x4 x, float4 y);");
}

#[test]
fn check_function_overload_conflicts() {
    // Check we can have an overloaded function
    check_types("void f(float x) {} void f(uint x) {}");

    // Check we can not redefine an overload
    check_fail("void f(float x) {} void f(float x) {}");

    // out and inout are considered as different overloads
    check_types("void f(float x) {} void f(out float x) {}");
    check_types("void f(float x) {} void f(inout float x) {}");

    // Other modifiers do not change the type for overload conflicts
    check_fail("void f(float x) {} void f(precise float x) {}");
    check_fail("void f(float x) {} void f(sample float x) {}");
    check_fail("void f(float x) {} void f(vertices float x) {}");

    // const does not change the signature type
    check_fail("void f(float x) {} void f(const float x) {}");
    check_fail("void f(const float x) {} void f(float x) {}");
    check_fail("void f(float x[1]) {} void f(const float x[1]) {}");
    check_fail("void f(const float x[1]) {} void f(float x[1]) {}");
}

#[test]
fn check_function_overload_selection_float_uint() {
    check_types(
        "
    float f(float x) { return x; }
    uint f(uint x) { return x; }
    void main() {
        assert_type<float>(f(0.0));
        assert_type<uint>(f(0u));
        assert_type<uint>(f(0));
    }
",
    );
}

#[test]
fn check_function_overload_selection_int_to_bool() {
    // Integers prefer bool casts over float casts
    check_types(
        "
    bool f(bool x) { return x; }
    float f(float x) { return x; }
    void main() {
        assert_type<bool>(f(0));
    }
",
    );
}

#[test]
fn check_function_overload_selection_half_float_double() {
    // Half will cast to float over double
    check_types(
        "
    float f(float x) { return x; }
    double f(double x) { return x; }
    void main() {
        assert_type<float>(f(0.0h));
    }
",
    );

    // Float literals without a type are equally float and double
    check_fail(
        "
    float f(float x) { return x; }
    double f(double x) { return x; }
    void main() {
        f(0.0);
    }
",
    );

    // Float literals without a type are equally half and double
    check_fail(
        "
    half f(half x) { return x; }
    double f(double x) { return x; }
    void main() {
        f(0.0);
    }
",
    );

    // Float literals without a type are equally half and float
    check_fail(
        "
    half f(half x) { return x; }
    float f(float x) { return x; }
    void main() {
        f(0.0);
    }
",
    );

    // Float prefers to cast up to double than down to half
    check_types(
        "
    half f(half x) { return x; }
    double f(double x) { return x; }
    void main() {
        assert_type<double>(f(0.0f));
    }
",
    );
}

#[test]
fn check_function_overload_selection_no_half_exception() {
    // Bools have equal preference for half as uint
    check_fail(
        "
    half f(half x) { return x; }
    uint f(uint x) { return x; }
    void main() {
        f(false);
    }
",
    );

    // Bools have equal preference for half as float
    check_fail(
        "
    half f(half x) { return x; }
    float f(float x) { return x; }
    void main() {
        f(false);
    }
",
    );
}

#[test]
fn check_function_overload_selection_int_uint() {
    // Exact type picks the exact match
    check_types(
        "
    int f(int x) { return x; }
    uint f(uint x) { return x; }
    void main() {
        assert_type<uint>(f(0u));
        assert_type<int>(f((int)0));
    }
",
    );

    // Literal int is ambiguous (even after applying unary minus operator)
    check_fail(
        "
    int f(int x) { return x; }
    uint f(uint x) { return x; }
    void main() {
        f(-1);
    }
",
    );

    // Float literals without a type have equal preference for signed or unsigned int casts
    check_fail(
        "
    int f(int x) { return x; }
    uint f(uint x) { return x; }
    void main() {
        f(0.0);
    }
",
    );

    // Float has equal preference for signed or unsigned int casts
    check_fail(
        "
    int f(int x) { return x; }
    uint f(uint x) { return x; }
    void main() {
        f(0.0f);
    }
",
    );

    // Half has equal preference for signed or unsigned int casts
    check_fail(
        "
    int f(int x) { return x; }
    uint f(uint x) { return x; }
    void main() {
        f(0.0h);
    }
",
    );

    // Double has equal preference for signed or unsigned int casts
    check_fail(
        "
    int f(int x) { return x; }
    uint f(uint x) { return x; }
    void main() {
        f(0.0L);
    }
",
    );

    // bool has equal preference for signed or unsigned int casts
    check_fail(
        "
    int f(int x) { return x; }
    uint f(uint x) { return x; }
    void main() {
        f(true);
    }
",
    );
}

#[test]
fn check_function_default_argument() {
    // Check we can declare functions with default arguments
    check_types("float f(float x, float y = 1.5f) { return x; }");
    check_types("float f(float x = 1.5f) { return x; }");

    // Check we can call function with default arguments either with or without the default value
    check_types(
        "
    float f(float x, float y = 1.5f) { return x; }
    void main() {
        assert_type<float>(f(0.0f, 1.0f));
        assert_type<float>(f(0.0f));
    }
",
    );

    // Default arguments must all come at the end
    check_fail("float f(float x, float y = 1.5f, float z) { return x; }");
    check_fail("float f(float x = 1.5f, float y) { return x; }");
}

#[test]
fn check_operators_on_literals() {
    check_types("void f() { true * false / true; }");
    check_types("void f() { 1 * 1 / 1; }");
    check_types("void f() { 1 * 1 / 1.0; }");
    check_types("void f() { 1 * 1.0 / 1; }");
    check_types("void f() { 1 * 1.0 / 1.0; }");
    check_types("void f() { 1.0 * 1 / 1; }");
    check_types("void f() { 1.0 * 1 / 1.0; }");
    check_types("void f() { 1.0 * 1.0 / 1; }");
    check_types("void f() { 1.0 * 1.0 / 1.0; }");

    check_types("void f() { true + 0; }");
    check_types("void f() { true + 0.0; }");
    check_types("void f() { 0 + true; }");
    check_types("void f() { 0.0 + true; }");
}

#[test]
fn check_function_param_type_modifiers() {
    // trivial cases
    check_types("void f(float x) {}");

    check_types("void f(const float x) {}");
    check_types("void f(float const x) {}");
    check_types("void f(volatile float x) {}");
    check_types("void f(float volatile x) {}");
    check_types("void f(const volatile float x) {}");

    // Matrix orderings are allowed (on matrix types)
    check_types("void f(row_major float4x4 x) {}");
    check_types("void f(column_major float4x4 x) {}");
    check_fail("void f(row_major column_major float4x4 x) {}");
    check_fail("void f(row_major float x) {}");
    check_fail("void f(column_major float x) {}");

    // unorm/snorm are allowed
    check_types("void f(unorm float x) {}");
    check_types("void f(snorm float x) {}");
    check_fail("void f(unorm snorm float x) {}");
    check_fail("void f(unorm uint x) {}");
    check_fail("void f(snorm uint x) {}");

    // storage types are not allowed
    check_fail("void f(static float x) {}");
    check_fail("void f(extern float x) {}");
    check_fail("void f(groupshared float x) {}");

    // precise is allowed
    // This is allowed in HLSL but does not seem to be very good at actually applying precise if not one of the entry point outputs
    check_types("void f(precise float x) {}");

    // parameter output types are allowed
    check_types("void f(in float x) {}");
    check_types("void f(out float x) {}");
    check_types("void f(inout float x) {}");
    check_fail("void f(in out float x) {}");
    check_fail("void f(out inout float x) {}");
    check_fail("void f(inout in float x) {}");
}

#[test]
fn check_function_param_type_interpolators() {
    // interpolation modifiers are allowed
    check_types("void f(nointerpolation float x) {}");
    check_types("void f(linear float x) {}");
    check_types("void f(centroid float x) {}");
    check_types("void f(noperspective float x) {}");
    check_types("void f(sample float x) {}");

    // Only one interpolation modifier is allowed
    check_fail("void f(nointerpolation linear float x) {}");
    check_fail("void f(linear centroid float x) {}");
    check_fail("void f(centroid noperspective float x) {}");
    check_fail("void f(noperspective sample float x) {}");
    check_fail("void f(sample nointerpolation float x) {}");
}

#[test]
fn check_function_param_type_geometry_modifiers() {
    check_types("void f(point float x[1]) {}");
    check_types("void f(line float x[2]) {}");
    check_types("void f(triangle float x[3]) {}");
    check_types("void f(lineadj float x[4]) {}");
    check_types("void f(triangleadj float x[6]) {}");

    check_types("void f(in triangle float x[3]) {}");

    // No validation for modifier used on correct element count
}

#[test]
fn check_function_param_type_mesh_modifiers() {
    check_types("void f(out vertices float x[1]) {}");
    check_fail("void f(vertices float x[1]) {}");
    check_fail("void f(out nointerpolation vertices float x[1]) {}");

    check_types("void f(out primitives float x[1]) {}");
    check_fail("void f(primitives float x[1]) {}");
    check_fail("void f(out primitives nointerpolation float x[1]) {}");

    check_types("void f(out indices uint3 x[1]) {}");
    check_types("void f(out indices uint2 x[1]) {}");
    check_fail("void f(out indices uint x[1]) {}");
    check_fail("void f(indices uint3 x[1]) {}");
    check_fail("void f(out nointerpolation indices uint3 x[1]) {}");

    check_types("void f(in payload float x) {}");
    check_fail("void f(payload float x) {}");
    check_fail("void f(in nointerpolation payload float x) {}");
}

#[test]
fn check_function_return_type_modifiers() {
    // trivial cases
    check_types("float f() { return 0; }");

    // const is allowed
    check_types("const float f() { return 0; }");
    check_types("float const f() { return 0; }");

    // volatile is not allowed
    check_fail("volatile float f() { return 0; }");

    // volatile can sneak in indirectly
    check_types("typedef const float X; X f() { return 0; }");
    check_types("typedef volatile float X; X f() { return 0; }");
    check_types("typedef const volatile float X; X f() { return 0; }");

    // Matrix orderings are allowed (on matrix types)
    check_types("row_major float4x4 f() { return (float4x4)0; }");
    check_types("column_major float4x4 f() { return (float4x4)0; }");
    check_fail("row_major column_major float4x4 f() { return (float4x4)0; }");
    check_fail("row_major float f() { return (float4x4)0; }");
    check_fail("column_major float f() { return (float4x4)0; }");

    // unorm/snorm are allowed
    check_types("unorm float f() { return 0; }");
    check_types("snorm float f() { return 0; }");
    check_fail("unorm snorm float f() { return 0; }");
    check_fail("unorm uint f() { return 0; }");
    check_fail("snorm uint f() { return 0; }");

    // only static storage type is allowed - this binds to the function not the return
    check_types("static float f() { return 0; }");
    check_fail("extern float f() { return 0; }");
    check_fail("groupshared float f() { return 0; }");

    // precise is allowed
    // This is allowed in HLSL but does not seem to be very good at actually applying precise if not one of the entry point outputs
    check_types("precise float f() { return 0; }");

    // parameter output types are not allowed
    check_fail("in float f() { return 0; }");
    check_fail("out float f() { return 0; }");
    check_fail("inout float f() { return 0; }");

    // Interpolation modifiers are not allowed
    check_fail("nointerpolation float f() { return 0; }");
    check_fail("linear float f() { return 0; }");
    check_fail("centroid float f() { return 0; }");
    check_fail("noperspective float f() { return 0; }");
    check_fail("sample float f() { return 0; }");

    // Geometry shader primitive types are not allowed
    check_fail("point float f() { return 0; }");
    check_fail("line float f() { return 0; }");
    check_fail("triangle float f() { return 0; }");
    check_fail("lineadj float f() { return 0; }");
    check_fail("triangleadj float f() { return 0; }");

    // Mesh shader output modifiers are not allowed
    check_fail("vertices float f() { return 0; }");
    check_fail("primitives float f() { return 0; }");
    check_fail("indices float f() { return 0; }");
    check_fail("payload float f() { return 0; }");
}

#[test]
fn check_function_with_contextual_keyword_names() {
    check_types("void precise() { precise(); (precise)(); }");
    check_fail("void nointerpolation() {}");
    check_fail("void linear() {}");
    check_fail("void centroid() {}");
    check_fail("void noperspective() {}");
    check_types("void sample() { sample(); (sample)(); }");
    check_fail("void point() {}");
    check_fail("void line() {}");
    check_fail("void triangle() {}");
    check_fail("void lineadj() {}");
    check_fail("void triangleadj() {}");
    check_types("void vertices() { vertices(); (vertices)(); }");
    check_types("void primitives() { primitives(); (primitives)(); }");
    check_types("void indices() { indices(); (indices)(); }");
    check_types("void payload() { payload(); (payload)(); }");
}

#[test]
fn check_local_variable_type_modifiers() {
    // trivial case
    check_types("void f() { float x; }");

    // only default (local) and static storage types are allowed
    check_types("void f() { static float x; }");
    check_fail("void f() { extern float x; }");
    check_fail("void f() { groupshared float x; }");
    check_types("void f() { static static float x; }");

    // const is allowed
    check_types("void f() { const float x; }");
    check_types("void f() { static const float x; }");
    check_fail("void f() { extern const float x; }");
    check_fail("void f() { groupshared const float x; }");

    // volatile is allowed
    check_types("void f() { volatile float x; }");
    check_types("void f() { const volatile float x; }");

    // Matrix orderings are allowed (on matrix types)
    check_types("void f() { row_major float4x4 x; }");
    check_types("void f() { column_major float4x4 x; }");
    check_fail("void f() { row_major column_major float4x4 x; }");
    check_fail("void f() { row_major float4 x; }");
    check_fail("void f() { column_major float4 x; }");

    // parameter output types are not allowed
    check_fail("void f() { in float x; }");
    check_fail("void f() { out float x; }");
    check_fail("void f() { inout float x; }");

    // precise is allowed
    check_types("void f() { precise float x; }");

    // Interpolation modifiers are not allowed
    check_fail("void f() { nointerpolation float x; }");
    check_fail("void f() { linear float x; }");
    check_fail("void f() { centroid float x; }");
    check_fail("void f() { noperspective float x; }");
    check_fail("void f() { sample float x; }");

    // Geometry shader primitive types are not allowed
    check_fail("void f() { point float x; }");
    check_fail("void f() { line float x; }");
    check_fail("void f() { triangle float x; }");
    check_fail("void f() { lineadj float x; }");
    check_fail("void f() { triangleadj float x; }");

    // Mesh shader output modifiers are not allowed
    check_fail("void f() { vertices float x; }");
    check_fail("void f() { primitives float x; }");
    check_fail("void f() { indices uint3 x; }");
    check_fail("void f() { payload float x; }");
}

#[test]
fn check_local_variable_with_contextual_keyword_names() {
    // Mirror HLSL's interesting selection of names that work
    check_types("void f() { uint precise; }");
    check_fail("void f() { uint nointerpolation; }");
    check_fail("void f() { uint linear; }");
    check_fail("void f() { uint centroid; }");
    check_fail("void f() { uint noperspective; }");
    check_types("void f() { uint sample; sample = sample; }");
    check_fail("void f() { uint point; }");
    check_fail("void f() { uint line; }");
    check_fail("void f() { uint triangle; }");
    check_fail("void f() { uint lineadj; }");
    check_fail("void f() { uint triangleadj; }");
    check_types("void f() { uint vertices; vertices = vertices; }");
    check_types("void f() { uint primitives; primitives = (primitives); }");
    check_types("void f() { uint indices; (indices) = indices; }");
    check_types("void f() { uint payload; payload = payload; }");
}

#[test]
fn check_function_templates() {
    check_types("template<typename T> void f() {}");
    check_types("template<typename T> void f(T v) {}");
    check_types("template<typename T> T f(T v) { return v; }");

    // Duplicate argument name should fail
    check_fail("template<typename T, typename T> T f(T v) { return v; }");

    // Invoke the template function with explicit template arguments
    check_types("template<typename T> T f(T v) { return v; } void main() { f<float>(0.0); }");

    // Invoke it twice with different arguments
    check_types(
        "template<typename T> T f(T v) { return v; } void main() { f<float>(0.0); f<uint>(0); }",
    );

    // Invoke it twice with the same arguments
    check_types(
        "template<typename T> T f(T v) { return v; } void main() { f<float>(0.0); f<float>(0.0); }",
    );

    // Infer the template arguments
    check_types(
        "template<typename T> T f(T v) { return v; } void main() { assert_type<float>(f(0.0f)); }",
    );

    // Infer float from float literal
    check_types(
        "template<typename T> T f(T v) { return v; } void main() { assert_type<float>(f(0.0)); }",
    );

    // Check support for using template parameters in template arguments
    check_types("template<typename T> T g(T v) { return v + 1; } template<typename T> T f(T v) { return g<T>(v); } void main() { f<float>(0.0); }");

    // Check that we do not fail type checking due to function contents
    check_types("template<typename T> T f(T v) { return v + 1; }");

    // And instantiate it
    check_types("template<typename T> T f(T v) { return v + 1; } void main() {  f<float>(0.0); }");

    // Check we can declare instances of a template type inside the function body
    check_types("template<typename T> void f() { T t; t + 1; }; void main() { f<float>(); };");

    // Check that picking a type that does not create a valid instantiation fails
    check_fail(
        "struct S {}; template<typename T> void f() { T t; t + 1; } void main() { f<S>(); }",
    );

    // Check structs can pass through a template function
    check_types(
        "struct S {}; template<typename T> T f(T t) { return t; } void main() { S s1; S s2 = f<S>(s1); }",
    );

    // Check multiple type arguments are accepted
    check_types("template<typename T1, typename T2> T1 f(T1 v1, T2 v2) { return v1 + v2; } void main() { f<float, int>(0.0, 0); }");

    // Check that the template arguments can make an array
    check_types("template<typename T> T f(T v[2]) { return v[1]; } void main() { float a[2] = { 0, 1 }; f<float>(a); }");

    // Check that array parameters can infer the required type
    check_types("template<typename T> T f(T v[2]) { return v[1]; } void main() { float a[2] = { 0, 1 }; f(a); }");

    // Check that the template arguments can make a vector
    check_types("template<typename T> T f(vector<T, 2> v) { return v[1]; } void main() { float2 a = { 0, 1 }; f<float>(a); }");

    // Check that vector parameters can infer the required type
    check_types("template<typename T> T f(vector<T, 2> v) { return v[1]; } void main() { float2 a = { 0, 1 }; f(a); }");

    // Check that the template arguments can make a matrix
    check_types("template<typename T> T f(matrix<T, 2, 2> v) { return v[1][0]; } void main() { float2x2 a; f<float>(a); }");

    // Check that matrix parameters can infer the required type
    check_types("template<typename T> T f(matrix<T, 2, 2> v) { return v[1][0]; } void main() { float2x2 a; f(a); }");

    // Redefinitions should fail - different typename
    check_fail("template<typename T> void f() {} template<typename G> void f() {}");

    // Redefinitions should fail - same typename
    check_fail("template<typename T> void f() {} template<typename T> void f() {}");

    // The name can be omitted
    check_types("template<typename> void f() {}");
    check_types("template<typename, typename> void f() {}");
}

#[test]
fn check_function_template_non_type() {
    // Check that a template value argument can make an array with a size
    check_types("template<uint L> void f() { float data[L]; } void main() { f<2>(); f<4>(); }");

    // Check we can pass a value argument with a type from a type argument
    check_types("template<typename T, T L> void f() { T data[L]; } void main() { f<uint, 2>(); f<float, 4>(); }");

    // Redefinitions should fail - different name
    check_fail("template<uint X> void f() {} template<uint Y> void f() {}");

    // Different value type os not a redefinition
    check_types("template<uint X> void f() {} template<int Y> void f() {}");

    // But we don't handle redefinitions where the value parameter is based on another template type

    // Check that passing a type to a non-type parameter fails gracefully
    check_fail("template<uint L> void f() {} void main() { f<uint>(); }");

    // Check that passing a value to a type parameter fails gracefully
    check_fail("template<typename T> void f() {} void main() { f<2>(); }");

    // Check we are able to pick the valid parameter type - type first version
    check_types(
        "template<typename T> void f() {} template<uint L> void f() {} void main() { f<uint>(); f<0>(); }",
    );

    // Check we are able to pick the valid parameter type - value first version
    check_types(
        "template<uint L> void f() {} template<typename T> void f() {} void main() { f<uint>(); f<0>(); }",
    );

    // The name can be omitted
    check_types("template<uint> void f() {}");
    check_types("template<uint, uint> void f() {}");
}

#[test]
fn check_function_attributes() {
    check_types("[numthreads(64, 1, 1)] void Main() {}");
    check_fail("[numthreads] void Main() {}");
    check_fail("[numthreads(64)] void Main() {}");

    check_types("[WaveSize(64)] void Main() {}");
    check_types("[wavesize(64)] void Main() {}");
    check_fail("[WaveSize] void Main() {}");
    check_fail("[WaveSize(64, 32)] void Main() {}");

    check_types("[outputtopology(\"triangle\")] void Main() {}");
    check_fail("[outputtopology(4)] void Main() {}");

    check_types("[WaveSize(64)] [numthreads(64, 1, 1)] void Main() {}");
}

#[test]
fn check_intrinsic_calls() {
    check_types("void f() { AllMemoryBarrier(); }");
    check_types("void f() { AllMemoryBarrierWithGroupSync(); }");
    check_types("void f() { DeviceMemoryBarrier(); }");
    check_types("void f() { DeviceMemoryBarrierWithGroupSync(); }");
    check_types("void f() { GroupMemoryBarrier(); }");
    check_types("void f() { GroupMemoryBarrierWithGroupSync(); }");

    check_types("void f() { all(false); }");
    check_types("void f() { all(bool2(false, false)); }");
    check_types("void f() { all(bool3(false, false, false)); }");
    check_types("void f() { all(bool4(false, false, false, false)); }");

    check_types("void f() { any(false); }");
    check_types("void f() { any(bool2(false, false)); }");
    check_types("void f() { any(bool3(false, false, false)); }");
    check_types("void f() { any(bool4(false, false, false, false)); }");

    check_types("void f() { abs(int(7)); }");
    check_types("void f() { abs(int2(7, 7)); }");
    check_types("void f() { abs(int3(7, 7, 7)); }");
    check_types("void f() { abs(int4(7, 7, 7, 7)); }");
    check_types("void f() { abs(float(7.0)); }");
    check_types("void f() { abs(float2(7.0, 7.0)); }");
    check_types("void f() { abs(float3(7.0, 7.0, 7.0)); }");
    check_types("void f() { abs(float4(7.0, 7.0, 7.0, 7.0)); }");

    check_types("Buffer<float4> buf; void main() { buf.Load(0); }");
    check_types("Buffer buf; void main() { buf.Load(0); }");
    check_types("Buffer<uint4> buf; void main() { buf.Load(0); }");
    check_types("RWBuffer<float4> buf; void main() { buf.Load(0); }");
    check_fail("RWBuffer buf;");

    check_types("RWByteAddressBuffer buf; void main() { uint previous; buf.InterlockedAdd(0, 1, previous); }");
}

#[test]
fn check_structs() {
    check_types("struct S {}; S value;");
    check_fail("struct S {}; S value = 0;");
    check_types("struct S {}; void main() { S value; }");
    check_fail("struct S {}; void main() { S value = 0; }");
    check_types("struct S1 {}; struct S2 {}; S1 g1; S2 g2;");
    check_fail("struct S {}; struct S {};");

    // Declaring a struct with the same name as a variable is allowed
    check_types("struct S {}; int S;");
    check_types("int S; struct S {};");
}

#[test]
fn check_struct_with_contextual_keyword_names() {
    check_types("struct NormalIdentifier {};");
    check_fail("struct precise {};");
    check_fail("struct nointerpolation {};");
    check_fail("struct linear {};");
    check_fail("struct centroid {};");
    check_fail("struct noperspective {};");
    check_fail("struct sample {};");
    check_fail("struct point {};");
    check_fail("struct line {};");
    check_fail("struct triangle {};");
    check_fail("struct lineadj {};");
    check_fail("struct triangleadj {};");
    check_fail("struct vertices {};");
    check_fail("struct primitives {};");
    check_fail("struct indices {};");
    check_fail("struct payload {};");
}

#[test]
fn check_struct_member_type_modifiers() {
    // trivial cases
    check_types("struct S { uint x; };");
    check_types("struct S { float x; uint y; };");

    // const and volatile are not allowed on struct fields directly
    check_fail("struct S { const float x; };");
    check_fail("struct S { volatile float x; };");

    // but they can sneak in indirectly
    check_types("typedef const float X; struct S { X x; };");
    check_types("typedef volatile float X; struct S { X x; };");
    check_types("typedef const volatile float X; struct S { X x; };");

    // Matrix orderings are allowed (on matrix types)
    check_types("struct S { row_major float4x4 x; };");
    check_types("struct S { column_major float4x4 x; };");
    check_fail("struct S { row_major float4 x; };");
    check_fail("struct S { column_major float4 x; };");
    check_fail("struct S { row_major column_major float4x4 x; };");

    // unorm/snorm are allowed
    check_types("struct S { unorm float x; };");
    check_types("struct S { snorm float x; };");
    check_fail("struct S { unorm snorm float4 x; };");
    check_fail("struct S { unorm uint x; };");
    check_fail("struct S { snorm uint x; };");

    // only static storage type is allowed
    check_types("struct S { static float x; };");
    check_fail("struct S { extern float x; };");
    check_fail("struct S { groupshared float x; };");
    check_types("struct S { static static float x; };");

    // precise is allowed
    check_types("struct S { precise float x; };");

    // Interpolation modifiers are valid on structs
    check_types("struct S { nointerpolation float x; };");
    check_types("struct S { linear float x; };");
    check_types("struct S { centroid float x; };");
    check_types("struct S { noperspective float x; };");
    check_types("struct S { sample float x; };");

    // Only one interpolation modifier is allowed
    check_fail("struct S { nointerpolation linear float x; };");
    check_fail("struct S { linear centroid float x; };");
    check_fail("struct S { centroid noperspective float x; };");
    check_fail("struct S { noperspective sample float x; };");
    check_fail("struct S { sample nointerpolation float x; };");

    // Geometry shader primitive types are not allowed
    check_fail("struct S { point float x; };");
    check_fail("struct S { line float x; };");
    check_fail("struct S { triangle float x; };");
    check_fail("struct S { lineadj float x; };");
    check_fail("struct S { triangleadj float x; };");

    // Mesh shader output modifiers are not allowed
    check_fail("struct S { vertices float x; };");
    check_fail("struct S { primitives float x; };");
    check_fail("struct S { indices float x; };");
    check_fail("struct S { payload float x; };");

    // parameter output types are not allowed
    check_fail("struct S { in float x; };");
    check_fail("struct S { out float x; };");
    check_fail("struct S { inout float x; };");
}

#[test]
fn check_struct_member_with_contextual_keyword_names() {
    check_types("struct S { uint precise; }; void f(S s) { s.precise; }");
    check_fail("struct S { uint nointerpolation; };");
    check_fail("struct S { uint linear; };");
    check_fail("struct S { uint centroid; };");
    check_fail("struct S { uint noperspective; };");
    check_types("struct S { uint sample; }; void f(S s) { s.sample; }");
    check_fail("struct S { uint point; };");
    check_fail("struct S { uint line; };");
    check_fail("struct S { uint triangle; };");
    check_fail("struct S { uint lineadj; };");
    check_fail("struct S { uint triangleadj; };");
    check_types("struct S { uint vertices; }; void f(S s) { s.vertices; }");
    check_types("struct S { uint primitives; }; void f(S s) { s.primitives; }");
    check_types("struct S { uint indices; }; void f(S s) { s.indices; }");
    check_types("struct S { uint payload; }; void f(S s) { s.payload; }");
}

#[test]
fn check_struct_methods() {
    check_types("struct S { void f() {} }; void main() { S s; s.f(); }");
    check_fail("struct S { void f() {} }; void main() { f(); }");
    check_types("struct S { void f() { g(); } void g() { f(); } };");
    check_types("struct S { int x; void f() { x = x + 1; } };");
    check_types("struct S { void f() { x = x + 1; } int x; };");
    check_types("struct S { float x; bool f() { if (y) { x = x + 1; return false; } return true; } int y; };");
    check_types("struct S { void f(S s) {} };");
    check_types("struct S { float x; bool f(S s) { if (y || s.y) { x = x + 1; s.x = s.x + 1; return false; } return true; } int y; };");

    // Methods can be declared to be defined later out of line - but defining them later is not currently supported
    check_types("struct S { void f(); };");

    // No duplicate members
    check_fail("struct S { uint x; uint x; };");

    // No duplicate methods
    check_fail("struct S { void f() {} void f() {} };");
    check_fail("struct S { void f(); void f(); };");
    check_fail("struct S { void f() {} void f(); };");
    check_fail("struct S { void f(); void f() {} };");

    // No methods and members with the same name
    check_fail("struct S { void x() {} uint x; };");
    check_fail("struct S { uint x; void x() {} };");
}

#[test]
fn check_struct_method_with_contextual_keyword_names() {
    check_types("struct S { void precise() {} };");
    check_fail("struct S { void nointerpolation() {} };");
    check_fail("struct S { void linear() {} };");
    check_fail("struct S { void centroid() {} };");
    check_fail("struct S { void noperspective() {} };");
    check_types("struct S { void sample() {} }; void f(S s) { s.sample(); }");
    check_fail("struct S { void point() {} };");
    check_fail("struct S { void line() {} };");
    check_fail("struct S { void triangle() {} };");
    check_fail("struct S { void lineadj() {} };");
    check_fail("struct S { void triangleadj() {} };");
    check_types("struct S { void vertices() {} }; void f(S s) { s.vertices(); }");
    check_types("struct S { void primitives() {} }; void f(S s) { s.primitives(); }");
    check_types("struct S { void indices() {} }; void f(S s) { s.indices(); }");
    check_types("struct S { void payload() {} }; void f(S s) { s.payload(); }");
}

#[test]
fn check_struct_method_templates() {
    // Check a non-method template function can return the struct types we will use in the next test
    check_types("template<typename T> T f(T t) { return t; } struct M {}; void main() { M m1; M m2 = f<M>(m1); };");

    // Check a (non-templated) struct template method can have template arguments to the function
    // Currently limited to explicitly listing the types in the call
    check_types("struct S { template<typename T> T f(T t) { return t; } }; struct M {}; void main() { S s; M m1; M m2 = s.f<M>(m1); };");

    // Check we can have type dependent operations in an uninstantiated template
    check_types("struct S { template<typename T> void f() { t + 1; } };");

    // And do the above test but with a valid instantiation
    check_types(
        "struct S { template<typename T> void f(T t) { t + 1; } }; void main() { S s; s.f<float>(0.0); };",
    );

    // Check we can declare instances of template types as local variables
    check_types(
        "struct S { template<typename T> void f() { T t; t + 1; } }; void main() { S s; s.f<float>(); };",
    );

    // Check (non-templated) struct template method can access both the template type and the containing struct type
    check_types("struct S { template<typename T> void f() { S s1; T t1; { S s2; T t2; return; } } }; struct M {}; void main() { S s; s.f<M>(); };");

    // Check a templated method on a templated struct receives the correct types
    check_types(
        "template<typename T>
        struct S
        {
            template<typename G>
            void f()
            {
                T t;
                G g;
                assert_type<int2>(t);
                assert_type<float3>(g);
            }
        };

        void main()
        {
            S<int2> s;
            s.f<float3>();
        }",
    );
}

#[test]
fn check_struct_templates() {
    // Check basic templated struct
    check_types("template<typename T> struct S {};");

    // Check that redefinitions fail
    check_fail("template<typename T> struct S {}; template<typename G> struct S {};");

    // Check we can create an instance of a templated struct
    check_types("template<typename T> struct S {}; S<int> s;");

    // Check the same template argument name can be used twice
    check_types(
        "template<typename T> struct M {}; template<typename T> struct N {}; M<int> m; N<int> n;",
    );

    // Ensure we fail if we try to make a templated struct without providing template arguments
    check_fail("template<typename T> struct S {}; S s;");

    // Check we can use the template argument in a method
    check_types("template<typename T> struct S { void f() { T t; t = uint2(3, 4); } }; S<int2> s;");

    // Check we fail if the type is not usable for the template
    check_fail(
        "template<typename T> struct S { void f() { T t; t = uint2(3, 4); } }; S<float4x4> s;",
    );

    // Check that the same template arguments give the same type
    check_types("template<typename T> struct S {}; void main() { S<int> s1; S<int> s2; s1 = s2; }");

    // Check that different template arguments give different types
    check_fail(
        "template<typename T> struct S {}; void main() { S<int> s1; S<float> s2; s1 = s2; }",
    );
}

#[test]
fn check_enums() {
    // Check basic usage of empty enums
    check_types("enum S {}; S value;");
    check_fail("enum S {}; S value = 0;");
    check_types("enum S {}; void main() { S value; }");
    check_fail("enum S {}; void main() { S value = 0; }");
    check_types("enum S1 {}; enum S2 {}; S1 g1; S2 g2;");

    // Check we can declare enum values
    check_types("enum S { A };");
    check_types("enum S { A, B, };");
    check_types("enum S { A = 2, B, C = 1 };");

    // Check we can not declare the same value name multiple times
    check_fail("enum S { A, A };");

    // Check the enum value expression can be evaluated to a constant
    check_types("static const uint c_value = 1; enum S { A = c_value };");
    check_fail("static uint c_value = 1; enum S { A = c_value };");

    // Check we can use the enum values unscoped
    check_types("enum S { A = 1 }; void f() { S s = A; }");

    // Check we can use the enum values scoped
    check_types("enum S { A = 1 }; void f() { S s = S::A; }");
}

#[test]
fn check_enum_value_init_references() {
    // Check we can initialize a value with another value
    // The implied zero value on A is of type int
    check_types("enum S { A, B = assert_eval<int>(A, (int)0) };");
    check_types("enum S { A, B = assert_eval<int>(S::A, (int)0) };");
    check_types("enum S { A, B = assert_eval<int>(::S::A, (int)0) };");
    check_types("enum S { A, B = assert_eval<int>(::A, (int)0) };");

    // We can not use enum values declared later
    check_fail("enum S { A = B, B };");

    // Check we can initialize a value with a combination of other values
    // A and B don't have proper enum type here so don't need to cast to int
    // A is uint as it is initialized with a uint
    // B is an untyped int as it is initialized with a literal - assert_eval will check for type equivalence but we can't write the direct type to check
    check_types("enum S { A = 2u, B = 3, C = assert_eval<uint>(A, 2u) | assert_eval(B, 3) };");
    check_types("enum S { A = 2u, B = 3, C = S::A | S::B };");
    check_types("enum S { A = 2u, B = 3, C = ::S::A | ::S::B };");
    check_types("enum S { A = 2u, B = 3, C = ::A | ::B };");

    // Other enums can be initialized from previous enums
    check_types("enum S1 { A }; enum S2 { Z = A };");

    // If evaluating A and B in a constant expression we have an implicit cast to int
    // Test the case where the int cast is explicit
    check_types("enum S1 { A, B }; enum S2 { Z = (int)A | (int)assert_type<S1>(B) };");
    check_types("enum S1 { A, B }; enum S2 { Z = (int)S1::A | (int)assert_type<S1>(S1::B) };");
    check_types("enum S1 { A, B }; enum S2 { Z = (int)::S1::A | (int)assert_type<S1>(::S1::B) };");
    check_types("enum S1 { A, B }; enum S2 { Z = (int)::A | (int)assert_type<S1>(::B) };");
}

#[test]
fn check_enum_int_cast() {
    // Check that an enum value can implicitly cast to int
    check_types("enum S { A = 1 }; void f() { int s = A; }");

    // Check that it still works from scoped name
    check_types("enum S { A = 1 }; void f() { int s = S::A; }");

    // Test that A and B implicitly cast to int before evaluating
    check_types("enum S1 { A, B }; enum S2 { X = A | B, Z = assert_type<S1>(assert_type<S1>(A) | assert_type<S1>(B)) };");
    check_types("enum S1 { A, B }; enum S2 { X = A + B, Z = assert_type<S1>(assert_type<S1>(A) + assert_type<S1>(B)) };");

    // Most unary operations should convert to int
    check_types("enum S1 { A }; enum S2 { Z = ~A };");
    check_types("enum S1 { A }; enum S2 { Z = !A };");
    check_types("enum S1 { A }; enum S2 { Z = +A };");
    check_types("enum S1 { A }; enum S2 { Z = -A };");

    // Increment operators should not be supported
    check_fail("enum S1 { A }; enum S2 { Z = ++A };");
    check_fail("enum S1 { A }; enum S2 { Z = --A };");

    // Check evaluation of various operators work with enum -> primitive cast
    check_types(
        "enum S { A = 2u, B = 3, C = false }; void f() {
        assert_type<S>(A);
        assert_type<S>(B);
        assert_type<S>(C);
        assert_eval<S>(+A, (S)2);
        assert_eval<S>(+B, (S)3);
        assert_eval<S>(+C, (S)0);
        assert_eval<S>(-A, (S)-2);
        assert_eval<S>(-B, (S)-3);
        assert_eval<S>(-C, (S)0);
        assert_eval<S>(~A, ~(S)2);
        assert_eval<S>(~B, ~(S)3);
        assert_eval<S>(~C, ~(S)0);
        assert_eval<bool>(!A, false);
        assert_eval<bool>(!B, false);
        assert_eval<bool>(!C, true);
        assert_eval<bool>(A < B, true);
        assert_eval<bool>(B < C, false);
    }",
    );
}

#[test]
fn check_enum_name_conflicts() {
    // Two enums can not have the same name
    check_fail("enum S {}; enum S {};");

    // Declaring an enum with the same name as a variable is allowed
    check_types("enum S {}; int S;");
    check_types("int S; enum S {};");

    // Declaring an enum with the same name as a struct is not allowed
    check_fail("enum S {}; struct S {};");
    check_fail("struct S {}; enum S {};");

    // Declaring an enum with the same name as a typedef is not allowed
    check_fail("typedef float S; enum S {};");
    check_fail("enum S {}; typedef float S;");

    // Check an enum value can not have the same name as a struct
    check_fail("struct A {}; enum S { A };");

    // Check an enum value can not have the same name as an enum
    check_fail("enum A {}; enum S { A };");

    // Check an enum value can not have the same name as its own enum
    check_fail("enum S { S };");

    // Check an enum value can not have the same name as a typedef
    // HLSL allows this but we do not
    check_fail("typedef float A; enum S { A };");

    // Check that a global can have the same name as an enum - and we can even use the name in a value expression
    check_types("static const uint S = 1; enum S { A = S };");
}

#[test]
fn check_enum_valid_value_types() {
    check_types("enum E { A = 0 };");
    check_types("enum E { A = false };");
    check_types("enum E { A = 0u };");
    check_fail("enum E { A = 0.0 };");
    check_fail("enum E { A = 0.0h };");
    check_fail("enum E { A = 0.0L };");
    check_types("enum E1 { A1 }; enum E2 { A2 = A1 };");
    check_fail("struct S {}; static const S s; enum E { A = s };");
}

#[test]
fn check_enum_valid_value_range() {
    check_types("enum E { A = 0, B = 0xFFFFFFFFu };");
    check_types("enum E { A = 0, B = 0xFFFFFFFF };");
    check_types("enum E { A = -2147483648, B = 2147483647 };");
    // We don't support long enums
    check_fail("enum E { A = 0, B = 18446744073709551615 };");
}

#[test]
fn check_typedef() {
    check_types("typedef uint u32;");
    check_types("typedef uint u32; u32 x = 1;");
    check_types("typedef uint u32x4[4];");
    check_types("typedef uint u32x4[4]; u32x4 v = { 2, 3, 4, 5 };");
}

#[test]
fn check_typedef_type_modifiers() {
    // const and volatile are allowed
    check_types("typedef const float X;");
    check_types("typedef volatile float X;");
    check_types("typedef const volatile float X;");

    // Matrix orderings are allowed (on matrix types)
    check_types("typedef row_major float4x4 X;");
    check_types("typedef column_major float4x4 X;");
    check_fail("typedef row_major float4 X;");
    check_fail("typedef column_major float4 X;");
    check_fail("typedef row_major column_major float4x4 X;");

    // unorm/snorm are allowed
    check_types("typedef unorm float X;");
    check_types("typedef snorm float X;");
    check_fail("typedef unorm snorm float X;");
    check_fail("typedef unorm uint X;");
    check_fail("typedef snorm uint X;");

    // Storage classes are not allowed
    check_fail("typedef static uint X;");
    check_fail("typedef extern uint X;");
    check_fail("typedef groupshared uint X;");

    // precise is not allowed on typedef
    // This is allowed in HLSL but does not do anything
    check_fail("typedef precise float X;");

    // Interpolation modifiers are not allowed
    check_fail("typedef nointerpolation uint X;");
    check_fail("typedef linear uint X;");
    check_fail("typedef centroid uint X;");
    check_fail("typedef noperspective uint X;");
    check_fail("typedef sample uint X;");

    // Geometry shader primitive types are not allowed
    check_fail("typedef point uint X;");
    check_fail("typedef line uint X;");
    check_fail("typedef triangle uint X;");
    check_fail("typedef lineadj uint X;");
    check_fail("typedef triangleadj uint X;");

    // Mesh shader output modifiers are not allowed
    check_fail("typedef vertices uint X;");
    check_fail("typedef primitives uint X;");
    check_fail("typedef indices uint3 X;");
    check_fail("typedef payload uint X;");

    // parameter output types are not allowed
    check_fail("typedef in uint X;");
    check_fail("typedef out uint X;");
    check_fail("typedef inout uint X;");
}

#[test]
fn check_typedef_with_contextual_keyword_names() {
    // HLSL permits some of these but then they are mostly unusable
    check_types("typedef uint precise;");
    check_fail("typedef uint nointerpolation;");
    check_fail("typedef uint linear;");
    check_fail("typedef uint centroid;");
    check_fail("typedef uint noperspective;");
    check_types("typedef uint sample;");
    check_fail("typedef uint point;");
    check_fail("typedef uint line;");
    check_fail("typedef uint triangle;");
    check_fail("typedef uint lineadj;");
    check_fail("typedef uint triangleadj;");
    check_types("typedef uint vertices;");
    check_types("typedef uint primitives;");
    check_types("typedef uint3 indices;");
    check_types("typedef uint payload;");
}

#[test]
fn check_cbuffer() {
    check_types(
        "cbuffer MyConstants { float c1; uint c2; } float f() { { return c1 + float(c2); } }",
    );

    // Declaring the same block twice should fail
    check_fail("cbuffer MyConstants { float c1; } cbuffer MyConstants { float c1; }");

    // Declaring a block with the same name as a struct or variable is allowed
    check_types("cbuffer S {} struct S {};");
    check_types("struct S {}; cbuffer S {}");
    check_types("cbuffer S {} int S;");
    check_types("int S; cbuffer S {}");

    // Constant buffers may have register annotations
    check_types("cbuffer MyConstants : register(b4) { float c1; uint c2; }");

    // But they must be in the 'b' group
    check_fail("cbuffer MyConstants : register(t4) { float c1; uint c2; }");

    // Check for duplicate member names
    check_fail("cbuffer A { float c1; } cbuffer B { float c1; }");
    check_fail("float c1; cbuffer B { float c1; }");
    check_fail("float c1() {} cbuffer B { float c1; }");
    check_fail("enum A { c1 = 0 }; cbuffer B { float c1; }");
    check_fail("namespace c1 {} cbuffer B { float c1; }");

    // Check members trigger duplicate names elsewhere
    check_fail("cbuffer B { float c1; } float c1;");
    check_fail("cbuffer B { float c1; } float c1() {}");
    check_fail("cbuffer B { float c1; } enum A { c1 = 0 };");
    check_fail("cbuffer B { float c1; } namespace c1 {}");

    // While cbuffer member names are global they do not conflict
    check_types("namespace N { float c1; } cbuffer B { float c1; }");
    check_types("cbuffer B { float c1; } namespace N { float c1; }");
}

#[test]
fn check_cbuffer_member_type_modifiers() {
    // trivial cases
    check_types("cbuffer S { uint x; }");
    check_types("cbuffer S { float x; uint y; }");

    // const is allowed
    check_types("cbuffer S { const float x; }");

    // volatile is not allowed
    check_fail("cbuffer S { volatile float x; }");

    // volatile can sneak in indirectly
    check_types("typedef const float X; cbuffer S { X x; }");
    check_types("typedef volatile float X; cbuffer S { X x; }");
    check_types("typedef const volatile float X; cbuffer S { X x; }");

    // Matrix orderings are allowed (on matrix types)
    check_types("cbuffer S { row_major float4x4 x; }");
    check_types("cbuffer S { column_major float4x4 x; }");
    check_fail("cbuffer S { row_major float4 x; }");
    check_fail("cbuffer S { column_major float4 x; }");
    check_fail("cbuffer S { row_major column_major float4x4 x; }");

    // unorm/snorm are allowed
    check_types("cbuffer S { unorm float x; }");
    check_types("cbuffer S { snorm float x; }");
    check_fail("cbuffer S { unorm snorm float4 x; }");
    check_fail("cbuffer S { unorm uint x; }");
    check_fail("cbuffer S { snorm uint x; }");

    // storage types are not allowed
    // HLSL does not error for these but they are meaningless
    check_fail("cbuffer S { static float x; }");
    check_fail("cbuffer S { extern float x; }");
    check_fail("cbuffer S { groupshared float x; }");

    // precise is allowed
    // This is very pointless as nothing in shader generates a value for a constant buffer member
    // But as it is pointless we do not have to do anything with it
    check_types("cbuffer S { precise float x; }");

    // Interpolation modifiers are not allowed
    check_fail("cbuffer S { nointerpolation float x; }");
    check_fail("cbuffer S { linear float x; }");
    check_fail("cbuffer S { centroid float x; }");
    check_fail("cbuffer S { noperspective float x; }");
    check_fail("cbuffer S { sample float x; }");

    // Geometry shader primitive types are not allowed
    check_fail("cbuffer S { point float x; }");
    check_fail("cbuffer S { line float x; }");
    check_fail("cbuffer S { triangle float x; }");
    check_fail("cbuffer S { lineadj float x; }");
    check_fail("cbuffer S { triangleadj float x; }");

    // Mesh shader output modifiers are not allowed
    check_fail("cbuffer S { vertices float x; }");
    check_fail("cbuffer S { primitives float x; }");
    check_fail("cbuffer S { indices float x; }");
    check_fail("cbuffer S { payload float x; }");

    // parameter output types are not allowed
    check_fail("cbuffer S { in float x; }");
    check_fail("cbuffer S { out float x; }");
    check_fail("cbuffer S { inout float x; }");
}

#[test]
fn check_cbuffer_member_with_contextual_keyword_names() {
    check_types("cbuffer MyConstants { uint precise; }; void f() { precise; }");
    check_fail("cbuffer MyConstants { uint nointerpolation; };");
    check_fail("cbuffer MyConstants { uint linear; };");
    check_fail("cbuffer MyConstants { uint centroid; };");
    check_fail("cbuffer MyConstants { uint noperspective; };");
    check_types("cbuffer MyConstants { uint sample; }; void f() { sample; }");
    check_fail("cbuffer MyConstants { uint point; };");
    check_fail("cbuffer MyConstants { uint line; };");
    check_fail("cbuffer MyConstants { uint triangle; };");
    check_fail("cbuffer MyConstants { uint lineadj; };");
    check_fail("cbuffer MyConstants { uint triangleadj; };");
    check_types("cbuffer MyConstants { uint vertices; }; void f() { vertices; }");
    check_types("cbuffer MyConstants { uint primitives; }; void f() { primitives; }");
    check_types("cbuffer MyConstants { uint indices; }; void f() { indices; }");
    check_types("cbuffer MyConstants { uint payload; }; void f() { payload; }");
}

#[test]
fn check_variable_scope() {
    check_fail("int a; int a;");
    check_fail("void f() { int a; int a; }");
}

#[test]
fn check_object_type_globals() {
    check_types("Buffer buf;");
    check_types("Buffer<uint4> buf;");
    check_types("Buffer<const uint4> buf;");
    check_types("RWBuffer<float4> buf;");
    check_types("RWBuffer<uint4> buf;");
    check_types("RWBuffer<const float4> buf;");

    check_types("ByteAddressBuffer buf;");
    check_types("RWByteAddressBuffer buf;");

    check_types("StructuredBuffer<uint4> buf;");
    check_types("struct S {}; StructuredBuffer<S> buf;");
    check_types("struct S {}; StructuredBuffer<const S> buf;");
    check_types("RWStructuredBuffer<uint4> buf;");
    check_types("struct S {}; RWStructuredBuffer<S> buf;");
    check_types("struct S {}; RWStructuredBuffer<const S> buf;");

    check_types("Texture2D tex;");
    check_types("Texture2D<uint4> tex;");
    check_types("Texture2D<const uint4> tex;");
    check_types("Texture2DArray tex;");
    check_types("Texture2DArray<uint4> tex;");
    check_types("Texture2DArray<const uint4> tex;");
    check_types("RWTexture2D<float4> tex;");
    check_types("RWTexture2D<uint4> tex;");
    check_types("RWTexture2D<const uint4> tex;");
    check_types("RWTexture2DArray<float4> tex;");
    check_types("RWTexture2DArray<uint4> tex;");
    check_types("RWTexture2DArray<const uint4> tex;");

    // HLSL forbids constant buffers with non-struct types - maybe we should as well
    check_types("ConstantBuffer<uint4> buf;");
    check_types("struct S {}; ConstantBuffer<S> buf;");

    // Check allowed register types for Texture2D
    check_types("Texture2D<float4> tex : register(t9);");
    check_fail("Texture2D<float4> tex : register(u9);");
    check_fail("Texture2D<float4> tex : register(b9);");
    check_fail("Texture2D<float4> tex : register(s9);");

    // Check allowed register types for RWTexture2D
    check_types("RWTexture2D<float4> tex : register(u10);");
    check_fail("RWTexture2D<float4> tex : register(t10);");
    check_fail("RWTexture2D<float4> tex : register(b10);");
    check_fail("RWTexture2D<float4> tex : register(s10);");

    // Constant buffers may have register annotations of type 'b'
    check_types("ConstantBuffer<uint4> buf : register(b4);");
    check_fail("ConstantBuffer<uint4> buf : register(u4);");

    // Register annotations are not permitted on non-objects
    check_fail("uint x : register(t4);");
    check_fail("uint x : register(u4);");
    check_fail("uint x : register(b4);");
    check_fail("uint x : register(s4);");
}

#[test]
fn check_texture_index() {
    check_types("RWTexture2D<float4> tex; void main() { tex[uint2(0, 0)] = float4(1, 2, 3, 4); }");
    check_types(
        "RWTexture2D<float4> tex; void sub(out float4 v) {} void main() { sub(tex[uint2(0, 0)]); }",
    );
    check_fail(
        "RWTexture2D<const float4> tex; void main() { tex[uint2(0, 0)] = float4(1, 2, 3, 4); }",
    );

    check_types(
        "RWTexture2DArray<float4> tex; void main() { tex[uint3(0, 0, 0)] = float4(1, 2, 3, 4); }",
    );
    check_types(
        "RWTexture2DArray<float4> tex; void sub(out float4 v) {} void main() { sub(tex[uint3(0, 0, 0)]); }",
    );
    check_fail(
        "RWTexture2DArray<const float4> tex; void main() { tex[uint3(0, 0, 0)] = float4(1, 2, 3, 4); }",
    );

    check_types("Texture2D tex; void main() { float x = tex[uint2(0, 0)]; }");
    check_fail("Texture2D tex; void main() { tex[uint2(0, 0)] = float4(1, 2, 3, 4); }");
    check_fail("Texture2D tex; void sub(out float4 v) {} void main() { sub(tex[uint2(0, 0)]); }");

    check_types("Texture2DArray tex; void main() { float x = tex[uint3(0, 0, 0)]; }");
    check_fail("Texture2DArray tex; void main() { tex[uint3(0, 0, 0)] = float4(1, 2, 3, 4); }");
    check_fail(
        "Texture2DArray tex; void sub(out float4 v) {} void main() { sub(tex[uint3(0, 0, 0)]); }",
    );
}

#[test]
fn check_buffer_index() {
    check_types("RWBuffer<float> buf; void main() { buf[0] = 3; }");
    check_types("RWBuffer<float> buf; void sub(out float v) {} void main() { sub(buf[0]); }");
    check_fail("RWBuffer<const float> buf; void main() { buf[0] = 3; }");
    check_types("Buffer<float> buf; void main() { float v = buf[0]; }");
    check_types("Buffer<const float> buf; void main() { float v = buf[0]; }");
    check_fail("Buffer<float> buf; void main() { buf[0] = 3; }");
    check_fail("Buffer<float> buf; void sub(out float v) {} void main() { sub(buf[0]); }");
}

#[test]
fn check_swizzle() {
    check_types("void sub(out float4 v) {} void main() { float4 t; sub(t.wzyx); }");
    check_fail("void sub(out float4 v) {} void main() { float4 t; sub(t.wwww); }");

    check_types("void sub(float4 v) {} void main() { float t; sub(t.xxxx); }");
    check_fail("void sub(float4 v) {} void main() { float t; sub(t.yyyy); }");
}

#[test]
fn check_matrix_swizzle() {
    check_types("void main() { float4x4 t; t._m00; t._m01; t._m02; t._m03; t._m10; t._m11; t._m12; t._m13; t._m20; t._m21; t._m22; t._m23; t._m30; t._m31; t._m32; t._m33; }");
    check_types("void main() { float4x4 t; t._11; t._12; t._13; t._14; t._21; t._22; t._23; t._24; t._31; t._32; t._33; t._34; t._41; t._42; t._43; t._44; }");
    check_types("void main() { float4x4 t; t._m00_m01; t._m00_m01_m02; t._m00_m01_m02_m03; }");
    check_fail("void main() { float4x4 t; t._m00_m01_m02_m03_m10; }");
    check_fail("void main() { float3x3 t; t._m00_m01_m02_m03; }");
    check_fail("void main() { float3x3 t; t._m00_12; }");
    check_fail("void main() { float3x3 t; t._11_m01; }");
    check_types("void sub(out float4 v) {} void main() { float4x4 t; sub(t._m00_m01_m02_m03); }");
    check_fail("void sub(out float4 v) {} void main() { float4x4 t; sub(t._m00_m01_m00_m03); }");
}

#[test]
fn check_operator_prefix_increment() {
    check_types("void g(out int x) {} void f() { int x = 0u; g(++x); }");
    check_fail("void f() { const int x = 0u; ++x; }");

    check_types("void g(out uint x) {} void f() { uint x = 0u; g(++x); }");
    check_fail("void f() { const uint x = 0u; ++x; }");

    check_types("void g(out float x) {} void f() { float x = 0u; g(++x); }");
    check_fail("void f() { const float x = 0u; ++x; }");
}

#[test]
fn check_operator_postfix_increment() {
    check_types("void f() { int x = 0; x++; }");
    check_fail("void g(out int x) {} void f() { int x = 0; g(x++); }");
    check_fail("void f() { const int x = 0; x++; }");

    check_types("void f() { uint x = 0; x++; }");
    check_fail("void g(out uint x) {} void f() { uint x = 0; g(x++); }");
    check_fail("void f() { const uint x = 0; x++; }");

    check_types("void f() { float x = 0; x++; }");
    check_fail("void g(out float x) {} void f() { float x = 0; g(x++); }");
    check_fail("void f() { const float x = 0; x++; }");
}

#[test]
fn check_operator_prefix_decrement() {
    check_types("void g(out int x) {} void f() { int x = 0; g(--x); }");
    check_fail("void f() { const int x = 0; --x; }");

    check_types("void g(out uint x) {} void f() { uint x = 0; g(--x); }");
    check_fail("void f() { const uint x = 0; --x; }");

    check_types("void g(out float x) {} void f() { float x = 0; g(--x); }");
    check_fail("void f() { const float x = 0; --x; }");
}

#[test]
fn check_operator_postfix_decrement() {
    check_types("void f() { int x = 0; x++; }");
    check_fail("void g(out int x) {} void f() { int x = 0; g(x--); }");
    check_fail("void f() { const int x = 0; x--; }");

    check_types("void f() { uint x = 0; x++; }");
    check_fail("void g(out uint x) {} void f() { uint x = 0; g(x--); }");
    check_fail("void f() { const uint x = 0; x--; }");

    check_types("void f() { float x = 0; x++; }");
    check_fail("void g(out float x) {} void f() { float x = 0; g(x--); }");
    check_fail("void f() { const float x = 0; x--; }");
}

#[test]
fn check_operator_unary_plus() {
    check_types("void f() { int x = 0; +x; }");
    check_fail("void g(out int x) {} void f() { int x = 0; g(+x); }");
    check_types("void f() { const int x = 0; +x; }");

    check_types("void f() { uint x = 0u; +x; }");
    check_fail("void g(out uint x) {} void f() { uint x = 0; g(+x); }");
    check_types("void f() { const uint x = 0; +x; }");

    check_types("void f() { float x = 0; +x; }");
    check_fail("void g(out float x) {} void f() { float x = 0; g(+x); }");
    check_types("void f() { const float x = 0; +x; }");
}

#[test]
fn check_operator_minus() {
    check_types("void f() { int x = 0; -x; }");
    check_fail("void g(out int x) {} void f() { int x = 0; g(-x); }");
    check_types("void f() { const int x = 0; -x; }");

    check_types("void f() { uint x = 0u; -x; }");
    check_fail("void g(out uint x) {} void f() { uint x = 0; g(-x); }");
    check_types("void f() { const uint x = 0; -x; }");

    check_types("void f() { float x = 0; +x; }");
    check_fail("void g(out float x) {} void f() { float x = 0; g(-x); }");
    check_types("void f() { const float x = 0; -x; }");
}

#[test]
fn check_operator_logical_not() {
    check_types("void f() { bool x = false; assert_type<bool>(!x); }");
    check_types("void f() { const bool x = false; assert_type<bool>(!x); }");
    check_types("void f() { uint x = 0; assert_type<bool>(!x); }");
    check_types("void f() { const uint x = 0; assert_type<bool>(!x); }");
    check_types("void f() { int x = 0; assert_type<bool>(!x); }");
    check_types("void f() { const int x = 0; assert_type<bool>(!x); }");
    check_types("void f() { float x = 0; assert_type<bool>(!x); }");
    check_types("void f() { const float x = 0; assert_type<bool>(!x); }");
    check_fail("void g(out bool x) {} void f() { bool x = false; g(!x); }");
}

#[test]
fn check_operator_bitwise_not() {
    check_types("void f() { uint x = 0; ~x; assert_type<uint>(~x); }");
    check_types("void f() { const uint x = 0; ~x; assert_type<uint>(~x); }");
    check_types("void f() { int x = 0; ~x; assert_type<int>(~x); }");
    check_types("void f() { const int x = 0; ~x; assert_type<int>(~x); }");
    check_types("void f() { bool x = false; assert_type<int>(~x); }");
    check_types("void f() { const bool x = false; assert_type<int>(~x); }");
    check_fail("void f() { float x = 0; ~x; }");
    check_fail("void f() { const float x = 0; ~x; }");
    check_fail("void g(out uint x) {} void f() { uint x = 0; g(~x); }");
}

#[test]
fn check_operator_add() {
    check_types("void f() { uint x, y = 0; assert_type<uint>(x + y); }");
    check_types("void f() { const uint x, y = 0; assert_type<uint>(x + y); }");
    check_types("void f() { float x; float2 y; assert_type<float2>(x + y); }");
}

#[test]
fn check_operator_subtract() {
    check_types("void f() { uint x, y = 0; assert_type<uint>(x - y); }");
    check_types("void f() { const uint x, y = 0; assert_type<uint>(x - y); }");
    check_types("void f() { float x; float2 y; assert_type<float2>(x - y); }");
}

#[test]
fn check_operator_multiply() {
    check_types("void f() { uint x, y = 0; assert_type<uint>(x * y); }");
    check_types("void f() { const uint x, y = 0; assert_type<uint>(x * y); }");
    // vector expansion on x
    check_types("void f() { float x; float2 y; assert_type<float2>(x * y); }");
    // vector truncation on y
    check_types("void f() { float2 x; float3 y; assert_type<float2>(x * y); }");
}

#[test]
fn check_operator_divide() {
    check_types("void f() { uint x, y = 0; uint z = x / y; }");
    check_types("void f() { const uint x, y = 0; uint z = x / y; }");
    check_types("void f() { float x; float2 y; float2 z = x / y; }");
    check_types("void f() { float2 x; float y; float2 z = x / y; }");
}

#[test]
fn check_operator_left_shift() {
    check_types("void f() { uint x, y = 0; assert_type<uint>(x << y); }");
    check_types("void f() { const uint x, y = 0; assert_type<uint>(x << y); }");
    check_types("void f() { uint x; uint2 y; assert_type<uint2>(x << y); }");
    check_types("void f() { uint2 x; uint y; assert_type<uint2>(x << y); }");

    check_fail("void f() { half x; half y; x << y; }");
    check_fail("void f() { float x; float y; x << y; }");
    check_fail("void f() { double x; double y; x << y; }");
    check_fail("void f() { float x; uint y; x << y; }");
    check_fail("void f() { uint x; float y; x << y; }");
}

#[test]
fn check_operator_right_shift() {
    check_types("void f() { uint x, y = 0; assert_type<uint>(x >> y); }");
    check_types("void f() { const uint x, y = 0; assert_type<uint>(x >> y); }");
    check_types("void f() { uint x; uint2 y; assert_type<uint2>(x >> y); }");
    check_types("void f() { uint2 x; uint y; assert_type<uint2>(x >> y); }");

    check_fail("void f() { half x; half y; x >> y; }");
    check_fail("void f() { float x; float y; x >> y; }");
    check_fail("void f() { double x; double y; x >> y; }");
    check_fail("void f() { float x; uint y; x >> y; }");
    check_fail("void f() { uint x; float y; x >> y; }");
}

#[test]
fn check_operator_bitwise_and() {
    check_types("void f() { uint x, y = 0; assert_type<uint>(x & y); }");
    check_types("void f() { const uint x, y = 0; assert_type<uint>(x & y); }");
    check_types("void f() { uint x; uint2 y; assert_type<uint2>(x & y); }");
    check_types("void f() { uint2 x; uint3 y; assert_type<uint2>(x & y); }");
}

#[test]
fn check_operator_bitwise_or() {
    check_types("void f() { uint x, y = 0; assert_type<uint>(x | y); }");
    check_types("void f() { const uint x, y = 0; assert_type<uint>(x | y); }");
    check_types("void f() { uint x; uint2 y; assert_type<uint2>(x | y); }");
    check_types("void f() { uint2 x; uint3 y; assert_type<uint2>(x | y); }");
}

#[test]
fn check_operator_bitwise_xor() {
    check_types("void f() { uint x, y = 0; assert_type<uint>(x ^ y); }");
    check_types("void f() { const uint x, y = 0; assert_type<uint>(x ^ y); }");
    check_types("void f() { uint x; uint2 y; assert_type<uint2>(x ^ y); }");
    check_types("void f() { uint2 x; uint3 y; assert_type<uint2>(x ^ y); }");
}

#[test]
fn check_operator_boolean_and() {
    check_types("void f() { bool x, y = 0; assert_type<bool>(x && y); }");
    check_types("void f() { const bool x, y = 0; assert_type<bool>(x && y); }");

    check_types("void f() { uint x, y = 0; assert_type<bool>(x && y); }");
    check_types("void f() { const uint x, y = 0; assert_type<bool>(x && y); }");

    // Non-scalars forbidden from short circuiting operators
    check_fail("void f() { uint x; uint2 y; uint2 z = x && y; }");
    check_fail("void f() { uint2 x; uint y; uint2 z = x && y; }");
    check_fail("void f() { uint x; uint1 y; uint2 z = x && y; }");
}

#[test]
fn check_operator_boolean_or() {
    check_types("void f() { bool x, y = 0; assert_type<bool>(x || y); }");
    check_types("void f() { const bool x, y = 0; assert_type<bool>(x || y); }");

    check_types("void f() { uint x, y = 0; assert_type<bool>(x || y); }");
    check_types("void f() { const uint x, y = 0; assert_type<bool>(x || y); }");

    // Non-scalars forbidden from short circuiting operators
    check_fail("void f() { uint x; uint2 y; uint2 z = x || y; }");
    check_fail("void f() { uint2 x; uint y; uint2 z = x || y; }");
    check_fail("void f() { uint x; uint1 y; uint2 z = x || y; }");
}

#[test]
fn check_operator_ternary_conditional() {
    check_types("void f() { bool x, y, z = 0; assert_type<bool>(x ? y : z); }");
    check_types("void f() { const bool x, y, z = 0; assert_type<bool>(x ? y : z); }");

    check_types("void f() { uint x, y, z = 0; assert_type<uint>(x ? y : z); }");
    check_types("void f() { const uint x, y, z = 0; assert_type<uint>(x ? y : z); }");

    // Non-scalars forbidden from condition
    check_types("void f() { uint x; uint2 y, z; uint2 a = x ? y : z; }");
    check_fail("void f() { uint2 x; uint2 y, z; uint2 a = x ? y : z; }");
}

#[test]
fn check_operator_less_than() {
    check_types("void f() { uint x, y = 0; assert_type<bool>(x < y); }");
    check_types("void f() { const uint x, y = 0; assert_type<bool>(x < y); }");

    check_types("void f() { bool x, y = 0; assert_type<bool>(x < y); }");
    check_types("void f() { const bool x, y = 0; assert_type<bool>(x < y); }");

    check_types("void f() { uint x; uint2 y; assert_type<bool2>(x < y); }");
}

#[test]
fn check_operator_less_equal() {
    check_types("void f() { uint x, y = 0; assert_type<bool>(x <= y); }");
    check_types("void f() { const uint x, y = 0; assert_type<bool>(x <= y); }");

    check_types("void f() { bool x, y = 0; assert_type<bool>(x <= y); }");
    check_types("void f() { const bool x, y = 0; assert_type<bool>(x <= y); }");

    check_types("void f() { uint x; uint2 y; assert_type<bool2>(x <= y); }");
}

#[test]
fn check_operator_greater_than() {
    check_types("void f() { uint x, y = 0; assert_type<bool>(x > y); }");
    check_types("void f() { const uint x, y = 0; assert_type<bool>(x > y); }");

    check_types("void f() { bool x, y = 0; assert_type<bool>(x > y); }");
    check_types("void f() { const bool x, y = 0; assert_type<bool>(x > y); }");

    check_types("void f() { uint x; uint2 y; assert_type<bool2>(x > y); }");
}

#[test]
fn check_operator_greater_equal() {
    check_types("void f() { uint x, y = 0; assert_type<bool>(x >= y); }");
    check_types("void f() { const uint x, y = 0; assert_type<bool>(x >= y); }");

    check_types("void f() { bool x, y = 0; assert_type<bool>(x >= y); }");
    check_types("void f() { const bool x, y = 0; assert_type<bool>(x >= y); }");

    check_types("void f() { uint x; uint2 y; assert_type<bool2>(x >= y); }");
}

#[test]
fn check_operator_equality() {
    check_types("void f() { uint x, y = 0; assert_type<bool>(x == y); }");
    check_types("void f() { const uint x, y = 0; assert_type<bool>(x == y); }");

    check_types("void f() { bool x, y = 0; assert_type<bool>(x == y); }");
    check_types("void f() { const bool x, y = 0; assert_type<bool>(x == y); }");

    check_types("void f() { uint x; uint2 y; assert_type<bool2>(x == y); }");
    check_types("void f() { uint2 x; uint3 y; assert_type<bool2>(x == y); }");
}

#[test]
fn check_operator_inequality() {
    check_types("void f() { uint x, y = 0; assert_type<bool>(x != y); }");
    check_types("void f() { const uint x, y = 0; assert_type<bool>(x != y); }");

    check_types("void f() { bool x, y = 0; assert_type<bool>(x != y); }");
    check_types("void f() { const bool x, y = 0; assert_type<bool>(x != y); }");

    check_types("void f() { uint x; uint2 y; assert_type<bool2>(x != y); }");
}

#[test]
fn check_assignment() {
    // Trivial assignment
    check_types("void f() { int x = 1; int y = x; }");
    // Sum assignment
    check_types("void f() { int x = 1; int y = x; y += x; }");
    // Sum assignment to self
    check_types("void f() { int x = 1; x += x; }");
    // Sum assignment with rvalue value is okay (although this is unsequenced)
    check_types("void f() { int x = 1; x += x += x * x; }");
    // Sum assignment with rvalue object is not
    check_fail("void f() { int x = 1; x * x += x; }");
    // Still not okay
    check_fail("void f() { int x = 1; x += x * x += x; }");
    // Try with chaining all the assignment operators (also unsequenced)
    check_types("void f() { int x = 1; x = x += x /= x -= x *= x %= x; }");
    // Check different types on each side but which have casts
    check_types("void f() { int x = 1; float y = x; y += x; x += y; }");
    // Check different types work in a chain (again unsequenced)
    check_types("void f() { int x = 1; float y = x; y += x += y /= x; }");
    // Check assignment of different type with implicit vector truncation
    check_types("void f() { uint x; uint2 y; x = y; }");
    // Check assignment of different type with implicit vector splat
    check_types("void f() { uint2 x; uint y; x = y; }");
    // Check sum assignment of different type with implicit vector truncation
    check_types("void f() { uint x; uint2 y; x += y; }");
    // Check sum assignment of different type with implicit vector splat
    check_types("void f() { uint2 x; uint y; x += y; }");
}

#[test]
fn check_operator_sizeof() {
    check_types("void f() { assert_type<uint>(sizeof(bool)); }");
    check_types("void f() { assert_type<uint>(sizeof(uint)); }");
    check_types("void f() { assert_type<uint>(sizeof(float)); }");
    check_types("void f() { assert_type<uint>(sizeof(const uint)); }");
    check_types("void f() { assert_type<uint>(sizeof(uint const)); }");

    // sizeof can apply to expressions
    check_types("void f() { uint x; assert_type<uint>(sizeof(x)); }");
    check_types("void f() { uint x; assert_type<uint>(sizeof(x + x)); }");

    // sizeof can apply to struct types and expressions with struct type
    check_types("struct S {}; void f() { assert_type<uint>(sizeof(S)); }");
    check_types("struct S {}; void f() { S s; assert_type<uint>(sizeof(s)); }");

    // sizeof can apply to enum types and expressions with enum type
    check_types("enum E {}; void f() { assert_type<uint>(sizeof(E)); }");
    check_types("enum E {}; void f() { E e; assert_type<uint>(sizeof(e)); }");

    // expression parsed for non-type uint and fails to type check when uint is a type instead of non-type identifier
    check_fail("void f() { uint x; assert_type<uint>(sizeof(uint | 7)); }");
    check_fail("void f() { uint x; assert_type<uint>(sizeof(7 | uint)); }");

    // sizeof takes an expression with local variable T - not type T
    check_types("typedef float T; void f() { uint T; assert_type<uint>(sizeof(T|7)); }");

    // uint literal has uint type so has a size
    check_types("void f() { sizeof(0u); }");

    // literal int does not have a size
    check_fail("void f() { sizeof(0); }");

    // using sizeof() to determine an enum value is okay
    check_types("enum E { A = 0, B = assert_type<uint>(sizeof(0.0f)) };");

    // using sizeof() on an enum to init another enum is okoy
    check_types("enum E1 { A = 0 }; enum E2 { B = assert_type<uint>(sizeof(A)) };");

    // using sizeof() on an enum value from the same enum is okay if the value is typed
    check_types("enum E { A = 0u, B = assert_type<uint>(sizeof(A)) };");

    // using sizeof() on an enum value from the same enum is not okay if the value is untyped
    check_fail("enum E { A = 0, B = assert_type<uint>(sizeof(A)) };");
}

#[test]
fn check_constructors() {
    check_types("int x = int(7);");
    check_types("int x = (int)(7);");

    //  Fails as we expect a single argument
    check_fail("int x = int(7, 6);");

    check_types("vector<int, 2> x = int2(7, 6);");
    check_types("int2 x = vector<int, 2>(7, 6);");
}

#[test]
fn check_fake_constructor() {
    // These are valid but are actually a casted comma expression and not a constructor
    check_types("int x = (int)(7, 6);");
    check_types("int2 x = (int2)(7, 6);");
    check_types("vector<int, 2> x = (int2)(7, 6);");
    check_types("int2 x = (vector<int, 2>)(7, 6);");
}

#[test]
fn check_cast() {
    check_types("ByteAddressBuffer buf; void f() { uint x = buf.Load(0); }");
    check_types("ByteAddressBuffer buf; void f() { uint x = (uint)buf.Load(0); }");
    check_types("ByteAddressBuffer buf; void f() { uint x = (uint)buf.Load<uint>(0); }");
    check_types("struct S {}; ByteAddressBuffer buf; void f() { S x = (S)buf.Load<S>(0); }");
    check_types("struct S {}; ByteAddressBuffer buf; void f() { S x = buf.Load<S>(0); }");
    check_types("struct S {}; BufferAddress buf; void f() { S x = (S)buf.Load<S>(0); }");
    check_types("struct S {}; BufferAddress buf; void f() { S x = buf.Load<S>(0); }");
}

#[test]
fn check_cast_or_not() {
    // If a or b are types changes how the expression parses
    check_types("int a; int b; int c; void f() { (a) + (b) + (c); }");
    check_types("typedef int a; typedef int b; int c; void f() { (a) + (b) + (c); }");
    check_types("typedef int a; int b; int c; void f() { (a) + (b) + (c); }");
    check_types("int a; typedef int b; int c; void f() { (a) + (b) + (c); }");

    // Although c can never be a type in this expression
    check_fail("int a; int b; typedef int c; void f() { (a) + (b) + (c); }");
    check_fail("typedef int a; int b; typedef int c; void f() { (a) + (b) + (c); }");
    check_fail("typedef int a; typedef int b; typedef int c; void f() { (a) + (b) + (c); }");
    check_fail("int a; typedef int b; typedef int c; void f() { (a) + (b) + (c); }");
}

#[test]
fn check_namespaces() {
    // Check we can declare a namespace
    check_types("namespace N {}");

    // Check we can declare a global in a namespace
    check_types("namespace N { int a; }");

    // Check we can declare a global in a namespace with the same name as in another namespace
    check_types("int a; namespace N { int a; }");

    // Check we can use values from the parent namespace in the current namespace
    check_types("int a; namespace N { void f() { a; } }");

    // Check we can use values in the previous opening of the same namespace
    check_types("namespace N { int a; } namespace N { void f() { a; } }");

    // Check we can not access values in another namespace
    check_fail("namespace M { int a; } namespace N { void f() { a; } }");

    // Check we can use values in another namespace by qualifying the name
    check_types("namespace M { int a; } namespace N { void f() { M::a; } }");

    // Check we can use values in another namespace by qualifying the name - with rooted name
    check_types("namespace M { int a; } namespace N { void f() { ::M::a; } }");

    // Check we can not access a type name inside a namespace
    check_fail("namespace N { struct S {}; } StructuredBuffer<S> g_buffer;");

    // Check we can access a type name inside a namespace by qualifying the name
    check_types("namespace N { struct S {}; } StructuredBuffer<N::S> g_buffer;");

    // Check we can use namespaced structs in various global buffers
    check_types(
        "namespace N1
        {
            struct S { uint x; };
            StructuredBuffer<S> g_buffer1;
            StructuredBuffer<N1::S> g_buffer2;
        }
        namespace N2
        {
            struct S { N1::S s; };
            StructuredBuffer<S> g_buffer1;
            StructuredBuffer<N2::S> g_buffer2;
            StructuredBuffer<N1::S> g_buffer3;
        }
        StructuredBuffer<N1::S> g_buffer1;
        StructuredBuffer<N2::S> g_buffer2;
    ",
    );

    // Check we fail to compile a namespace with an existing symbol name
    check_fail("int N; namespace N {}");
    check_fail("void N() {} namespace N {}");
    check_fail("enum P { N = 0 }; namespace N {}");
    check_fail("struct N {}; namespace N {}");
    check_fail("enum N {}; namespace N {}");
}

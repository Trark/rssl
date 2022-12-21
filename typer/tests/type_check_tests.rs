mod shared;
use shared::*;

#[test]
fn check_static_primitive_variables() {
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
    check_types("static float4 g_myArray[2];");
    check_types("#define SIZE 2\nstatic float4 g_myArray[SIZE];");
    check_types("static const uint c_size = 2; static float4 g_myArray[c_size];");
    check_fail("static uint c_size = 2; static float4 g_myArray[c_size];");
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
type_test.rssl:1:32: note: candidate function not viable: void mul(in S)
struct S {}; struct A {}; void mul(S s) {} void main() { A a; mul(a); }
                               ^
",
    );
}

#[test]
fn check_function_templates() {
    check_types("template<typename T> void f() {}");
    check_types("template<typename T> void f(T v) {}");
    check_types("template<typename T> T f(T v) { return v; }");

    // Invoke the template function with explicit template arguments
    check_types("template<typename T> T f(T v) { return v; } void main() { f<float>(0.0); }");

    // We currently do not support inferring the template arguments
    check_fail("template<typename T> T f(T v) { return v; } void main() { f(0.0); }");

    // Check support for using template parameters in template arguments
    check_types("template<typename T> T g(T v) { return v + 1; } template<typename T> T f(T v) { return g<T>(v); } void main() { f<float>(0.0); }");

    // Check that we do not fail type checking due to function contents
    check_types("template<typename T> T f(T v) { return v + 1; }");
    // TODO: This does not generate a complete ir
    check_types("template<typename T> T f(T v) { return v + 1; } void main() { f<float>(0.0); }");

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
fn check_struct_methods() {
    check_types("struct S { void f() {} }; void main() { S s; s.f(); }");
    check_fail("struct S { void f() {} }; void main() { f(); }");
    check_types("struct S { void f() { g(); } void g() { f(); } };");
    check_types("struct S { int x; void f() { x = x + 1; } };");
    check_types("struct S { void f() { x = x + 1; } int x; };");
    check_types("struct S { float x; bool f() { if (y) { x = x + 1; return false; } return true; } int y; };");
    check_types("struct S { void f(S s) {} };");
    check_types("struct S { float x; bool f(S s) { if (y || s.y) { x = x + 1; s.x = s.x + 1; return false; } return true; } int y; };");
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
fn check_typedef() {
    check_types("typedef uint u32;");
    check_types("typedef uint u32; u32 x = 1;");
    check_types("typedef uint u32x4[4];");
    check_types("typedef uint u32x4[4]; u32x4 v = { 2, 3, 4, 5 };");
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
    check_types("RWTexture2D<float4> tex;");
    check_types("RWTexture2D<uint4> tex;");
    check_types("RWTexture2D<const uint4> tex;");

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
    check_types("Texture2D tex; void main() { float x = tex[uint2(0, 0)]; }");
    check_fail("Texture2D tex; void main() { tex[uint2(0, 0)] = float4(1, 2, 3, 4); }");
    check_fail("Texture2D tex; void sub(out float4 v) {} void main() { sub(tex[uint2(0, 0)]); }");
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
    check_types("void f() { bool x = false; !x; }");
    check_fail("void g(out bool x) {} void f() { bool x = false; g(!x); }");
}

#[test]
fn check_operator_bitwise_not() {
    check_types("void f() { uint x = 0; ~x; }");
    check_fail("void g(out uint x) {} void f() { uint x = 0; g(~x); }");
}

#[test]
fn check_operator_add() {
    check_types("void f() { uint x, y = 0; uint z = x + y; }");
    check_types("void f() { const uint x, y = 0; uint z = x + y; }");
}

#[test]
fn check_operator_subtract() {
    check_types("void f() { uint x, y = 0; uint z = x - y; }");
    check_types("void f() { const uint x, y = 0; uint z = x - y; }");
}

#[test]
fn check_operator_multiply() {
    check_types("void f() { uint x, y = 0; uint z = x * y; }");
    check_types("void f() { const uint x, y = 0; uint z = x * y; }");
}

#[test]
fn check_operator_divide() {
    check_types("void f() { uint x, y = 0; uint z = x / y; }");
    check_types("void f() { const uint x, y = 0; uint z = x / y; }");
}

#[test]
fn check_operator_left_shift() {
    check_types("void f() { uint x, y = 0; uint z = x << y; }");
    check_types("void f() { const uint x, y = 0; uint z = x << y; }");
}

#[test]
fn check_operator_right_shift() {
    check_types("void f() { uint x, y = 0; uint z = x >> y; }");
    check_types("void f() { const uint x, y = 0; uint z = x >> y; }");
}

#[test]
fn check_operator_bitwise_and() {
    check_types("void f() { uint x, y = 0; uint z = x & y; }");
    check_types("void f() { const uint x, y = 0; uint z = x & y; }");
}

#[test]
fn check_operator_bitwise_or() {
    check_types("void f() { uint x, y = 0; uint z = x | y; }");
    check_types("void f() { const uint x, y = 0; uint z = x | y; }");
}

#[test]
fn check_operator_bitwise_xor() {
    check_types("void f() { uint x, y = 0; uint z = x ^ y; }");
    check_types("void f() { const uint x, y = 0; uint z = x ^ y; }");
}

#[test]
fn check_operator_boolean_and() {
    check_types("void f() { bool x, y = 0; bool z = x && y; }");
    check_types("void f() { const bool x, y = 0; bool z = x && y; }");

    check_types("void f() { uint x, y = 0; uint z = x && y; }");
    check_types("void f() { const uint x, y = 0; uint z = x && y; }");
}

#[test]
fn check_operator_boolean_or() {
    check_types("void f() { bool x, y = 0; bool z = x || y; }");
    check_types("void f() { const bool x, y = 0; bool z = x || y; }");

    check_types("void f() { uint x, y = 0; uint z = x || y; }");
    check_types("void f() { const uint x, y = 0; uint z = x || y; }");
}

#[test]
fn check_operator_less_than() {
    check_types("void f() { uint x, y = 0; uint z = x < y; }");
    check_types("void f() { const uint x, y = 0; uint z = x < y; }");

    check_types("void f() { bool x, y = 0; bool z = x < y; }");
    check_types("void f() { const bool x, y = 0; bool z = x < y; }");
}

#[test]
fn check_operator_less_equal() {
    check_types("void f() { uint x, y = 0; uint z = x <= y; }");
    check_types("void f() { const uint x, y = 0; uint z = x <= y; }");

    check_types("void f() { bool x, y = 0; bool z = x <= y; }");
    check_types("void f() { const bool x, y = 0; bool z = x <= y; }");
}

#[test]
fn check_operator_greater_than() {
    check_types("void f() { uint x, y = 0; uint z = x > y; }");
    check_types("void f() { const uint x, y = 0; uint z = x > y; }");

    check_types("void f() { bool x, y = 0; bool z = x > y; }");
    check_types("void f() { const bool x, y = 0; bool z = x > y; }");
}

#[test]
fn check_operator_greater_equal() {
    check_types("void f() { uint x, y = 0; uint z = x >= y; }");
    check_types("void f() { const uint x, y = 0; uint z = x >= y; }");

    check_types("void f() { bool x, y = 0; bool z = x >= y; }");
    check_types("void f() { const bool x, y = 0; bool z = x >= y; }");
}

#[test]
fn check_operator_equality() {
    check_types("void f() { uint x, y = 0; uint z = x == y; }");
    check_types("void f() { const uint x, y = 0; uint z = x == y; }");

    check_types("void f() { bool x, y = 0; bool z = x == y; }");
    check_types("void f() { const bool x, y = 0; bool z = x == y; }");
}

#[test]
fn check_operator_inequality() {
    check_types("void f() { uint x, y = 0; uint z = x != y; }");
    check_types("void f() { const uint x, y = 0; uint z = x != y; }");

    check_types("void f() { bool x, y = 0; bool z = x != y; }");
    check_types("void f() { const bool x, y = 0; bool z = x != y; }");
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
    check_types("struct a {}; struct b {}; int c; void f() { (a) + (b) + (c); }");

    // These fail due to creating + expressions between custom types and ints
    check_fail("struct a {}; int b; int c; void f() { (a) + (b) + (c); }");
    check_fail("int a; struct b {}; int c; void f() { (a) + (b) + (c); }");

    // Although c can never be a type in this expression
    check_fail("int a; int b; struct c {}; void f() { (a) + (b) + (c); }");
    check_fail("struct a {}; int b; struct c {}; void f() { (a) + (b) + (c); }");
    check_fail("struct a {}; struct b {}; struct c {}; void f() { (a) + (b) + (c); }");
    check_fail("int a; struct b {}; struct c {}; void f() { (a) + (b) + (c); }");
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
}

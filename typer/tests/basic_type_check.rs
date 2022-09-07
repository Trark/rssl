mod shared;
use shared::*;

#[test]
fn check_static_primitive_variables() {
    check_types("static uint x = 4;");
    check_types("groupshared float lds_data[7];");
}

#[test]
fn reject_invalid_initialisers() {
    check_fail("static uint3 x = float2(1, 2);");
    check_fail("static float4 c[2] = float4(0, 1, 2, 3);");
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
    // TODO: This currently prints an error indicating none were valid
    check_fail("void f(int2 x) {} void f(int3 x) {} void main() { f(int4(0, 0, 0, 0)); }");

    // Check a function can call itself
    check_types("void f() { f(); }");
    // And that overload resolution works with itself
    check_types("void f(int3 x) {} void f(int2 x) { f(int2(3, 4)); }");
    // And that ambiguous overloads fail with itself
    check_fail("void f(int3 x) {} void f(int2 x) { f(1); }");
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

    // We currently do not support using template parameters in template arguments
    check_types("template<typename T> T g(T v) { return v + 1; } template<typename T> T f(T v) { return g<T>(v); } void main() { f<float>(0.0); }");

    // Check that we do not fail type checking due to function contents
    // We currently do not try to process these
    check_types("template<typename T> T f(T v) { return v + 1; }");
    // Even when we actually invoke the function
    // This does not generate a complete ir
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
    check_types("Buffer<uint4> buf; void main() { buf.Load(0); }");

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
}

#[test]
fn check_variable_scope() {
    check_fail("int a; int a;");
    check_fail("void f() { int a; int a; }");
}

#[test]
fn check_object_type_arguments() {
    check_types("Buffer buf;");
    check_types("Buffer<uint4> buf;");
    check_types("RWBuffer buf;");
    check_types("RWBuffer<uint4> buf;");

    check_types("ByteAddressBuffer buf;");
    check_types("RWByteAddressBuffer buf;");

    check_types("StructuredBuffer<uint4> buf;");
    check_types("struct S {}; StructuredBuffer<S> buf;");
    check_types("RWStructuredBuffer<uint4> buf;");
    check_types("struct S {}; RWStructuredBuffer<S> buf;");

    check_types("Texture2D tex;");
    check_types("Texture2D<uint4> tex;");
    check_types("RWTexture2D tex;");
    check_types("RWTexture2D<uint4> tex;");

    check_types("ConstantBuffer<uint4> buf;");
    check_types("struct S {}; ConstantBuffer<S> buf;");
}

#[test]
fn check_texture_index() {
    check_types("RWTexture2D tex; void main() { tex[uint2(0, 0)] = float4(1, 2, 3, 4); }");
    check_types(
        "RWTexture2D tex; void sub(out float4 v) {} void main() { sub(tex[uint2(0, 0)]); }",
    );
    check_types("Texture2D tex; void main() { float x = tex[uint2(0, 0)]; }");
    check_fail("Texture2D tex; void main() { tex[uint2(0, 0)] = float4(1, 2, 3, 4); }");
    check_fail("Texture2D tex; void sub(out float4 v) {} void main() { sub(tex[uint2(0, 0)]); }");
}

#[test]
fn check_buffer_index() {
    check_types("RWBuffer<float> buf; void main() { buf[0] = 3; }");
    check_types("RWBuffer<float> buf; void sub(out float v) {} void main() { sub(buf[0]); }");
    check_types("Buffer<float> buf; void main() { float v = buf[0]; }");
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
fn check_constructors() {
    check_types("int x = int(7);");
    check_types("int x = (int)(7);");

    //  Fails as we expect a single argument
    check_fail("int x = int(7, 6);");
}

#[test]
fn check_fake_constructor() {
    // These are valid but are actually a casted comma expression and not a constructor
    check_types("int x = (int)(7, 6);");
    check_types("int2 x = (int2)(7, 6);");
}

#[test]
fn check_cast() {
    check_types("ByteAddressBuffer buf; void f() { uint x = buf.Load(0); }");
    check_types("ByteAddressBuffer buf; void f() { uint x = (uint)buf.Load(0); }");
    check_types("ByteAddressBuffer buf; void f() { uint x = (uint)buf.Load<uint>(0); }");
    check_types("struct S {}; ByteAddressBuffer buf; void f() { S x = (S)buf.Load<S>(0); }");
    check_types("struct S {}; ByteAddressBuffer buf; void f() { S x = buf.Load<S>(0); }");
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

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
}

#[test]
fn check_function_templates() {
    check_types("template<typename T> void f() {}");
    check_types("template<typename T> void f(T v) {}");
    check_types("template<typename T> T f(T v) { return v; }");
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

    // "int x = (int)(7, 6);" is valid but is actually a comma expression
    // This currently fails as the comma expression is not implemented
}

#[test]
fn check_cast() {
    check_types("ByteAddressBuffer buf; void f() { uint x = buf.Load(0); }");
    check_types("ByteAddressBuffer buf; void f() { uint x = (uint)buf.Load(0); }");
    check_types("ByteAddressBuffer buf; void f() { uint x = (uint)buf.Load<uint>(0); }");
    check_types("struct S {}; ByteAddressBuffer buf; void f() { S x = (S)buf.Load<S>(0); }");
}

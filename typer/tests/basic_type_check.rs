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
fn check_function_calls() {
    check_types("void subroutine() {} void main() { subroutine(); }");
    check_fail("void main() { subroutine(); }");
    check_types("void f1() {} void f2(float x) {} void main() { f1(); f2(0); }");
}

#[test]
fn check_structs() {
    check_types("struct S {}; S value;");
    check_fail("struct S {}; S value = 0;");
    check_types("struct S {}; void main() { S value; }");
    check_fail("struct S {}; void main() { S value = 0; }");
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
}

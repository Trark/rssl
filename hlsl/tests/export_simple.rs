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

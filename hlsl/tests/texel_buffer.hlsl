extern const Buffer<float4> g_input : register(t0);
extern const RWBuffer<float4> g_output : register(u1);

void test() {
    const float4 s1 = g_input.Load(0);
    const float4 s2 = g_output.Load(0);
    g_output[2u] = g_output[1u] = (float4)g_input[1u];
}

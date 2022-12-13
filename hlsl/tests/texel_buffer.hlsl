Buffer<float4> g_input : register(t0);
RWBuffer<float4> g_output : register(u1);

void test() {
    uint outInt;
    const float4 s1 = g_input.Load(0);
    const float4 s2 = g_output.Load(0);
    const float4 s3 = g_input.Load(0, outInt);
    const float4 s4 = g_output.Load(0, outInt);
    g_input.GetDimensions(outInt);
    g_output.GetDimensions(outInt);
    g_output[2u] = g_output[1u] = (float4)g_input[1u];
}

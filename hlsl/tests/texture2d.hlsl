extern const Texture2D<float4> g_input : register(t0);
extern const RWTexture2D<float4> g_output : register(u1);
extern const SamplerState g_sampler : register(s2);

void test() {
    const float4 s1 = g_input.Sample((SamplerState)g_sampler, float2(0.0, 0.0));
    const float4 s2 = g_input.Load(int3(0, 0, 0));
    const float4 s3 = g_output.Load(int2(0, 0));
    g_output[uint2(2u, 2u)] = g_output[uint2(1u, 1u)] = (float4)g_input[uint2(1u, 1u)];
}

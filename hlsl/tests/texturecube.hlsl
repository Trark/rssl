TextureCube<float4> g_input : register(t0);
TextureCubeArray<float4> g_array : register(t1);
SamplerState g_sampler : register(s2);

void test() {
    uint outInt;
    const float4 sample_base = g_input.Sample((SamplerState)g_sampler, float3(0.0, 0.0, 0.0));
    const float4 sample_clamp = g_input.Sample((SamplerState)g_sampler, float3(0.0, 0.0, 0.0), 0.0);
    const float4 sample_status = g_input.Sample((SamplerState)g_sampler, float3(0.0, 0.0, 0.0), 0.0, outInt);
    const float4 sample_array_base = g_array.Sample((SamplerState)g_sampler, float4(0.0, 0.0, 0.0, 0.0));
    const float4 sample_array_clamp = g_array.Sample((SamplerState)g_sampler, float4(0.0, 0.0, 0.0, 0.0), 0.0);
    const float4 sample_array_status = g_array.Sample((SamplerState)g_sampler, float4(0.0, 0.0, 0.0, 0.0), 0.0, outInt);
}

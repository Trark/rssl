TextureCube<float4> g_input : register(t0);
TextureCubeArray<float4> g_array : register(t1);
SamplerState g_sampler : register(s2);

void test() {
    uint outInt;
    const float4 sample_base = g_input.Sample((SamplerState)g_sampler, float3(0.0f, 0.0f, 0.0f));
    const float4 sample_clamp = g_input.Sample((SamplerState)g_sampler, float3(0.0f, 0.0f, 0.0f), 0.0f);
    const float4 sample_status = g_input.Sample((SamplerState)g_sampler, float3(0.0f, 0.0f, 0.0f), 0.0f, outInt);
    const float4 sample_level_base = g_input.SampleLevel((SamplerState)g_sampler, float3(0.0f, 0.0f, 0.0f), 1.0f);
    const float4 sample_level_status = g_input.SampleLevel((SamplerState)g_sampler, float3(0.0f, 0.0f, 0.0f), 1.0f, outInt);
    const float4 sample_array_base = g_array.Sample((SamplerState)g_sampler, float4(0.0f, 0.0f, 0.0f, 0.0f));
    const float4 sample_array_clamp = g_array.Sample((SamplerState)g_sampler, float4(0.0f, 0.0f, 0.0f, 0.0f), 0.0f);
    const float4 sample_array_status = g_array.Sample((SamplerState)g_sampler, float4(0.0f, 0.0f, 0.0f, 0.0f), 0.0f, outInt);
    const float4 sample_array_level_base = g_array.SampleLevel((SamplerState)g_sampler, float4(0.0f, 0.0f, 0.0f, 0.0f), 1.0f);
    const float4 sample_array_level_status = g_array.SampleLevel((SamplerState)g_sampler, float4(0.0f, 0.0f, 0.0f, 0.0f), 1.0f, outInt);
}

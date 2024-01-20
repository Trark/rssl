Texture3D<float4> g_input : register(t0);
RWTexture3D<float4> g_output : register(u1);
SamplerState g_sampler : register(s2);

void test() {
    uint outInt;
    g_input.GetDimensions(outInt, outInt, outInt);
    g_input.GetDimensions(0u, outInt, outInt, outInt, outInt);
    g_output.GetDimensions(outInt, outInt, outInt);
    const float4 load_srv = g_input.Load(int4(0, 0, 0, 0));
    const float4 load_srv_offset = g_input.Load(int4(0, 0, 0, 0), int3(0, 0, 0));
    const float4 load_srv_status = g_input.Load(int4(0, 0, 0, 0), int3(0, 0, 0), outInt);
    const float4 sample_base = g_input.Sample((SamplerState)g_sampler, float3(0.0f, 0.0f, 0.0f));
    const float4 sample_offset = g_input.Sample((SamplerState)g_sampler, float3(0.0f, 0.0f, 0.0f), int3(0, 0, 0));
    const float4 sample_clamp = g_input.Sample((SamplerState)g_sampler, float3(0.0f, 0.0f, 0.0f), int3(0, 0, 0), 0.0f);
    const float4 sample_status = g_input.Sample((SamplerState)g_sampler, float3(0.0f, 0.0f, 0.0f), int3(0, 0, 0), 0.0f, outInt);
    const float4 sample_level_base = g_input.SampleLevel((SamplerState)g_sampler, float3(0.0f, 0.0f, 0.0f), 1.0f);
    const float4 sample_level_offset = g_input.SampleLevel((SamplerState)g_sampler, float3(0.0f, 0.0f, 0.0f), 1.0f, int3(0, 0, 0));
    const float4 sample_level_status = g_input.SampleLevel((SamplerState)g_sampler, float3(0.0f, 0.0f, 0.0f), 1.0f, int3(0, 0, 0), outInt);
    const float4 sample_bias_base = g_input.SampleBias((SamplerState)g_sampler, float3(0.0f, 0.0f, 0.0f), 0.0f);
    const float4 sample_bias_offset = g_input.SampleBias((SamplerState)g_sampler, float3(0.0f, 0.0f, 0.0f), 0.0f, int3(0, 0, 0));
    const float4 sample_bias_clamp = g_input.SampleBias((SamplerState)g_sampler, float3(0.0f, 0.0f, 0.0f), 0.0f, int3(0, 0, 0), 0.0f);
    const float4 sample_bias_status = g_input.SampleBias((SamplerState)g_sampler, float3(0.0f, 0.0f, 0.0f), 0.0f, int3(0, 0, 0), 0.0f, outInt);
    const float4 load_uav = g_output.Load(int3(0, 0, 0));
    const float4 load_uav_status = g_output.Load(int3(0, 0, 0), outInt);
    g_output[uint3(2u, 2u, 2u)] = g_output[uint3(1u, 1u, 1u)] = (float4)g_input[uint3(1u, 1u, 1u)];
    float4 mips_load = (float4)g_input.mips[0u][uint3(0u, 0u, 0u)];
}

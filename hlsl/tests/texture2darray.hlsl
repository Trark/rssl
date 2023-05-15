Texture2DArray<float4> g_input : register(t0);
RWTexture2DArray<float4> g_output : register(u1);
SamplerState g_sampler : register(s2);
SamplerComparisonState g_comparisonSampler : register(s3);

void test() {
    uint outInt;
    g_input.GetDimensions(0u, outInt, outInt, outInt, outInt);
    g_output.GetDimensions(outInt, outInt, outInt);
    const float4 load_srv = g_input.Load(int4(0, 0, 0, 0));
    const float4 load_srv_offset = g_input.Load(int4(0, 0, 0, 0), int2(0, 0));
    const float4 load_srv_status = g_input.Load(int4(0, 0, 0, 0), int2(0, 0), outInt);
    const float4 sample_base = g_input.Sample((SamplerState)g_sampler, float3(0.0, 0.0, 0.0));
    const float4 sample_offset = g_input.Sample((SamplerState)g_sampler, float3(0.0, 0.0, 0.0), int2(0, 0));
    const float4 sample_clamp = g_input.Sample((SamplerState)g_sampler, float3(0.0, 0.0, 0.0), int2(0, 0), 0.0);
    const float4 sample_status = g_input.Sample((SamplerState)g_sampler, float3(0.0, 0.0, 0.0), int2(0, 0), 0.0, outInt);
    const float4 sample_bias_base = g_input.SampleBias((SamplerState)g_sampler, float3(0.0, 0.0, 0.0), 0.0);
    const float4 sample_bias_offset = g_input.SampleBias((SamplerState)g_sampler, float3(0.0, 0.0, 0.0), 0.0, int2(0, 0));
    const float4 sample_bias_clamp = g_input.SampleBias((SamplerState)g_sampler, float3(0.0, 0.0, 0.0), 0.0, int2(0, 0), 0.0);
    const float4 sample_bias_status = g_input.SampleBias((SamplerState)g_sampler, float3(0.0, 0.0, 0.0), 0.0, int2(0, 0), 0.0, outInt);
    const float4 sample_cmp = g_input.SampleCmp((SamplerComparisonState)g_comparisonSampler, float3(0.0, 0.0, 0.0), 0.0, int2(0, 0));
    const float4 sample_cmp_clamp = g_input.SampleCmp((SamplerComparisonState)g_comparisonSampler, float3(0.0, 0.0, 0.0), 0.0, int2(0, 0), 0.0);
    const float4 sample_cmp_status = g_input.SampleCmp((SamplerComparisonState)g_comparisonSampler, float3(0.0, 0.0, 0.0), 0.0, int2(0, 0), 0.0, outInt);
    const float4 sample_cmp_zero = g_input.SampleCmpLevelZero((SamplerComparisonState)g_comparisonSampler, float3(0.0, 0.0, 0.0), 0.0, int2(0, 0));
    const float4 sample_cmp_zero_status = g_input.SampleCmpLevelZero((SamplerComparisonState)g_comparisonSampler, float3(0.0, 0.0, 0.0), 0.0, int2(0, 0), outInt);
    const float4 gather = g_input.GatherRed((SamplerState)g_sampler, float3(0.0, 0.0, 0.0), int2(0, 0));
    const float4 gather_status = g_input.GatherRed((SamplerState)g_sampler, float3(0.0, 0.0, 0.0), int2(0, 0), outInt);
    const float4 gather_red = g_input.GatherRed((SamplerState)g_sampler, float3(0.0, 0.0, 0.0), int2(0, 0));
    const float4 gather_red_status = g_input.GatherRed((SamplerState)g_sampler, float3(0.0, 0.0, 0.0), int2(0, 0), outInt);
    const float4 load_uav = g_output.Load(int3(0, 0, 0));
    const float4 load_uav_status = g_output.Load(int3(0, 0, 0), outInt);
    g_output[uint3(2u, 2u, 2u)] = g_output[uint3(1u, 1u, 1u)] = (float4)g_input[uint3(1u, 1u, 1u)];
    float4 mips_load = (float4)g_input.mips[0u][uint3(0u, 0u, 0u)];
}

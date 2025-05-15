constant metal::sampler g_linearSampler = metal::sampler(metal::s_address::clamp_to_edge, metal::t_address::clamp_to_edge, metal::r_address::clamp_to_edge, metal::min_filter::linear, metal::mag_filter::linear, metal::mip_filter::linear, metal::compare_func::never, metal::max_anisotropy(1u), metal::lod_clamp(0.0f, FLT_MAX));

struct VertexAttributes
{
    [[user(WETNESS)]] float wetness;
    [[user(MATERIAL)]] uint material;
};

void VSMAIN(uint vid, uint iid, thread float4& o_pos, thread float2& o_texcoord, thread VertexAttributes& o_vertex, const metal::texture2d<float> g_input, thread int& s_value) {
    g_input;
    o_pos = float4(0.0f, 0.0f, 0.0f, 0.0f);
    o_texcoord = float2(0.5f, 0.5f);
    o_vertex.wetness = 0.0f;
    o_vertex.material = 0u;
    s_value = 1;
}

float4 PSMAIN(uint pid, float2 i_texcoord, thread float4& o_target0, const metal::texture2d<float> g_input, const metal::texture2d<float, metal::access::read_write> g_output) {
    g_input;
    g_output;
    g_linearSampler;
    o_target0 = float4(i_texcoord.xy, 0.0f, 0.0f);
    return float4(0.0f, 0.0f, 0.0f, 0.0f);
}

struct ArgumentBuffer0
{
    [[id(0)]] const metal::texture2d<float> g_input;
    [[id(1)]] const metal::texture3d<float> g_unused;
};

struct ArgumentBuffer1
{
    [[id(0)]] const metal::texture2d<float, metal::access::read_write> g_output;
};

struct VertexOutput
{
    float4 o_pos [[position]];
    float2 o_texcoord [[user(TEXCOORD)]];
    VertexAttributes o_vertex;
};

[[vertex]]
VertexOutput VertexShaderEntry(uint vid [[vertex_id]], uint iid [[instance_id]], constant ArgumentBuffer0& set0 [[buffer(0)]], constant ArgumentBuffer1& set1 [[buffer(1)]]) {
    int s_value = 0;
    VertexOutput out;
    VSMAIN(vid, iid, out.o_pos, out.o_texcoord, out.o_vertex, set0.g_input, s_value);
    return out;
}

struct PixelOutput
{
    float4 out [[color(1)]];
    float4 o_target0 [[color(0)]];
};

[[fragment]]
PixelOutput PixelShaderEntry(uint pid [[primitive_id]], VertexOutput in [[stage_in]], constant ArgumentBuffer0& set0 [[buffer(0)]], constant ArgumentBuffer1& set1 [[buffer(1)]]) {
    PixelOutput out;
    out.out = PSMAIN(pid, in.o_texcoord, out.o_target0, set0.g_input, set1.g_output);
    return out;
}

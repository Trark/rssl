void VSMAIN(uint vid, uint iid, thread float4& o_pos, const metal::texture2d<float> g_input, thread int& s_value) {
    g_input;
    o_pos = float4(0.0f, 0.0f, 0.0f, 0.0f);
    s_value = 1;
}

float4 PSMAIN(uint pid, thread float4& o_target0, const metal::texture2d<float> g_input, const metal::texture2d<float, metal::access::read_write> g_output) {
    g_input;
    g_output;
    o_target0 = float4(0.0f, 0.0f, 0.0f, 0.0f);
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
};

[[vertex]]
VertexOutput VertexShaderEntry(uint vid [[vertex_id]], uint iid [[instance_id]], constant ArgumentBuffer0& set0 [[buffer(0)]], constant ArgumentBuffer1& set1 [[buffer(1)]]) {
    int s_value = 0;
    VertexOutput out;
    VSMAIN(vid, iid, out.o_pos, set0.g_input, s_value);
    return out;
}

struct PixelOutput
{
    float4 out [[color(1)]];
    float4 o_target0 [[color(0)]];
};

[[fragment]]
PixelOutput PixelShaderEntry(uint pid [[primitive_id]], constant ArgumentBuffer0& set0 [[buffer(0)]], constant ArgumentBuffer1& set1 [[buffer(1)]]) {
    PixelOutput out;
    out.out = PSMAIN(pid, out.o_target0, set0.g_input, set1.g_output);
    return out;
}

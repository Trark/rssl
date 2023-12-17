void VSMAIN(uint vid, uint iid, const metal::texture2d<float> g_input) {
    g_input;
}

void PSMAIN(uint pid, const metal::texture2d<float> g_input, const metal::texture2d<float, metal::access::read_write> g_output) {
    g_input;
    g_output;
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

[[vertex]]
void VertexShaderEntry(uint vid [[vertex_id]], uint iid [[instance_id]], constant ArgumentBuffer0& set0 [[buffer(0)]], constant ArgumentBuffer1& set1 [[buffer(1)]]) {
    VSMAIN(vid, iid, set0.g_input);
}

[[fragment]]
void PixelShaderEntry(uint pid [[primitive_id]], constant ArgumentBuffer0& set0 [[buffer(0)]], constant ArgumentBuffer1& set1 [[buffer(1)]]) {
    PSMAIN(pid, set0.g_input, set1.g_output);
}

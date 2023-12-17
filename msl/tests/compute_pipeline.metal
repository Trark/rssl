void CSMAIN(uint3 dtid, const metal::texture2d_array<float> g_input, const metal::texture3d<float, metal::access::read_write> g_output) {
    g_input;
    g_output;
}

struct ArgumentBuffer0
{
    [[id(0)]] const metal::texture2d_array<float> g_input;
    [[id(1)]] const metal::texture2d<float> g_unused;
};

struct ArgumentBuffer1
{
    [[id(0)]] const metal::texture3d<float, metal::access::read_write> g_output;
};

[[kernel]]
void ComputeShaderEntry(uint3 dtid [[thread_position_in_grid]], constant ArgumentBuffer0& set0 [[buffer(0)]], constant ArgumentBuffer1& set1 [[buffer(1)]]) {
    CSMAIN(dtid, set0.g_input, set1.g_output);
}

void CSMAIN(metal::array<const metal::texture2d<float>, 1024> g_bindlessTexture2d) {
    metal::texture2d<float> tex = (metal::texture2d<float>)g_bindlessTexture2d[0u];
}

struct ArgumentBuffer0
{
};

struct ArgumentBuffer1
{
    [[id(0)]] metal::array<const metal::texture2d<float>, 1024> g_bindlessTexture2d;
};

struct ArgumentBuffer2
{
    [[id(0)]] const metal::sampler g_linearSampler;
};

[[kernel]]
void ComputeShaderEntry(constant ArgumentBuffer0& set0 [[buffer(0)]], constant ArgumentBuffer1& set1 [[buffer(1)]], constant ArgumentBuffer2& set2 [[buffer(2)]]) {
    CSMAIN(set1.g_bindlessTexture2d);
}

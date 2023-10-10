Texture2D<float4> g_bindlessTexture2d[1024] : register(t0, space1);
SamplerState g_linearSampler : register(s0, space2);

[numthreads(8, 8, 1)]
void CSMAIN() {
    Texture2D<float4> tex = (Texture2D<float4>)g_bindlessTexture2d[0u];
}

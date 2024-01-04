[[vk::binding(0, 1)]] Texture2D<float4> g_bindlessTexture2d[1024];
[[vk::binding(1024, 1)]] ByteAddressBuffer g_bindlessByteBuffer[1024];
[[vk::binding(0, 2)]] SamplerState g_linearSampler;

[numthreads(8, 8, 1)]
void CSMAIN() {
    Texture2D<float4> tex = (Texture2D<float4>)g_bindlessTexture2d[0u];
    tex.Load((int3)0u);
    g_bindlessTexture2d[1u].Load((int3)0u);
    ByteAddressBuffer buf = (ByteAddressBuffer)g_bindlessByteBuffer[0u];
    g_bindlessByteBuffer[2u].Load(0u);
}

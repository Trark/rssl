Texture2DArray<float4> g_input : register(t0);
Texture2D<float4> g_unused : register(t1);
RWTexture3D<float4> g_output : register(u0, space1);
static uint s_unused;
static uint4 s_localData[4096];
groupshared float4 s_sharedData[1024];

void CSMAIN(uint3 dtid : SV_DispatchThreadID) {
    g_input;
    g_output;
    s_localData[0u] = uint4(1u, 2u, 3u, 4u);
    s_sharedData[dtid.x] = float4(1.0f, 2.0f, 3.0f, 4.0f);
}

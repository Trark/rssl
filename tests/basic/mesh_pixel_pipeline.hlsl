Texture2D<float4> g_input : register(t0);
Texture3D<float4> g_unused : register(t1);
RWTexture2D<float4> g_output : register(u0, space1);
static int s_value = 0;

struct VertexAttributes
{
    float4 position : SV_Position;
    float2 texcoord : TEXCOORD;
};

struct PrimitiveAttributes
{
    uint material : MATERIAL;
};

[numthreads(64, 1, 1)]
[outputtopology("triangle")]
void MSMAIN(uint3 dtid : SV_DispatchThreadID, out vertices VertexAttributes o_vertices[128], out primitives PrimitiveAttributes o_primitives[64], out indices uint3 o_triangles[64]) {
    g_input;
    s_value = 1;
    SetMeshOutputCounts(128u, 64u);
    for (uint v = dtid.x; v < 128u; v += 64u)
    {
        VertexAttributes vertex;
        vertex.position = float4(0.0f, 0.0f, 0.0f, 1.0f);
        vertex.texcoord = float2(0.0f, 0.0f);
        o_vertices[v] = vertex;
    }
    PrimitiveAttributes prim;
    prim.material = dtid.x % 8u;
    o_primitives[dtid.x] = prim;
    o_triangles[dtid.x] = uint3(0u, 1u, 2u);
}

float4 PSMAIN(uint pid : SV_PrimitiveID, float2 i_texcoord : TEXCOORD, uint i_material : MATERIAL, out float4 o_target0 : SV_Target0) : SV_Target1 {
    g_input;
    g_output;
    o_target0 = float4(i_texcoord.xy, (float)i_material, 0.0f);
    return float4(0.0f, 0.0f, 0.0f, 0.0f);
}

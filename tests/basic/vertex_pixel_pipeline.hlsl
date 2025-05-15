Texture2D<float4> g_input : register(t0);
Texture3D<float4> g_unused : register(t1);
SamplerState g_linearSampler : register(s2);
RWTexture2D<float4> g_output : register(u0, space1);
static int s_value = 0;

struct VertexAttributes
{
    float wetness : WETNESS;
    uint material : MATERIAL;
};

void VSMAIN(uint vid : SV_VertexID, uint iid : SV_InstanceID, out float4 o_pos : SV_Position, out float2 o_texcoord : TEXCOORD, out VertexAttributes o_vertex) {
    g_input;
    o_pos = float4(0.0f, 0.0f, 0.0f, 0.0f);
    o_texcoord = float2(0.5f, 0.5f);
    o_vertex.wetness = 0.0f;
    o_vertex.material = 0u;
    s_value = 1;
}

float4 PSMAIN(uint pid : SV_PrimitiveID, float2 i_texcoord : TEXCOORD, out float4 o_target0 : SV_Target0) : SV_Target1 {
    g_input;
    g_output;
    g_linearSampler;
    o_target0 = float4(i_texcoord.xy, 0.0f, 0.0f);
    return float4(0.0f, 0.0f, 0.0f, 0.0f);
}

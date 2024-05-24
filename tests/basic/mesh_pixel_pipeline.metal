namespace helper {

template<typename Mesh>
void set_indices(Mesh mesh, uint index, uint3 indices) {
    mesh.set_index(3u * index + 0u, indices.x);
    mesh.set_index(3u * index + 1u, indices.y);
    mesh.set_index(3u * index + 2u, indices.z);
}

} // namespace helper

struct VertexAttributes
{
    [[position]] float4 position;
    [[user(TEXCOORD)]] float2 texcoord;
};

struct PrimitiveAttributes
{
    [[user(MATERIAL)]] uint material;
};

void MSMAIN(uint3 dtid, metal::mesh<VertexAttributes, PrimitiveAttributes, 128u, 64u, metal::topology::triangle> o_mesh, const metal::texture2d<float> g_input, thread int& s_value) {
    g_input;
    s_value = 1;
    128u, o_mesh.set_primitive_count(64u);
    for (uint v = dtid.x; v < 128u; v += 64u)
    {
        VertexAttributes vertex_0;
        vertex_0.position = float4(0.0f, 0.0f, 0.0f, 1.0f);
        vertex_0.texcoord = float2(0.0f, 0.0f);
        o_mesh.set_vertex(v, vertex_0);
    }
    PrimitiveAttributes prim;
    prim.material = dtid.x % 8u;
    o_mesh.set_primitive(dtid.x, prim);
    helper::set_indices(o_mesh, dtid.x, uint3(0u, 1u, 2u));
}

float4 PSMAIN(uint pid, float2 i_texcoord, uint i_material, thread float4& o_target0, const metal::texture2d<float> g_input, const metal::texture2d<float, metal::access::read_write> g_output) {
    g_input;
    g_output;
    o_target0 = float4(i_texcoord.xy, (float)i_material, 0.0f);
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

[[mesh]]
void MeshShaderEntry(uint3 dtid [[thread_position_in_grid]], constant ArgumentBuffer0& set0 [[buffer(0)]], constant ArgumentBuffer1& set1 [[buffer(1)]], metal::mesh<VertexAttributes, PrimitiveAttributes, 128u, 64u, metal::topology::triangle> o_mesh) {
    int s_value = 0;
    MSMAIN(dtid, o_mesh, set0.g_input, s_value);
}

struct PixelInput
{
    VertexAttributes o_vertices;
    PrimitiveAttributes o_primitives;
};

struct PixelOutput
{
    float4 out [[color(1)]];
    float4 o_target0 [[color(0)]];
};

[[fragment]]
PixelOutput PixelShaderEntry(uint pid [[primitive_id]], PixelInput in [[stage_in]], constant ArgumentBuffer0& set0 [[buffer(0)]], constant ArgumentBuffer1& set1 [[buffer(1)]]) {
    PixelOutput out;
    out.out = PSMAIN(pid, in.o_vertices.texcoord, in.o_primitives.material, out.o_target0, set0.g_input, set1.g_output);
    return out;
}

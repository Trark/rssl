struct Payload
{
    uint start_location;
};

struct VertexAttributes
{
    float4 position : SV_Position;
};

[numthreads(64, 1, 1)]
void TaskEntry(uint3 dtid : SV_DispatchThreadID) {
    Payload data;
    data.start_location = dtid.x;
    DispatchMesh(4u, 1u, 1u, data);
}

[numthreads(64, 1, 1)]
[outputtopology("triangle")]
void MeshEntry(uint3 dtid : SV_DispatchThreadID, in payload Payload data, out vertices VertexAttributes o_vertices[64], out indices uint3 o_triangles[64]) {
    SetMeshOutputCounts(64u, 64u);
    VertexAttributes vertex;
    vertex.position = float4((float)data.start_location, 0.0f, 0.0f, 1.0f);
    o_vertices[dtid.x] = vertex;
    o_triangles[dtid.x] = uint3(0u, 1u, 2u);
}

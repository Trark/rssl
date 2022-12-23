struct MeshPayload
{
};

struct MeshVertex
{
};

struct MeshPrimitive
{
};

[outputtopology("triangle")]
[numthreads(124, 1, 1)]
void MeshShader(uint thread_id : SV_DispatchThreadID, uint group_index : SV_GroupIndex, in payload MeshPayload payloadData, out vertices MeshVertex verts[124], out primitives MeshPrimitive prims[64], out indices uint3 triangles[64]) {
    SetMeshOutputCounts(124u, 64u);
}

namespace helper {

template<typename Mesh>
void set_indices(Mesh mesh, uint index, uint3 indices) {
    mesh.set_index(3u * index + 0u, indices.x);
    mesh.set_index(3u * index + 1u, indices.y);
    mesh.set_index(3u * index + 2u, indices.z);
}

} // namespace helper

struct Payload
{
    uint start_location;
};

struct VertexAttributes
{
    [[position]] float4 position;
};

void TaskEntry(uint3 dtid, object_data Payload& o_payload, metal::mesh_grid_properties mesh_grid_properties) {
    Payload data;
    data.start_location = dtid.x;
    mesh_grid_properties.set_threadgroups_per_grid(uint3(4u, 1u, 1u)), o_payload = data;
}

void MeshEntry(uint3 dtid, Payload data, metal::mesh<VertexAttributes, void, 64u, 64u, metal::topology::triangle> o_mesh) {
    64u, o_mesh.set_primitive_count(64u);
    VertexAttributes vertex_0;
    vertex_0.position = float4((float)data.start_location, 0.0f, 0.0f, 1.0f);
    o_mesh.set_vertex(dtid.x, vertex_0);
    helper::set_indices(o_mesh, dtid.x, uint3(0u, 1u, 2u));
}

[[object]]
[[max_total_threads_per_threadgroup(64 * 1 * 1)]]
void TaskShaderEntry(uint3 dtid [[thread_position_in_grid]], object_data Payload& o_payload [[payload]], metal::mesh_grid_properties mesh_grid_properties) {
    TaskEntry(dtid, o_payload, mesh_grid_properties);
}

[[mesh]]
[[max_total_threads_per_threadgroup(64 * 1 * 1)]]
void MeshShaderEntry(uint3 dtid [[thread_position_in_grid]], object_data const Payload& data [[payload]], metal::mesh<VertexAttributes, void, 64u, 64u, metal::topology::triangle> o_mesh) {
    MeshEntry(dtid, data, o_mesh);
}

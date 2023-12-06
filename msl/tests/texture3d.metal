namespace helper {

template<typename T>
metal::vec<T, 4> Load(metal::texture3d<T> texture, int4 location) {
    return texture.read(uint3(location.x, location.y, location.z), location.w);
}

template<typename T>
metal::vec<T, 4> Load(metal::texture3d<T> texture, int4 location, int3 offset) {
    return texture.read(uint3(location.x + offset.x, location.y + offset.y, location.z + offset.z), location.w);
}

template<typename T>
metal::vec<T, 4> Load(metal::texture3d<T> texture, int4 location, int3 offset, thread uint &status) {
    metal::sparse_color<metal::vec<T, 4>> color = texture.sparse_read(uint3(location.x + offset.x, location.y + offset.y, location.z + offset.z), location.w);
    status = color.resident();
    return color.value();
}

template<typename T>
metal::vec<T, 4> Load(metal::texture3d<T, metal::access::read_write> texture, int3 location) {
    return texture.read(uint3(location.x, location.y, location.z));
}

template<typename T>
metal::vec<T, 4> Load(metal::texture3d<T, metal::access::read_write> texture, int3 location, thread uint &status) {
    metal::sparse_color<metal::vec<T, 4>> color = texture.sparse_read(uint3(location.x, location.y, location.z));
    status = color.resident();
    return color.value();
}

template<typename T>
metal::vec<T, 4> Sample(metal::texture3d<T> texture, metal::sampler s, float3 coord) {
    return texture.sample(s, float3(coord.x, coord.y, coord.z));
}

template<typename T>
metal::vec<T, 4> Sample(metal::texture3d<T> texture, metal::sampler s, float3 coord, int3 offset) {
    return texture.sample(s, float3(coord.x, coord.y, coord.z), offset);
}

template<typename T>
metal::vec<T, 4> Sample(metal::texture3d<T> texture, metal::sampler s, float3 coord, int3 offset, float clamp) {
    return texture.sample(s, float3(coord.x, coord.y, coord.z), metal::min_lod_clamp(clamp), offset);
}

template<typename T>
metal::vec<T, 4> Sample(metal::texture3d<T> texture, metal::sampler s, float3 coord, int3 offset, float clamp, thread uint &status) {
    metal::sparse_color<metal::vec<T, 4>> color = texture.sparse_sample(s, float3(coord.x, coord.y, coord.z), metal::min_lod_clamp(clamp), offset);
    status = color.resident();
    return color.value();
}

} // namespace helper

void test(const metal::texture3d<float> g_input, const metal::texture3d<float, metal::access::read_write> g_output, const metal::sampler g_sampler) {
    uint outInt;
    const float4 load_srv = helper::Load(g_input, int4(0, 0, 0, 0));
    const float4 load_srv_offset = helper::Load(g_input, int4(0, 0, 0, 0), int3(0, 0, 0));
    const float4 load_srv_status = helper::Load(g_input, int4(0, 0, 0, 0), int3(0, 0, 0), outInt);
    const float4 sample_base = helper::Sample(g_input, (metal::sampler)g_sampler, float3(0.0f, 0.0f, 0.0f));
    const float4 sample_offset = helper::Sample(g_input, (metal::sampler)g_sampler, float3(0.0f, 0.0f, 0.0f), int3(0, 0, 0));
    const float4 sample_clamp = helper::Sample(g_input, (metal::sampler)g_sampler, float3(0.0f, 0.0f, 0.0f), int3(0, 0, 0), 0.0f);
    const float4 sample_status = helper::Sample(g_input, (metal::sampler)g_sampler, float3(0.0f, 0.0f, 0.0f), int3(0, 0, 0), 0.0f, outInt);
    const float4 load_uav = helper::Load(g_output, int3(0, 0, 0));
    const float4 load_uav_status = helper::Load(g_output, int3(0, 0, 0), outInt);
}

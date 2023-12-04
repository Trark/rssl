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

} // namespace helper

void test(const metal::texture3d<float> g_input, const metal::texture3d<float, metal::access::read_write> g_output) {
    uint outInt;
    const float4 load_srv = helper::Load(g_input, int4(0, 0, 0, 0));
    const float4 load_srv_offset = helper::Load(g_input, int4(0, 0, 0, 0), int3(0, 0, 0));
    const float4 load_srv_status = helper::Load(g_input, int4(0, 0, 0, 0), int3(0, 0, 0), outInt);
    const float4 load_uav = helper::Load(g_output, int3(0, 0, 0));
    const float4 load_uav_status = helper::Load(g_output, int3(0, 0, 0), outInt);
}

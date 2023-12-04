namespace helper {

template<typename T>
metal::vec<T, 4> Load(metal::texture2d<T> texture, int3 location) {
    return texture.read(uint2(location.x, location.y), location.z);
}

template<typename T>
metal::vec<T, 4> Load(metal::texture2d<T> texture, int3 location, int2 offset) {
    return texture.read(uint2(location.x + offset.x, location.y + offset.y), location.z);
}

template<typename T>
metal::vec<T, 4> Load(metal::texture2d<T> texture, int3 location, int2 offset, thread uint &status) {
    metal::sparse_color<metal::vec<T, 4>> color = texture.sparse_read(uint2(location.x + offset.x, location.y + offset.y), location.z);
    status = color.resident();
    return color.value();
}

template<typename T>
metal::vec<T, 4> Load(metal::texture2d<T, metal::access::read_write> texture, int2 location) {
    return texture.read(uint2(location.x, location.y));
}

template<typename T>
metal::vec<T, 4> Load(metal::texture2d<T, metal::access::read_write> texture, int2 location, thread uint &status) {
    metal::sparse_color<metal::vec<T, 4>> color = texture.sparse_read(uint2(location.x, location.y));
    status = color.resident();
    return color.value();
}

} // namespace helper

void test(const metal::texture2d<float> g_input, const metal::texture2d<float, metal::access::read_write> g_output) {
    uint outInt;
    const float4 load_srv = helper::Load(g_input, int3(0, 0, 0));
    const float4 load_srv_offset = helper::Load(g_input, int3(0, 0, 0), int2(0, 0));
    const float4 load_srv_status = helper::Load(g_input, int3(0, 0, 0), int2(0, 0), outInt);
    const float4 load_uav = helper::Load(g_output, int2(0, 0));
    const float4 load_uav_status = helper::Load(g_output, int2(0, 0), outInt);
}

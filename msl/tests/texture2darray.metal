namespace helper {

template<typename T>
metal::vec<T, 4> Load(metal::texture2d_array<T> texture, int4 location) {
    return texture.read(uint2(location.x, location.y), location.z, location.w);
}

template<typename T>
metal::vec<T, 4> Load(metal::texture2d_array<T> texture, int4 location, int2 offset) {
    return texture.read(uint2(location.x + offset.x, location.y + offset.y), location.z, location.w);
}

template<typename T>
metal::vec<T, 4> Load(metal::texture2d_array<T> texture, int4 location, int2 offset, thread uint &status) {
    metal::sparse_color<metal::vec<T, 4>> color = texture.sparse_read(uint2(location.x + offset.x, location.y + offset.y), location.z, location.w);
    status = color.resident();
    return color.value();
}

} // namespace helper

void test(const metal::texture2d_array<float> g_input) {
    uint outInt;
    const float4 load_srv = helper::Load(g_input, int4(0, 0, 0, 0));
    const float4 load_srv_offset = helper::Load(g_input, int4(0, 0, 0, 0), int2(0, 0));
    const float4 load_srv_status = helper::Load(g_input, int4(0, 0, 0, 0), int2(0, 0), outInt);
}

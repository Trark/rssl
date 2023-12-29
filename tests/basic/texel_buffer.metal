namespace helper {

template<typename T>
metal::vec<T, 4> Load(metal::texture_buffer<T> texture, int location) {
    return texture.read(uint(location));
}

template<typename T>
metal::vec<T, 4> Load(metal::texture_buffer<T, metal::access::read_write> texture, int location) {
    return texture.read(uint(location));
}

} // namespace helper

void test(const metal::texture_buffer<uint> g_input, const metal::texture_buffer<uint, metal::access::read_write> g_output) {
    uint outInt;
    const uint load_srv = helper::Load(g_input, 0).x;
    const uint load_uav = helper::Load(g_output, 0).x;
}

struct ArgumentBuffer0
{
    [[id(0)]] const metal::texture_buffer<uint> g_input;
    [[id(1)]] const metal::texture_buffer<uint, metal::access::read_write> g_output;
};

[[kernel]]
void ComputeShaderEntry(constant ArgumentBuffer0& set0 [[buffer(0)]]) {
    test(set0.g_input, set0.g_output);
}

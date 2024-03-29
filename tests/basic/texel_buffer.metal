namespace helper {

template<typename T>
metal::vec<T, 4> Load(metal::texture_buffer<T> texture, int location) {
    return texture.read(uint(location));
}

template<typename T>
metal::vec<T, 4> Load(metal::texture_buffer<T, metal::access::read_write> texture, int location) {
    return texture.read(uint(location));
}

template<typename T>
metal::vec<T, 4> Store(metal::texture_buffer<T, metal::access::read_write> texture, uint location, metal::vec<T, 4> value) {
    texture.write(value, location);
    return value;
}

template<typename T>
metal::vec<T, 4> extend(T value) {
    return metal::vec<T, 4>(value, 0, 0, 0);
}

} // namespace helper

void test(const metal::texture_buffer<uint> g_input, const metal::texture_buffer<uint, metal::access::read_write> g_output) {
    uint outInt;
    const uint load_srv = helper::Load(g_input, 0).x;
    const uint load_uav = helper::Load(g_output, 0).x;
    helper::Store(g_output, 2u, helper::extend(helper::Store(g_output, 1u, helper::extend((uint)helper::Load(g_input, (int)1u).x)).x)).x;
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

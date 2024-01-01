namespace helper {

template<typename T>
void GetDimensions(metal::texture2d_array<T> texture, thread uint& width, thread uint& height, thread uint& elements) {
    width = texture.get_width();
    height = texture.get_height();
    elements = texture.get_array_size();
}

template<typename T>
void GetDimensions(metal::texture2d_array<T> texture, uint mipLevel, thread uint& width, thread uint& height, thread uint& elements, thread uint& numberOfLevels) {
    width = texture.get_width(mipLevel);
    height = texture.get_height(mipLevel);
    elements = texture.get_array_size();
    numberOfLevels = texture.get_num_mip_levels();
}

template<typename T>
void GetDimensions(metal::texture2d_array<T, metal::access::read_write> texture, thread uint& width, thread uint& height, thread uint& elements) {
    width = texture.get_width();
    height = texture.get_height();
    elements = texture.get_array_size();
}

template<typename T>
metal::vec<T, 4> Load(metal::texture2d_array<T> texture, int4 location) {
    return texture.read(uint2(location.x, location.y), location.z, location.w);
}

template<typename T>
metal::vec<T, 4> Load(metal::texture2d_array<T> texture, int4 location, int2 offset) {
    return texture.read(uint2(location.x + offset.x, location.y + offset.y), location.z, location.w);
}

template<typename T>
metal::vec<T, 4> Load(metal::texture2d_array<T> texture, int4 location, int2 offset, thread uint& status) {
    metal::sparse_color<metal::vec<T, 4>> color = texture.sparse_read(uint2(location.x + offset.x, location.y + offset.y), location.z, location.w);
    status = color.resident();
    return color.value();
}

template<typename T>
metal::vec<T, 4> Load(metal::texture2d_array<T, metal::access::read_write> texture, int3 location) {
    return texture.read(uint2(location.x, location.y), location.z);
}

template<typename T>
metal::vec<T, 4> Load(metal::texture2d_array<T, metal::access::read_write> texture, int3 location, thread uint& status) {
    metal::sparse_color<metal::vec<T, 4>> color = texture.sparse_read(uint2(location.x, location.y), location.z);
    status = color.resident();
    return color.value();
}

template<typename T>
metal::vec<T, 4> Store(metal::texture2d_array<T, metal::access::read_write> texture, uint3 location, metal::vec<T, 4> value) {
    texture.write(value, location.xy, location.z);
    return value;
}

template<typename T>
metal::vec<T, 4> Sample(metal::texture2d_array<T> texture, metal::sampler s, float3 coord) {
    return texture.sample(s, coord.xy, uint(coord.z));
}

template<typename T>
metal::vec<T, 4> Sample(metal::texture2d_array<T> texture, metal::sampler s, float3 coord, int2 offset) {
    return texture.sample(s, coord.xy, uint(coord.z), offset);
}

template<typename T>
metal::vec<T, 4> Sample(metal::texture2d_array<T> texture, metal::sampler s, float3 coord, int2 offset, float clamp) {
    return texture.sample(s, coord.xy, uint(coord.z), metal::min_lod_clamp(clamp), offset);
}

template<typename T>
metal::vec<T, 4> Sample(metal::texture2d_array<T> texture, metal::sampler s, float3 coord, int2 offset, float clamp, thread uint& status) {
    metal::sparse_color<metal::vec<T, 4>> color = texture.sparse_sample(s, coord.xy, uint(coord.z), metal::min_lod_clamp(clamp), offset);
    status = color.resident();
    return color.value();
}

template<typename T>
metal::vec<T, 4> extend(metal::vec<T, 3> value) {
    return metal::vec<T, 4>(value, 0);
}

template<size_t N>
metal::vec<int, N + 1> make_signed_push_0(metal::vec<uint, N> coord) {
    return metal::vec<int, N + 1>(metal::vec<int, N>(coord), 0u);
}

} // namespace helper

void test(const metal::texture2d_array<float> g_input, const metal::texture2d_array<float, metal::access::read_write> g_output, const metal::sampler g_sampler) {
    uint outInt;
    helper::GetDimensions(g_input, outInt, outInt, outInt);
    helper::GetDimensions(g_input, 0u, outInt, outInt, outInt, outInt);
    helper::GetDimensions(g_output, outInt, outInt, outInt);
    const float3 load_srv = helper::Load(g_input, int4(0, 0, 0, 0)).xyz;
    const float3 load_srv_offset = helper::Load(g_input, int4(0, 0, 0, 0), int2(0, 0)).xyz;
    const float3 load_srv_status = helper::Load(g_input, int4(0, 0, 0, 0), int2(0, 0), outInt).xyz;
    const float3 sample_base = helper::Sample(g_input, (metal::sampler)g_sampler, float3(0.0f, 0.0f, 0.0f)).xyz;
    const float3 sample_offset = helper::Sample(g_input, (metal::sampler)g_sampler, float3(0.0f, 0.0f, 0.0f), int2(0, 0)).xyz;
    const float3 sample_clamp = helper::Sample(g_input, (metal::sampler)g_sampler, float3(0.0f, 0.0f, 0.0f), int2(0, 0), 0.0f).xyz;
    const float3 sample_status = helper::Sample(g_input, (metal::sampler)g_sampler, float3(0.0f, 0.0f, 0.0f), int2(0, 0), 0.0f, outInt).xyz;
    const float3 load_uav = helper::Load(g_output, int3(0, 0, 0)).xyz;
    const float3 load_uav_status = helper::Load(g_output, int3(0, 0, 0), outInt).xyz;
    helper::Store(g_output, uint3(2u, 2u, 2u), helper::extend(helper::Store(g_output, uint3(1u, 1u, 1u), helper::extend((float3)helper::Load(g_input, helper::make_signed_push_0(uint3(1u, 1u, 1u))).xyz)).xyz)).xyz;
}

struct ArgumentBuffer0
{
    [[id(0)]] const metal::texture2d_array<float> g_input;
    [[id(1)]] const metal::texture2d_array<float, metal::access::read_write> g_output;
    [[id(2)]] const metal::sampler g_sampler;
};

[[kernel]]
void ComputeShaderEntry(constant ArgumentBuffer0& set0 [[buffer(0)]]) {
    test(set0.g_input, set0.g_output, set0.g_sampler);
}

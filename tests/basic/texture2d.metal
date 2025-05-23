namespace helper {

template<typename T>
void GetDimensions(metal::texture2d<T> texture, thread uint& width, thread uint& height) {
    width = texture.get_width();
    height = texture.get_height();
}

template<typename T>
void GetDimensions(metal::texture2d<T> texture, uint mipLevel, thread uint& width, thread uint& height, thread uint& numberOfLevels) {
    width = texture.get_width(mipLevel);
    height = texture.get_height(mipLevel);
    numberOfLevels = texture.get_num_mip_levels();
}

template<typename T>
void GetDimensions(metal::texture2d<T, metal::access::read_write> texture, thread uint& width, thread uint& height) {
    width = texture.get_width();
    height = texture.get_height();
}

template<typename T>
metal::vec<T, 4> Load(metal::texture2d<T> texture, int3 location) {
    return texture.read(uint2(location.x, location.y), location.z);
}

template<typename T>
metal::vec<T, 4> Load(metal::texture2d<T> texture, int3 location, int2 offset) {
    return texture.read(uint2(location.x + offset.x, location.y + offset.y), location.z);
}

template<typename T>
metal::vec<T, 4> Load(metal::texture2d<T> texture, int3 location, int2 offset, thread uint& status) {
    metal::sparse_color<metal::vec<T, 4>> color = texture.sparse_read(uint2(location.x + offset.x, location.y + offset.y), location.z);
    status = color.resident();
    return color.value();
}

template<typename T>
metal::vec<T, 4> Load(metal::texture2d<T, metal::access::read_write> texture, int2 location) {
    return texture.read(uint2(location.x, location.y));
}

template<typename T>
metal::vec<T, 4> Load(metal::texture2d<T, metal::access::read_write> texture, int2 location, thread uint& status) {
    metal::sparse_color<metal::vec<T, 4>> color = texture.sparse_read(uint2(location.x, location.y));
    status = color.resident();
    return color.value();
}

template<typename T>
metal::vec<T, 4> Store(metal::texture2d<T, metal::access::read_write> texture, uint2 location, metal::vec<T, 4> value) {
    texture.write(value, location);
    return value;
}

template<typename T>
metal::vec<T, 4> Sample(metal::texture2d<T> texture, metal::sampler s, float2 coord) {
    return texture.sample(s, coord);
}

template<typename T>
metal::vec<T, 4> Sample(metal::texture2d<T> texture, metal::sampler s, float2 coord, int2 offset) {
    return texture.sample(s, coord, offset);
}

template<typename T>
metal::vec<T, 4> Sample(metal::texture2d<T> texture, metal::sampler s, float2 coord, int2 offset, float clamp) {
    return texture.sample(s, coord, metal::min_lod_clamp(clamp), offset);
}

template<typename T>
metal::vec<T, 4> Sample(metal::texture2d<T> texture, metal::sampler s, float2 coord, int2 offset, float clamp, thread uint& status) {
    metal::sparse_color<metal::vec<T, 4>> color = texture.sparse_sample(s, coord, metal::min_lod_clamp(clamp), offset);
    status = color.resident();
    return color.value();
}

template<typename T>
metal::vec<T, 4> SampleLevel(metal::texture2d<T> texture, metal::sampler s, float2 coord, float lod) {
    return texture.sample(s, coord, metal::level(lod));
}

template<typename T>
metal::vec<T, 4> SampleLevel(metal::texture2d<T> texture, metal::sampler s, float2 coord, float lod, int2 offset) {
    return texture.sample(s, coord, metal::level(lod), offset);
}

template<typename T>
metal::vec<T, 4> SampleLevel(metal::texture2d<T> texture, metal::sampler s, float2 coord, float lod, int2 offset, thread uint& status) {
    metal::sparse_color<metal::vec<T, 4>> color = texture.sparse_sample(s, coord, metal::level(lod), offset);
    status = color.resident();
    return color.value();
}

template<size_t N>
metal::vec<int, N> make_signed(metal::vec<uint, N> coord) {
    return metal::vec<int, N>(coord);
}

template<size_t N>
metal::vec<int, N + 1> make_signed_push_0(metal::vec<uint, N> coord) {
    return metal::vec<int, N + 1>(metal::vec<int, N>(coord), 0u);
}

} // namespace helper

void test(const metal::texture2d<float> g_input, const metal::texture2d<float, metal::access::read_write> g_output, const metal::sampler g_sampler) {
    uint outInt;
    helper::GetDimensions(g_input, outInt, outInt);
    helper::GetDimensions(g_input, 0u, outInt, outInt, outInt);
    helper::GetDimensions(g_output, outInt, outInt);
    const float4 load_srv = helper::Load(g_input, int3(0, 0, 0));
    const float4 load_srv_offset = helper::Load(g_input, int3(0, 0, 0), int2(0, 0));
    const float4 load_srv_status = helper::Load(g_input, int3(0, 0, 0), int2(0, 0), outInt);
    const float4 sample_base = helper::Sample(g_input, (metal::sampler)g_sampler, float2(0.0f, 0.0f));
    const float4 sample_base_3 = helper::Sample(g_input, (metal::sampler)g_sampler, (float2)float3(0.0f, 0.0f, 0.0f).xy);
    const float4 sample_offset = helper::Sample(g_input, (metal::sampler)g_sampler, float2(0.0f, 0.0f), int2(0, 0));
    const float4 sample_clamp = helper::Sample(g_input, (metal::sampler)g_sampler, float2(0.0f, 0.0f), int2(0, 0), 0.0f);
    const float4 sample_status = helper::Sample(g_input, (metal::sampler)g_sampler, float2(0.0f, 0.0f), int2(0, 0), 0.0f, outInt);
    const float4 sample_level_base = helper::SampleLevel(g_input, (metal::sampler)g_sampler, float2(0.0f, 0.0f), 1.0f);
    const float4 sample_level_offset = helper::SampleLevel(g_input, (metal::sampler)g_sampler, float2(0.0f, 0.0f), 1.0f, int2(0, 0));
    const float4 sample_level_status = helper::SampleLevel(g_input, (metal::sampler)g_sampler, float2(0.0f, 0.0f), 1.0f, int2(0, 0), outInt);
    const float4 load_uav = helper::Load(g_output, int2(0, 0));
    const float4 load_uav_status = helper::Load(g_output, int2(0, 0), outInt);
    helper::Store(g_output, uint2(2u, 2u), helper::Store(g_output, uint2(1u, 1u), (float4)helper::Load(g_input, helper::make_signed_push_0(uint2(1u, 1u)))));
    helper::Load(g_output, helper::make_signed(uint2(1u, 1u)));
}

struct ArgumentBuffer0
{
    [[id(0)]] const metal::texture2d<float> g_input;
    [[id(1)]] const metal::texture2d<float, metal::access::read_write> g_output;
    [[id(2)]] const metal::sampler g_sampler;
};

[[kernel]]
void ComputeShaderEntry(constant ArgumentBuffer0& set0 [[buffer(0)]]) {
    test(set0.g_input, set0.g_output, set0.g_sampler);
}

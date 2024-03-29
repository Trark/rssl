namespace helper {

template<typename T>
metal::vec<T, 4> Sample(metal::texturecube<T> texture, metal::sampler s, float3 coord) {
    return texture.sample(s, coord);
}

template<typename T>
metal::vec<T, 4> Sample(metal::texturecube<T> texture, metal::sampler s, float3 coord, float clamp) {
    return texture.sample(s, coord, metal::min_lod_clamp(clamp));
}

template<typename T>
metal::vec<T, 4> Sample(metal::texturecube<T> texture, metal::sampler s, float3 coord, float clamp, thread uint& status) {
    metal::sparse_color<metal::vec<T, 4>> color = texture.sparse_sample(s, coord, metal::min_lod_clamp(clamp));
    status = color.resident();
    return color.value();
}

template<typename T>
metal::vec<T, 4> SampleLevel(metal::texturecube<T> texture, metal::sampler s, float3 coord, float lod) {
    return texture.sample(s, coord, metal::level(lod));
}

template<typename T>
metal::vec<T, 4> SampleLevel(metal::texturecube<T> texture, metal::sampler s, float3 coord, float lod, thread uint& status) {
    metal::sparse_color<metal::vec<T, 4>> color = texture.sparse_sample(s, coord, metal::level(lod));
    status = color.resident();
    return color.value();
}

template<typename T>
metal::vec<T, 4> Sample(metal::texturecube_array<T> texture, metal::sampler s, float4 coord) {
    return texture.sample(s, coord.xyz, uint(coord.z));
}

template<typename T>
metal::vec<T, 4> Sample(metal::texturecube_array<T> texture, metal::sampler s, float4 coord, float clamp) {
    return texture.sample(s, coord.xyz, uint(coord.z), metal::min_lod_clamp(clamp));
}

template<typename T>
metal::vec<T, 4> Sample(metal::texturecube_array<T> texture, metal::sampler s, float4 coord, float clamp, thread uint& status) {
    metal::sparse_color<metal::vec<T, 4>> color = texture.sparse_sample(s, coord.xyz, uint(coord.z), metal::min_lod_clamp(clamp));
    status = color.resident();
    return color.value();
}

template<typename T>
metal::vec<T, 4> SampleLevel(metal::texturecube_array<T> texture, metal::sampler s, float4 coord, float lod) {
    return texture.sample(s, coord.xyz, uint(coord.z), metal::level(lod));
}

template<typename T>
metal::vec<T, 4> SampleLevel(metal::texturecube_array<T> texture, metal::sampler s, float4 coord, float lod, thread uint& status) {
    metal::sparse_color<metal::vec<T, 4>> color = texture.sparse_sample(s, coord.xyz, uint(coord.z), metal::level(lod));
    status = color.resident();
    return color.value();
}

} // namespace helper

void test(const metal::texturecube<float> g_input, const metal::texturecube_array<float> g_array, const metal::sampler g_sampler) {
    uint outInt;
    const float4 sample_base = helper::Sample(g_input, (metal::sampler)g_sampler, float3(0.0f, 0.0f, 0.0f));
    const float4 sample_clamp = helper::Sample(g_input, (metal::sampler)g_sampler, float3(0.0f, 0.0f, 0.0f), 0.0f);
    const float4 sample_status = helper::Sample(g_input, (metal::sampler)g_sampler, float3(0.0f, 0.0f, 0.0f), 0.0f, outInt);
    const float4 sample_level_base = helper::SampleLevel(g_input, (metal::sampler)g_sampler, float3(0.0f, 0.0f, 0.0f), 1.0f);
    const float4 sample_level_status = helper::SampleLevel(g_input, (metal::sampler)g_sampler, float3(0.0f, 0.0f, 0.0f), 1.0f, outInt);
    const float4 sample_array_base = helper::Sample(g_array, (metal::sampler)g_sampler, float4(0.0f, 0.0f, 0.0f, 0.0f));
    const float4 sample_array_clamp = helper::Sample(g_array, (metal::sampler)g_sampler, float4(0.0f, 0.0f, 0.0f, 0.0f), 0.0f);
    const float4 sample_array_status = helper::Sample(g_array, (metal::sampler)g_sampler, float4(0.0f, 0.0f, 0.0f, 0.0f), 0.0f, outInt);
    const float4 sample_array_level_base = helper::SampleLevel(g_array, (metal::sampler)g_sampler, float4(0.0f, 0.0f, 0.0f, 0.0f), 1.0f);
    const float4 sample_array_level_status = helper::SampleLevel(g_array, (metal::sampler)g_sampler, float4(0.0f, 0.0f, 0.0f, 0.0f), 1.0f, outInt);
}

struct ArgumentBuffer0
{
    [[id(0)]] const metal::texturecube<float> g_input;
    [[id(1)]] const metal::texturecube_array<float> g_array;
    [[id(2)]] const metal::sampler g_sampler;
};

[[kernel]]
void ComputeShaderEntry(constant ArgumentBuffer0& set0 [[buffer(0)]]) {
    test(set0.g_input, set0.g_array, set0.g_sampler);
}

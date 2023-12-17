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

} // namespace helper

void test(const metal::texturecube<float> g_input, const metal::texturecube_array<float> g_array, const metal::sampler g_sampler) {
    uint outInt;
    const float4 sample_base = helper::Sample(g_input, (metal::sampler)g_sampler, float3(0.0f, 0.0f, 0.0f));
    const float4 sample_clamp = helper::Sample(g_input, (metal::sampler)g_sampler, float3(0.0f, 0.0f, 0.0f), 0.0f);
    const float4 sample_status = helper::Sample(g_input, (metal::sampler)g_sampler, float3(0.0f, 0.0f, 0.0f), 0.0f, outInt);
    const float4 sample_array_base = helper::Sample(g_array, (metal::sampler)g_sampler, float4(0.0f, 0.0f, 0.0f, 0.0f));
    const float4 sample_array_clamp = helper::Sample(g_array, (metal::sampler)g_sampler, float4(0.0f, 0.0f, 0.0f, 0.0f), 0.0f);
    const float4 sample_array_status = helper::Sample(g_array, (metal::sampler)g_sampler, float4(0.0f, 0.0f, 0.0f, 0.0f), 0.0f, outInt);
}

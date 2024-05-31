void test_wave_intrinsics(uint thread_index_in_simdgroup, uint threads_per_simdgroup) {
    uint lane_count = threads_per_simdgroup;
    uint lane_index = thread_index_in_simdgroup;
    bool is_first = metal::simd_is_first();
    bool any_true = metal::simd_any(true);
    bool all_true = metal::simd_all(true);
    float3 wave_read_lane_first_f3 = metal::simd_broadcast_first(float3(0.0f, 0.0f, 0.0f));
    float3 wave_active_sum_f3 = metal::simd_sum(float3(0.0f, 0.0f, 0.0f));
    float3 wave_active_product_f3 = metal::simd_product(float3(0.0f, 0.0f, 0.0f));
    uint3 wave_active_bitand_u3 = metal::simd_and(uint3(0u, 0u, 0u));
    uint3 wave_active_bitor_u3 = metal::simd_or(uint3(0u, 0u, 0u));
    uint3 wave_active_bitxor_u3 = metal::simd_xor(uint3(0u, 0u, 0u));
    float3 wave_active_min_f3 = metal::simd_min(float3(0.0f, 0.0f, 0.0f));
    float3 wave_active_max_f3 = metal::simd_max(float3(0.0f, 0.0f, 0.0f));
    float3 wave_prefix_product_f3 = metal::simd_prefix_exclusive_product(float3(0.0f, 0.0f, 0.0f));
    float3 wave_prefix_sum_f3 = metal::simd_prefix_exclusive_sum(float3(0.0f, 0.0f, 0.0f));
}

void entry(uint thread_index_in_simdgroup, uint threads_per_simdgroup) {
    test_wave_intrinsics(thread_index_in_simdgroup, threads_per_simdgroup);
}

[[kernel]]
void ComputeShaderEntry(uint thread_index_in_simdgroup [[thread_index_in_simdgroup]], uint threads_per_simdgroup [[threads_per_simdgroup]]) {
    entry(thread_index_in_simdgroup, threads_per_simdgroup);
}

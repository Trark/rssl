void test_wave_intrinsics() {
    uint lane_count = WaveGetLaneCount();
    uint lane_index = WaveGetLaneIndex();
    bool is_first = WaveIsFirstLane();
    bool any_true = WaveActiveAnyTrue(true);
    bool all_true = WaveActiveAllTrue(true);
    uint4 ballot = WaveActiveBallot(true);
    float3 wave_read_lane_at_f3 = WaveReadLaneAt(float3(0.0f, 0.0f, 0.0f), 0u);
    float3 wave_read_lane_first_f3 = WaveReadLaneFirst(float3(0.0f, 0.0f, 0.0f));
    bool3 wave_active_all_equal_f3 = WaveActiveAllEqual(float3(0.0f, 0.0f, 0.0f));
    uint wave_active_count_bits = WaveActiveCountBits(true);
    float3 wave_active_sum_f3 = WaveActiveSum(float3(0.0f, 0.0f, 0.0f));
    float3 wave_active_product_f3 = WaveActiveProduct(float3(0.0f, 0.0f, 0.0f));
    uint3 wave_active_bitand_u3 = WaveActiveBitAnd(uint3(0u, 0u, 0u));
    uint3 wave_active_bitor_u3 = WaveActiveBitOr(uint3(0u, 0u, 0u));
    uint3 wave_active_bitxor_u3 = WaveActiveBitXor(uint3(0u, 0u, 0u));
    float3 wave_active_min_f3 = WaveActiveMin(float3(0.0f, 0.0f, 0.0f));
    float3 wave_active_max_f3 = WaveActiveMax(float3(0.0f, 0.0f, 0.0f));
    uint wave_prefix_count_bits = WavePrefixCountBits(true);
    float3 wave_prefix_product_f3 = WavePrefixProduct(float3(0.0f, 0.0f, 0.0f));
    float3 wave_prefix_sum_f3 = WavePrefixSum(float3(0.0f, 0.0f, 0.0f));
    float3 quad_read_x_f3 = QuadReadAcrossX(float3(0.0f, 0.0f, 0.0f));
    float3 quad_read_y_f3 = QuadReadAcrossY(float3(0.0f, 0.0f, 0.0f));
    float3 quad_read_d_f3 = QuadReadAcrossDiagonal(float3(0.0f, 0.0f, 0.0f));
    float3 quad_read_at_f3 = QuadReadLaneAt(float3(0.0f, 0.0f, 0.0f), 0u);
}

void entry() {
    test_wave_intrinsics();
}

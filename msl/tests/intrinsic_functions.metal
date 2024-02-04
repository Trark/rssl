void test() {
    bool all_1 = metal::all(true);
    bool all_2 = metal::all(bool2(true, true));
    bool all_3 = metal::all(bool3(true, true, true));
    bool all_4 = metal::all(bool4(true, true, true, true));
    bool any_1 = metal::any(true);
    bool any_2 = metal::any(bool2(true, true));
    bool any_3 = metal::any(bool3(true, true, true));
    bool any_4 = metal::any(bool4(true, true, true, true));
    uint select_u1 = metal::select(0u, 1u, true);
    int3 select_u3 = metal::select(int3(0, 0, 0), int3(0, 0, 0), bool3(true, true, true));
    float4 select_f4 = metal::select(float4(0.0f, 0.0f, 0.0f, 0.0f), float4(0.0f, 0.0f, 0.0f, 0.0f), bool4(true, true, true, true));
    int abs_i1 = metal::abs(0);
    int2 abs_i2 = metal::abs(int2(0, 0));
    int3 abs_i3 = metal::abs(int3(0, 0, 0));
    int4 abs_i4 = metal::abs(int4(0, 0, 0, 0));
    float abs_f1 = metal::abs(0.0f);
    float2 abs_f2 = metal::abs(float2(0.0f, 0.0f));
    float3 abs_f3 = metal::abs(float3(0.0f, 0.0f, 0.0f));
    float4 abs_f4 = metal::abs(float4(0.0f, 0.0f, 0.0f, 0.0f));
    int clamp_i1 = metal::clamp(0, 0, 0);
    int2 clamp_i2 = metal::clamp(int2(0, 0), int2(0, 0), int2(0, 0));
    int3 clamp_i3 = metal::clamp(int3(0, 0, 0), int3(0, 0, 0), int3(0, 0, 0));
    int4 clamp_14 = metal::clamp(int4(0, 0, 0, 0), int4(0, 0, 0, 0), int4(0, 0, 0, 0));
    float clamp_f1 = metal::clamp(0.0f, 0.0f, 0.0f);
    float2 clamp_f2 = metal::clamp(float2(0.0f, 0.0f), float2(0.0f, 0.0f), float2(0.0f, 0.0f));
    float3 clamp_f3 = metal::clamp(float3(0.0f, 0.0f, 0.0f), float3(0.0f, 0.0f, 0.0f), float3(0.0f, 0.0f, 0.0f));
    float4 clamp_f4 = metal::clamp(float4(0.0f, 0.0f, 0.0f, 0.0f), float4(0.0f, 0.0f, 0.0f, 0.0f), float4(0.0f, 0.0f, 0.0f, 0.0f));
    float3 cross_3 = metal::cross(float3(0.0f, 0.0f, 0.0f), float3(0.0f, 0.0f, 0.0f));
    float distance_2 = metal::distance(float2(0.0f, 0.0f), float2(0.0f, 0.0f));
    float distance_3 = metal::distance(float3(0.0f, 0.0f, 0.0f), float3(0.0f, 0.0f, 0.0f));
    float distance_4 = metal::distance(float4(0.0f, 0.0f, 0.0f, 0.0f), float4(0.0f, 0.0f, 0.0f, 0.0f));
    float dot_f2 = metal::dot(float2(0.0f, 0.0f), float2(0.0f, 0.0f));
    float dot_f3 = metal::dot(float3(0.0f, 0.0f, 0.0f), float3(0.0f, 0.0f, 0.0f));
    float dot_f4 = metal::dot(float4(0.0f, 0.0f, 0.0f, 0.0f), float4(0.0f, 0.0f, 0.0f, 0.0f));
    float3 mul_f3x3_f3 = (metal::float3x3)0 * (float3)0;
    float4 mul_f4x4_f4 = (metal::float4x4)0 * (float4)0;
    float lerp_f1 = metal::mix(0.0f, 0.0f, 0.0f);
    float2 lerp_f2 = metal::mix(float2(0.0f, 0.0f), float2(0.0f, 0.0f), float2(0.0f, 0.0f));
    float3 lerp_f3 = metal::mix(float3(0.0f, 0.0f, 0.0f), float3(0.0f, 0.0f, 0.0f), float3(0.0f, 0.0f, 0.0f));
    float4 lerp_f4 = metal::mix(float4(0.0f, 0.0f, 0.0f, 0.0f), float4(0.0f, 0.0f, 0.0f, 0.0f), float4(0.0f, 0.0f, 0.0f, 0.0f));
    float length_2 = metal::length(float2(0.0f, 0.0f));
    float length_3 = metal::length(float3(0.0f, 0.0f, 0.0f));
    float length_4 = metal::length(float4(0.0f, 0.0f, 0.0f, 0.0f));
    int min_i1 = metal::min(0, 0);
    int2 min_i2 = metal::min(int2(0, 0), int2(0, 0));
    int3 min_i3 = metal::min(int3(0, 0, 0), int3(0, 0, 0));
    int4 min_i4 = metal::min(int4(0, 0, 0, 0), int4(0, 0, 0, 0));
    float min_f1 = metal::min(0.0f, 0.0f);
    float2 min_f2 = metal::min(float2(0.0f, 0.0f), float2(0.0f, 0.0f));
    float3 min_f3 = metal::min(float3(0.0f, 0.0f, 0.0f), float3(0.0f, 0.0f, 0.0f));
    float4 min_f4 = metal::min(float4(0.0f, 0.0f, 0.0f, 0.0f), float4(0.0f, 0.0f, 0.0f, 0.0f));
    int max_i1 = metal::max(0, 0);
    int2 max_i2 = metal::max(int2(0, 0), int2(0, 0));
    int3 max_i3 = metal::max(int3(0, 0, 0), int3(0, 0, 0));
    int4 max_i4 = metal::max(int4(0, 0, 0, 0), int4(0, 0, 0, 0));
    float max_f1 = metal::max(0.0f, 0.0f);
    float2 max_f2 = metal::max(float2(0.0f, 0.0f), float2(0.0f, 0.0f));
    float3 max_f3 = metal::max(float3(0.0f, 0.0f, 0.0f), float3(0.0f, 0.0f, 0.0f));
    float4 max_f4 = metal::max(float4(0.0f, 0.0f, 0.0f, 0.0f), float4(0.0f, 0.0f, 0.0f, 0.0f));
    float2 normalize_2 = metal::normalize(float2(0.0f, 0.0f));
    float3 normalize_3 = metal::normalize(float3(0.0f, 0.0f, 0.0f));
    float4 normalize_4 = metal::normalize(float4(0.0f, 0.0f, 0.0f, 0.0f));
    float rcp_f1 = 1 / 0.0f;
    float2 rcp_f2 = 1 / float2(0.0f, 0.0f);
    float3 rcp_f3 = 1 / float3(0.0f, 0.0f, 0.0f);
    float4 rcp_f4 = 1 / float4(0.0f, 0.0f, 0.0f, 0.0f);
    float pow_f1 = metal::pow(0.0f, 0.0f);
    float2 pow_f2 = metal::pow(float2(0.0f, 0.0f), float2(0.0f, 0.0f));
    float3 pow_f3 = metal::pow(float3(0.0f, 0.0f, 0.0f), float3(0.0f, 0.0f, 0.0f));
    float4 pow_f4 = metal::pow(float4(0.0f, 0.0f, 0.0f, 0.0f), float4(0.0f, 0.0f, 0.0f, 0.0f));
    float saturate_1 = metal::saturate(0.0f);
    float2 saturate_2 = metal::saturate(float2(0.0f, 0.0f));
    float3 saturate_3 = metal::saturate(float3(0.0f, 0.0f, 0.0f));
    float4 saturate_4 = metal::saturate(float4(0.0f, 0.0f, 0.0f, 0.0f));
    int sign_i1 = (int)metal::sign((float)0);
    int2 sign_i2 = (int2)metal::sign((float2)int2(0, 0));
    int3 sign_i3 = (int3)metal::sign((float3)int3(0, 0, 0));
    int4 sign_i4 = (int4)metal::sign((float4)int4(0, 0, 0, 0));
    int sign_f1 = (int)metal::sign(0.0f);
    int2 sign_f2 = (int2)metal::sign(float2(0.0f, 0.0f));
    int3 sign_f3 = (int3)metal::sign(float3(0.0f, 0.0f, 0.0f));
    int4 sign_f4 = (int4)metal::sign(float4(0.0f, 0.0f, 0.0f, 0.0f));
    float smoothstep_f1 = metal::smoothstep(0.0f, 0.0f, 0.0f);
    float2 smoothstep_f2 = metal::smoothstep(float2(0.0f, 0.0f), float2(0.0f, 0.0f), float2(0.0f, 0.0f));
    float3 smoothstep_f3 = metal::smoothstep(float3(0.0f, 0.0f, 0.0f), float3(0.0f, 0.0f, 0.0f), float3(0.0f, 0.0f, 0.0f));
    float4 smoothstep_f4 = metal::smoothstep(float4(0.0f, 0.0f, 0.0f, 0.0f), float4(0.0f, 0.0f, 0.0f, 0.0f), float4(0.0f, 0.0f, 0.0f, 0.0f));
    float sqrt_1 = metal::sqrt(0.0f);
    float2 sqrt_2 = metal::sqrt(float2(0.0f, 0.0f));
    float3 sqrt_3 = metal::sqrt(float3(0.0f, 0.0f, 0.0f));
    float4 sqrt_4 = metal::sqrt(float4(0.0f, 0.0f, 0.0f, 0.0f));
    float rsqrt_1 = metal::rsqrt(0.0f);
    float2 rsqrt_2 = metal::rsqrt(float2(0.0f, 0.0f));
    float3 rsqrt_3 = metal::rsqrt(float3(0.0f, 0.0f, 0.0f));
    float4 rsqrt_4 = metal::rsqrt(float4(0.0f, 0.0f, 0.0f, 0.0f));
    float step_f1 = metal::step(0.0f, 0.0f);
    float2 step_f2 = metal::step(float2(0.0f, 0.0f), float2(0.0f, 0.0f));
    float3 step_f3 = metal::step(float3(0.0f, 0.0f, 0.0f), float3(0.0f, 0.0f, 0.0f));
    float4 step_f4 = metal::step(float4(0.0f, 0.0f, 0.0f, 0.0f), float4(0.0f, 0.0f, 0.0f, 0.0f));
    metal::float2x2 transpose_f2x2 = metal::transpose(metal::float2x2(0.0f, 0.0f, 0.0f, 0.0f));
    metal::float3x3 transpose_f3x3 = metal::transpose(metal::float3x3(0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f));
    metal::float4x4 transpose_f4x4 = metal::transpose(metal::float4x4(0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f));
    float determinant_f2x2 = metal::determinant(metal::float2x2(0.0f, 0.0f, 0.0f, 0.0f));
    float determinant_f3x3 = metal::determinant(metal::float3x3(0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f));
    float determinant_f4x4 = metal::determinant(metal::float4x4(0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f));
    float ddx_f1 = metal::dfdx(0.0f);
    float2 ddx_f2 = metal::dfdx(float2(0.0f, 0.0f));
    float3 ddx_f3 = metal::dfdx(float3(0.0f, 0.0f, 0.0f));
    float4 ddx_f4 = metal::dfdx(float4(0.0f, 0.0f, 0.0f, 0.0f));
    float ddx_fine_f1 = metal::dfdx(0.0f);
    float2 ddx_fine_f2 = metal::dfdx(float2(0.0f, 0.0f));
    float3 ddx_fine_f3 = metal::dfdx(float3(0.0f, 0.0f, 0.0f));
    float4 ddx_fine_f4 = metal::dfdx(float4(0.0f, 0.0f, 0.0f, 0.0f));
    float ddy_f1 = metal::dfdy(0.0f);
    float2 ddy_f2 = metal::dfdy(float2(0.0f, 0.0f));
    float3 ddy_f3 = metal::dfdy(float3(0.0f, 0.0f, 0.0f));
    float4 ddy_f4 = metal::dfdy(float4(0.0f, 0.0f, 0.0f, 0.0f));
    float ddy_fine_f1 = metal::dfdy(0.0f);
    float2 ddy_fine_f2 = metal::dfdy(float2(0.0f, 0.0f));
    float3 ddy_fine_f3 = metal::dfdy(float3(0.0f, 0.0f, 0.0f));
    float4 ddy_fine_f4 = metal::dfdy(float4(0.0f, 0.0f, 0.0f, 0.0f));
    uint non_uniform_resource_index = 0u;
}

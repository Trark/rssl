void test() {
    float out_f1;
    float2 out_f2;
    float3 out_f3;
    float4 out_f4;
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
    float acos_1 = metal::acos(0.0f);
    float2 acos_2 = metal::acos(float2(0.0f, 0.0f));
    float3 acos_3 = metal::acos(float3(0.0f, 0.0f, 0.0f));
    float4 acos_4 = metal::acos(float4(0.0f, 0.0f, 0.0f, 0.0f));
    float asin_1 = metal::asin(0.0f);
    float2 asin_2 = metal::asin(float2(0.0f, 0.0f));
    float3 asin_3 = metal::asin(float3(0.0f, 0.0f, 0.0f));
    float4 asin_4 = metal::asin(float4(0.0f, 0.0f, 0.0f, 0.0f));
    float atan_1 = metal::atan(0.0f);
    float2 atan_2 = metal::atan(float2(0.0f, 0.0f));
    float3 atan_3 = metal::atan(float3(0.0f, 0.0f, 0.0f));
    float4 atan_4 = metal::atan(float4(0.0f, 0.0f, 0.0f, 0.0f));
    float atan2_1 = metal::atan2(0.0f, 0.0f);
    float2 atan2_2 = metal::atan2(float2(0.0f, 0.0f), float2(0.0f, 0.0f));
    float3 atan2_3 = metal::atan2(float3(0.0f, 0.0f, 0.0f), float3(0.0f, 0.0f, 0.0f));
    float4 atan2_4 = metal::atan2(float4(0.0f, 0.0f, 0.0f, 0.0f), float4(0.0f, 0.0f, 0.0f, 0.0f));
    uint asuint_i1 = as_type<uint>(0);
    uint2 asuint_i2 = as_type<uint2>(int2(0, 0));
    uint3 asuint_i3 = as_type<uint3>(int3(0, 0, 0));
    uint4 asuint_14 = as_type<uint4>(int4(0, 0, 0, 0));
    uint asuint_f1 = as_type<uint>(0.0f);
    uint2 asuint_f2 = as_type<uint2>(float2(0.0f, 0.0f));
    uint3 asuint_f3 = as_type<uint3>(float3(0.0f, 0.0f, 0.0f));
    uint4 asuint_f4 = as_type<uint4>(float4(0.0f, 0.0f, 0.0f, 0.0f));
    float asfloat_u1 = as_type<float>(0u);
    float2 asfloat_u2 = as_type<float2>(uint2(0u, 0u));
    float3 asfloat_u3 = as_type<float3>(uint3(0u, 0u, 0u));
    float4 asfloat_u4 = as_type<float4>(uint4(0u, 0u, 0u, 0u));
    float asfloat_f1 = as_type<float>(0.0f);
    float2 asfloat_f2 = as_type<float2>(float2(0.0f, 0.0f));
    float3 asfloat_f3 = as_type<float3>(float3(0.0f, 0.0f, 0.0f));
    float4 asfloat_f4 = as_type<float4>(float4(0.0f, 0.0f, 0.0f, 0.0f));
    float exp_f1 = metal::exp(0.0f);
    float2 exp_f2 = metal::exp(float2(0.0f, 0.0f));
    float3 exp_f3 = metal::exp(float3(0.0f, 0.0f, 0.0f));
    float4 exp_f4 = metal::exp(float4(0.0f, 0.0f, 0.0f, 0.0f));
    float exp2_f1 = metal::exp2(0.0f);
    float2 exp2_f2 = metal::exp2(float2(0.0f, 0.0f));
    float3 exp2_f3 = metal::exp2(float3(0.0f, 0.0f, 0.0f));
    float4 exp2_f4 = metal::exp2(float4(0.0f, 0.0f, 0.0f, 0.0f));
    float log_f1 = metal::log(0.0f);
    float2 log_f2 = metal::log(float2(0.0f, 0.0f));
    float3 log_f3 = metal::log(float3(0.0f, 0.0f, 0.0f));
    float4 log_f4 = metal::log(float4(0.0f, 0.0f, 0.0f, 0.0f));
    float log2_f1 = metal::log2(0.0f);
    float2 log2_f2 = metal::log2(float2(0.0f, 0.0f));
    float3 log2_f3 = metal::log2(float3(0.0f, 0.0f, 0.0f));
    float4 log2_f4 = metal::log2(float4(0.0f, 0.0f, 0.0f, 0.0f));
    float log10_f1 = metal::log10(0.0f);
    float2 log10_f2 = metal::log10(float2(0.0f, 0.0f));
    float3 log10_f3 = metal::log10(float3(0.0f, 0.0f, 0.0f));
    float4 log10_f4 = metal::log10(float4(0.0f, 0.0f, 0.0f, 0.0f));
    int clamp_i1 = metal::clamp(0, 0, 0);
    int2 clamp_i2 = metal::clamp(int2(0, 0), int2(0, 0), int2(0, 0));
    int3 clamp_i3 = metal::clamp(int3(0, 0, 0), int3(0, 0, 0), int3(0, 0, 0));
    int4 clamp_14 = metal::clamp(int4(0, 0, 0, 0), int4(0, 0, 0, 0), int4(0, 0, 0, 0));
    float clamp_f1 = metal::clamp(0.0f, 0.0f, 0.0f);
    float2 clamp_f2 = metal::clamp(float2(0.0f, 0.0f), float2(0.0f, 0.0f), float2(0.0f, 0.0f));
    float3 clamp_f3 = metal::clamp(float3(0.0f, 0.0f, 0.0f), float3(0.0f, 0.0f, 0.0f), float3(0.0f, 0.0f, 0.0f));
    float4 clamp_f4 = metal::clamp(float4(0.0f, 0.0f, 0.0f, 0.0f), float4(0.0f, 0.0f, 0.0f, 0.0f), float4(0.0f, 0.0f, 0.0f, 0.0f));
    float cos_1 = metal::cos(0.0f);
    float2 cos_2 = metal::cos(float2(0.0f, 0.0f));
    float3 cos_3 = metal::cos(float3(0.0f, 0.0f, 0.0f));
    float4 cos_4 = metal::cos(float4(0.0f, 0.0f, 0.0f, 0.0f));
    float cosh_1 = metal::cosh(0.0f);
    float2 cosh_2 = metal::cosh(float2(0.0f, 0.0f));
    float3 cosh_3 = metal::cosh(float3(0.0f, 0.0f, 0.0f));
    float4 cosh_4 = metal::cosh(float4(0.0f, 0.0f, 0.0f, 0.0f));
    float3 cross_3 = metal::cross(float3(0.0f, 0.0f, 0.0f), float3(0.0f, 0.0f, 0.0f));
    float distance_2 = metal::distance(float2(0.0f, 0.0f), float2(0.0f, 0.0f));
    float distance_3 = metal::distance(float3(0.0f, 0.0f, 0.0f), float3(0.0f, 0.0f, 0.0f));
    float distance_4 = metal::distance(float4(0.0f, 0.0f, 0.0f, 0.0f), float4(0.0f, 0.0f, 0.0f, 0.0f));
    float dot_f2 = metal::dot(float2(0.0f, 0.0f), float2(0.0f, 0.0f));
    float dot_f3 = metal::dot(float3(0.0f, 0.0f, 0.0f), float3(0.0f, 0.0f, 0.0f));
    float dot_f4 = metal::dot(float4(0.0f, 0.0f, 0.0f, 0.0f), float4(0.0f, 0.0f, 0.0f, 0.0f));
    float3 mul_f3x3_f3 = (metal::float3x3)0 * (float3)0;
    float4 mul_f4x4_f4 = (metal::float4x4)0 * (float4)0;
    float floor_1 = metal::floor(0.0f);
    float2 floor_2 = metal::floor(float2(0.0f, 0.0f));
    float3 floor_3 = metal::floor(float3(0.0f, 0.0f, 0.0f));
    float4 floor_4 = metal::floor(float4(0.0f, 0.0f, 0.0f, 0.0f));
    float ceil_1 = metal::ceil(0.0f);
    float2 ceil_2 = metal::ceil(float2(0.0f, 0.0f));
    float3 ceil_3 = metal::ceil(float3(0.0f, 0.0f, 0.0f));
    float4 ceil_4 = metal::ceil(float4(0.0f, 0.0f, 0.0f, 0.0f));
    float trunc_1 = metal::trunc(0.0f);
    float2 trunc_2 = metal::trunc(float2(0.0f, 0.0f));
    float3 trunc_3 = metal::trunc(float3(0.0f, 0.0f, 0.0f));
    float4 trunc_4 = metal::trunc(float4(0.0f, 0.0f, 0.0f, 0.0f));
    float round_1 = metal::round(0.0f);
    float2 round_2 = metal::round(float2(0.0f, 0.0f));
    float3 round_3 = metal::round(float3(0.0f, 0.0f, 0.0f));
    float4 round_4 = metal::round(float4(0.0f, 0.0f, 0.0f, 0.0f));
    float frac_1 = metal::fract(0.0f);
    float2 frac_2 = metal::fract(float2(0.0f, 0.0f));
    float3 frac_3 = metal::fract(float3(0.0f, 0.0f, 0.0f));
    float4 frac_4 = metal::fract(float4(0.0f, 0.0f, 0.0f, 0.0f));
    float modf_1 = metal::modf(0.0f, out_f1);
    float2 modf_2 = metal::modf(float2(0.0f, 0.0f), out_f2);
    float3 modf_3 = metal::modf(float3(0.0f, 0.0f, 0.0f), out_f3);
    float4 modf_4 = metal::modf(float4(0.0f, 0.0f, 0.0f, 0.0f), out_f4);
    float fmod_1 = metal::fmod(0.0f, 0.0f);
    float2 fmod_2 = metal::fmod(float2(0.0f, 0.0f), float2(0.0f, 0.0f));
    float3 fmod_3 = metal::fmod(float3(0.0f, 0.0f, 0.0f), float3(0.0f, 0.0f, 0.0f));
    float4 fmod_4 = metal::fmod(float4(0.0f, 0.0f, 0.0f, 0.0f), float4(0.0f, 0.0f, 0.0f, 0.0f));
    float lerp_f1 = metal::mix(0.0f, 0.0f, 0.0f);
    float2 lerp_f2 = metal::mix(float2(0.0f, 0.0f), float2(0.0f, 0.0f), float2(0.0f, 0.0f));
    float3 lerp_f3 = metal::mix(float3(0.0f, 0.0f, 0.0f), float3(0.0f, 0.0f, 0.0f), float3(0.0f, 0.0f, 0.0f));
    float4 lerp_f4 = metal::mix(float4(0.0f, 0.0f, 0.0f, 0.0f), float4(0.0f, 0.0f, 0.0f, 0.0f), float4(0.0f, 0.0f, 0.0f, 0.0f));
    bool isnan_1 = metal::isnan(0.0f);
    bool2 isnan_2 = metal::isnan(float2(0.0f, 0.0f));
    bool3 isnan_3 = metal::isnan(float3(0.0f, 0.0f, 0.0f));
    bool4 isnan_4 = metal::isnan(float4(0.0f, 0.0f, 0.0f, 0.0f));
    bool isinf_1 = metal::isinf(0.0f);
    bool2 isinf_2 = metal::isinf(float2(0.0f, 0.0f));
    bool3 isinf_3 = metal::isinf(float3(0.0f, 0.0f, 0.0f));
    bool4 isinf_4 = metal::isinf(float4(0.0f, 0.0f, 0.0f, 0.0f));
    bool isfinite_1 = metal::isfinite(0.0f);
    bool2 isfinite_2 = metal::isfinite(float2(0.0f, 0.0f));
    bool3 isfinite_3 = metal::isfinite(float3(0.0f, 0.0f, 0.0f));
    bool4 isfinite_4 = metal::isfinite(float4(0.0f, 0.0f, 0.0f, 0.0f));
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
    float2 reflect_f2 = metal::reflect(float2(0.0f, 0.0f), float2(0.0f, 0.0f));
    float3 reflect_f3 = metal::reflect(float3(0.0f, 0.0f, 0.0f), float3(0.0f, 0.0f, 0.0f));
    float4 reflect_f4 = metal::reflect(float4(0.0f, 0.0f, 0.0f, 0.0f), float4(0.0f, 0.0f, 0.0f, 0.0f));
    float2 refract_f2 = metal::refract(float2(0.0f, 0.0f), float2(0.0f, 0.0f), 0.0f);
    float3 refract_f3 = metal::refract(float3(0.0f, 0.0f, 0.0f), float3(0.0f, 0.0f, 0.0f), 0.0f);
    float4 refract_f4 = metal::refract(float4(0.0f, 0.0f, 0.0f, 0.0f), float4(0.0f, 0.0f, 0.0f, 0.0f), 0.0f);
    uint countbits_u1 = metal::popcount(0u);
    uint2 countbits_u2 = metal::popcount(uint2(0u, 0u));
    uint3 countbits_u3 = metal::popcount(uint3(0u, 0u, 0u));
    uint4 countbits_u4 = metal::popcount(uint4(0u, 0u, 0u, 0u));
    uint reversebits_u1 = metal::reverse_bits(0u);
    uint2 reversebits_u2 = metal::reverse_bits(uint2(0u, 0u));
    uint3 reversebits_u3 = metal::reverse_bits(uint3(0u, 0u, 0u));
    uint4 reversebits_u4 = metal::reverse_bits(uint4(0u, 0u, 0u, 0u));
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
    float sin_1 = metal::sin(0.0f);
    float2 sin_2 = metal::sin(float2(0.0f, 0.0f));
    float3 sin_3 = metal::sin(float3(0.0f, 0.0f, 0.0f));
    float4 sin_4 = metal::sin(float4(0.0f, 0.0f, 0.0f, 0.0f));
    float sinh_1 = metal::sinh(0.0f);
    float2 sinh_2 = metal::sinh(float2(0.0f, 0.0f));
    float3 sinh_3 = metal::sinh(float3(0.0f, 0.0f, 0.0f));
    float4 sinh_4 = metal::sinh(float4(0.0f, 0.0f, 0.0f, 0.0f));
    float tan_1 = metal::tan(0.0f);
    float2 tan_2 = metal::tan(float2(0.0f, 0.0f));
    float3 tan_3 = metal::tan(float3(0.0f, 0.0f, 0.0f));
    float4 tan_4 = metal::tan(float4(0.0f, 0.0f, 0.0f, 0.0f));
    float tanh_1 = metal::tanh(0.0f);
    float2 tanh_2 = metal::tanh(float2(0.0f, 0.0f));
    float3 tanh_3 = metal::tanh(float3(0.0f, 0.0f, 0.0f));
    float4 tanh_4 = metal::tanh(float4(0.0f, 0.0f, 0.0f, 0.0f));
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

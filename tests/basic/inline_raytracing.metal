namespace helper {

metal::raytracing::intersection_params intersection_params(uint flags) {
    metal::raytracing::intersection_params params;
    params.force_opacity(flags & 1u ? metal::raytracing::forced_opacity::opaque : flags & 2u ? metal::raytracing::forced_opacity::non_opaque : metal::raytracing::forced_opacity::none);
    params.accept_any_intersection(flags & 4u);
    params.set_triangle_cull_mode(flags & 16u ? metal::raytracing::triangle_cull_mode::back : flags & 32u ? metal::raytracing::triangle_cull_mode::front : metal::raytracing::triangle_cull_mode::none);
    params.set_opacity_cull_mode(flags & 64u ? metal::raytracing::opacity_cull_mode::opaque : flags & 128u ? metal::raytracing::opacity_cull_mode::non_opaque : metal::raytracing::opacity_cull_mode::none);
    params.set_geometry_cull_mode((flags & 256u ? metal::raytracing::geometry_cull_mode::triangle : metal::raytracing::geometry_cull_mode::none) | (flags & 512u ? metal::raytracing::geometry_cull_mode::bounding_box : metal::raytracing::geometry_cull_mode::none));
    return params;
}

} // namespace helper

void entry(const metal::raytracing::instance_acceleration_structure g_bvh) {
    metal::raytracing::intersection_query<metal::raytracing::instancing, metal::raytracing::triangle_data> query;
    metal::raytracing::ray ray;
    ray.origin = float3(0.0f, 0.0f, 0.0f);
    ray.min_distance = 0.0f;
    ray.direction = float3(1.0f, 0.0f, 0.0f);
    ray.max_distance = 1.0f;
    query.reset(ray, (metal::raytracing::instance_acceleration_structure)g_bvh, 0u, helper::intersection_params(513u | 0u));
}

struct ArgumentBuffer0
{
    [[id(0)]] const metal::raytracing::instance_acceleration_structure g_bvh;
};

[[kernel]]
void ComputeShaderEntry(constant ArgumentBuffer0& set0 [[buffer(0)]]) {
    entry(set0.g_bvh);
}

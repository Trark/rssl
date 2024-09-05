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

uint to_committed_status(metal::raytracing::intersection_type value) {
    switch (value)
    {
        case metal::raytracing::intersection_type::none:
        return 0u;
        case metal::raytracing::intersection_type::triangle:
        return 1u;
        case metal::raytracing::intersection_type::bounding_box:
        return 2u;
    }
}

uint to_candidate_type(metal::raytracing::intersection_type value) {
    switch (value)
    {
        case metal::raytracing::intersection_type::triangle:
        return 0u;
        case metal::raytracing::intersection_type::bounding_box:
        return 1u;
    }
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
    bool c = query.next();
    query.abort();
    0u;
    1u;
    2u;
    uint committed_status = helper::to_committed_status(query.get_committed_intersection_type());
    0u;
    1u;
    uint candidate_type = helper::to_candidate_type(query.get_candidate_intersection_type());
    bool candidate_procedural_primitive_non_opaque = query.is_candidate_non_opaque_bounding_box();
    query.commit_triangle_intersection();
    query.commit_bounding_box_intersection(2.0f);
    float3 world_ray_origin = query.get_world_space_ray_origin();
    float3 world_ray_direction = query.get_world_space_ray_direction();
    float ray_t_min = query.get_ray_min_distance();
    float candidate_triangle_ray_t = query.get_candidate_triangle_distance();
    float committed_ray_t = query.get_committed_distance();
    uint candidate_instance_index = query.get_candidate_instance_id();
    uint candidate_instance_id = query.get_candidate_user_instance_id();
    uint candidate_geometry_index = query.get_candidate_geometry_id();
    uint candidate_primitive_index = query.get_candidate_primitive_id();
    float3 candidate_object_ray_origin = query.get_candidate_ray_origin();
    float3 candidate_object_ray_direction = query.get_candidate_ray_direction();
    metal::float4x3 candidate_object_to_world_3x4 = query.get_candidate_object_to_world_transform();
    metal::float3x4 candidate_object_to_world_4x3 = metal::transpose(query.get_candidate_object_to_world_transform());
    metal::float4x3 candidate_world_to_object_3x4 = query.get_candidate_world_to_object_transform();
    metal::float3x4 candidate_world_to_object_4x3 = metal::transpose(query.get_candidate_world_to_object_transform());
    uint committed_instance_index = query.get_committed_instance_id();
    uint committed_instance_id = query.get_committed_user_instance_id();
    uint committed_geometry_index = query.get_committed_geometry_id();
    uint committed_primitive_index = query.get_committed_primitive_id();
    float3 committed_object_ray_origin = query.get_committed_ray_origin();
    float3 committed_object_ray_direction = query.get_committed_ray_direction();
    metal::float4x3 committed_object_to_world_3x4 = query.get_committed_object_to_world_transform();
    metal::float3x4 committed_object_to_world_4x3 = metal::transpose(query.get_committed_object_to_world_transform());
    metal::float4x3 committed_world_to_object_3x4 = query.get_committed_world_to_object_transform();
    metal::float3x4 committed_world_to_object_4x3 = metal::transpose(query.get_committed_world_to_object_transform());
    float2 candidate_triangle_barycentrics = query.get_candidate_triangle_barycentric_coord();
    bool candidate_triangle_front_face = query.is_candidate_triangle_front_facing();
    float2 committed_triangle_barycentrics = query.get_committed_triangle_barycentric_coord();
    bool committed_triangle_front_face = query.is_committed_triangle_front_facing();
}

struct ArgumentBuffer0
{
    [[id(0)]] const metal::raytracing::instance_acceleration_structure g_bvh;
};

[[kernel]]
void ComputeShaderEntry(constant ArgumentBuffer0& set0 [[buffer(0)]]) {
    entry(set0.g_bvh);
}

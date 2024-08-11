void entry() {
    metal::raytracing::ray ray;
    ray.origin = float3(0.0f, 0.0f, 0.0f);
    ray.min_distance = 0.0f;
    ray.direction = float3(1.0f, 0.0f, 0.0f);
    ray.max_distance = 1.0f;
}

struct ArgumentBuffer0
{
    [[id(0)]] const metal::raytracing::instance_acceleration_structure g_bvh;
};

[[kernel]]
void ComputeShaderEntry(constant ArgumentBuffer0& set0 [[buffer(0)]]) {
    entry();
}

RaytracingAccelerationStructure g_bvh : register(t0);

void test() {
    RayQuery<0> query;
    RayDesc ray;
    ray.Origin = float3(0.0f, 0.0f, 0.0f);
    ray.TMin = 0.0f;
    ray.Direction = float3(1.0f, 0.0f, 0.0f);
    ray.TMax = 1.0f;
}

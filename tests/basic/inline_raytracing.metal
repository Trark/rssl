void entry() {}

struct ArgumentBuffer0
{
    [[id(0)]] const metal::raytracing::instance_acceleration_structure g_bvh;
};

[[kernel]]
void ComputeShaderEntry(constant ArgumentBuffer0& set0 [[buffer(0)]]) {
    entry();
}

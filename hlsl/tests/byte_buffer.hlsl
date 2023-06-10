ByteAddressBuffer g_input : register(t0);
RWByteAddressBuffer g_output : register(u1);

struct MyStruct
{
    uint m;
};

void test() {
    const uint x1 = g_input.Load(0u);
    const uint2 x2 = g_input.Load2(0u);
    const uint3 x3 = g_input.Load3(0u);
    const uint4 x4 = g_input.Load4(0u);
    const MyStruct x5 = g_input.Load<MyStruct>(0u);
    const uint y1 = g_output.Load(0u);
    const uint2 y2 = g_output.Load2(0u);
    const uint3 y3 = g_output.Load3(0u);
    const uint4 y4 = g_output.Load4(0u);
    const MyStruct y5 = g_output.Load<MyStruct>(0u);
    g_output.Store<uint>(0u, (uint)x1);
    g_output.Store2(0u, (uint2)x2);
    g_output.Store3(0u, (uint3)x3);
    g_output.Store4(0u, (uint4)x4);
    g_output.Store<float>(0u, 0.0f);
    g_output.Store2(0u, (uint2)0.0);
    g_output.Store3(0u, (uint3)0.0);
    g_output.Store4(0u, (uint4)0.0);
    uint outInt;
    g_input.GetDimensions(outInt);
    g_output.GetDimensions(outInt);
    g_output.InterlockedAdd(0u, 0u, outInt);
    g_output.InterlockedAnd(0u, 0u, outInt);
    g_output.InterlockedCompareExchange(0u, 0u, 0u, outInt);
    g_output.InterlockedCompareStore(0u, 0u, outInt);
    g_output.InterlockedExchange(0u, 0u, outInt);
    g_output.InterlockedMax(0u, 0u, outInt);
    g_output.InterlockedMin(0u, 0u, outInt);
    g_output.InterlockedOr(0u, 0u, outInt);
    g_output.InterlockedXor(0u, 0u, outInt);
}

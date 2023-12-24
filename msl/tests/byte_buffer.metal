struct MyStruct
{
    uint m;
};

void test(device const uint8_t* const g_input, device uint8_t* const g_output) {
    const uint x1 = *reinterpret_cast<device const uint*>(g_input + 0u);
    const uint2 x2 = *reinterpret_cast<device const uint2*>(g_input + 0u);
    const uint3 x3 = *reinterpret_cast<device const uint3*>(g_input + 0u);
    const uint4 x4 = *reinterpret_cast<device const uint4*>(g_input + 0u);
    const MyStruct x5 = *reinterpret_cast<device const MyStruct*>(g_input + 0u);
    const uint y1 = *reinterpret_cast<device const uint*>(g_output + 0u);
    const uint2 y2 = *reinterpret_cast<device const uint2*>(g_output + 0u);
    const uint3 y3 = *reinterpret_cast<device const uint3*>(g_output + 0u);
    const uint4 y4 = *reinterpret_cast<device const uint4*>(g_output + 0u);
    const MyStruct y5 = *reinterpret_cast<device const MyStruct*>(g_output + 0u);
    *reinterpret_cast<device uint*>(g_output + 0u) = (uint)x1;
    *reinterpret_cast<device uint2*>(g_output + 0u) = (uint2)x2;
    *reinterpret_cast<device uint3*>(g_output + 0u) = (uint3)x3;
    *reinterpret_cast<device uint4*>(g_output + 0u) = (uint4)x4;
    *reinterpret_cast<device float*>(g_output + 0u) = 0.0f;
    *reinterpret_cast<device uint2*>(g_output + 0u) = (uint2)0.0;
    *reinterpret_cast<device uint3*>(g_output + 0u) = (uint3)0.0;
    *reinterpret_cast<device uint4*>(g_output + 0u) = (uint4)0.0;
    uint outInt;
    (void)(outInt = atomic_fetch_add_explicit(reinterpret_cast<device metal::atomic<uint>*>(g_output + 4u), 7u, metal::memory_order::memory_order_relaxed));
    (void)(outInt = atomic_fetch_and_explicit(reinterpret_cast<device metal::atomic<uint>*>(g_output + 4u), 7u, metal::memory_order::memory_order_relaxed));
    (void)(outInt = atomic_exchange_explicit(reinterpret_cast<device metal::atomic<uint>*>(g_output + 4u), 7u, metal::memory_order::memory_order_relaxed));
    (void)(outInt = atomic_fetch_max_explicit(reinterpret_cast<device metal::atomic<uint>*>(g_output + 4u), 7u, metal::memory_order::memory_order_relaxed));
    (void)(outInt = atomic_fetch_min_explicit(reinterpret_cast<device metal::atomic<uint>*>(g_output + 4u), 7u, metal::memory_order::memory_order_relaxed));
    (void)(outInt = atomic_fetch_or_explicit(reinterpret_cast<device metal::atomic<uint>*>(g_output + 4u), 7u, metal::memory_order::memory_order_relaxed));
    (void)(outInt = atomic_fetch_xor_explicit(reinterpret_cast<device metal::atomic<uint>*>(g_output + 4u), 7u, metal::memory_order::memory_order_relaxed));
}

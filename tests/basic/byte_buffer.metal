namespace helper {

struct ByteAddressBuffer
{
    device const uint8_t* address;
    uint64_t size;

    template<typename T>
    T Load(uint offset) const {
        return *reinterpret_cast<device const T*>(address + offset);
    }

    void GetDimensions(thread uint& dim) const {
        dim = static_cast<uint>(size);
    }
};

struct RWByteAddressBuffer
{
    device uint8_t* address;
    uint64_t size;

    template<typename T>
    T Load(uint offset) const {
        return *reinterpret_cast<device const T*>(address + offset);
    }

    template<typename T>
    T Store(uint offset, T value) const {
        *reinterpret_cast<device T*>(address + offset) = value;
    }

    void InterlockedAdd(uint dest, uint value, thread uint& original_value) const {
        original_value = atomic_fetch_add_explicit(reinterpret_cast<device metal::atomic<uint>*>(address + dest), value, metal::memory_order::memory_order_relaxed);
    }

    void InterlockedAnd(uint dest, uint value, thread uint& original_value) const {
        original_value = atomic_fetch_and_explicit(reinterpret_cast<device metal::atomic<uint>*>(address + dest), value, metal::memory_order::memory_order_relaxed);
    }

    void InterlockedExchange(uint dest, uint value, thread uint& original_value) const {
        original_value = atomic_exchange_explicit(reinterpret_cast<device metal::atomic<uint>*>(address + dest), value, metal::memory_order::memory_order_relaxed);
    }

    void InterlockedMax(uint dest, uint value, thread uint& original_value) const {
        original_value = atomic_fetch_max_explicit(reinterpret_cast<device metal::atomic<uint>*>(address + dest), value, metal::memory_order::memory_order_relaxed);
    }

    void InterlockedMin(uint dest, uint value, thread uint& original_value) const {
        original_value = atomic_fetch_min_explicit(reinterpret_cast<device metal::atomic<uint>*>(address + dest), value, metal::memory_order::memory_order_relaxed);
    }

    void InterlockedOr(uint dest, uint value, thread uint& original_value) const {
        original_value = atomic_fetch_or_explicit(reinterpret_cast<device metal::atomic<uint>*>(address + dest), value, metal::memory_order::memory_order_relaxed);
    }

    void InterlockedXor(uint dest, uint value, thread uint& original_value) const {
        original_value = atomic_fetch_xor_explicit(reinterpret_cast<device metal::atomic<uint>*>(address + dest), value, metal::memory_order::memory_order_relaxed);
    }

    void GetDimensions(thread uint& dim) const {
        dim = static_cast<uint>(size);
    }
};

} // namespace helper

struct MyStruct
{
    uint m;
};

void test(const helper::ByteAddressBuffer g_input, const helper::RWByteAddressBuffer g_output) {
    const uint x1 = g_input.Load<uint>(0u);
    const uint2 x2 = g_input.Load<uint2>(0u);
    const uint3 x3 = g_input.Load<uint3>(0u);
    const uint4 x4 = g_input.Load<uint4>(0u);
    const MyStruct x5 = g_input.Load<MyStruct>(0u);
    const uint y1 = g_output.Load<uint>(0u);
    const uint2 y2 = g_output.Load<uint2>(0u);
    const uint3 y3 = g_output.Load<uint3>(0u);
    const uint4 y4 = g_output.Load<uint4>(0u);
    const MyStruct y5 = g_output.Load<MyStruct>(0u);
    g_output.Store<uint>(0u, (uint)x1);
    g_output.Store<uint2>(0u, (uint2)x2);
    g_output.Store<uint3>(0u, (uint3)x3);
    g_output.Store<uint4>(0u, (uint4)x4);
    g_output.Store<float>(0u, 0.0f);
    g_output.Store<uint2>(0u, (uint2)0.0);
    g_output.Store<uint3>(0u, (uint3)0.0);
    g_output.Store<uint4>(0u, (uint4)0.0);
    uint outInt;
    g_input.GetDimensions(outInt);
    g_output.GetDimensions(outInt);
    g_output.InterlockedAdd(4u, 7u, outInt);
    g_output.InterlockedAnd(4u, 7u, outInt);
    g_output.InterlockedExchange(4u, 7u, outInt);
    g_output.InterlockedMax(4u, 7u, outInt);
    g_output.InterlockedMin(4u, 7u, outInt);
    g_output.InterlockedOr(4u, 7u, outInt);
    g_output.InterlockedXor(4u, 7u, outInt);
}

struct ArgumentBuffer0
{
    [[id(0)]] const helper::ByteAddressBuffer g_input;
    [[id(2)]] const helper::RWByteAddressBuffer g_output;
};

[[kernel]]
void ComputeShaderEntry(constant ArgumentBuffer0& set0 [[buffer(0)]]) {
    test(set0.g_input, set0.g_output);
}

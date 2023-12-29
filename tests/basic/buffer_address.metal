namespace helper {

struct ByteAddressBuffer
{
    device const uint8_t* address;
    uint64_t size;

    template<typename T>
    T Load(uint offset) const {
        return *reinterpret_cast<device const T*>(address + offset);
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
};

} // namespace helper

struct MyStruct
{
    uint m;
};

void test(const helper::ByteAddressBuffer g_input, const helper::RWByteAddressBuffer g_output) {
    const MyStruct s1 = g_input.Load<MyStruct>(0u);
    const MyStruct s2 = g_output.Load<MyStruct>(0u);
    g_output.Store<MyStruct>(4u, (MyStruct)s1);
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

namespace helper {

template<typename T>
struct StructuredBuffer
{
    device const T* address;
    uint64_t size;

    T Load(uint location) const {
        return address[location];
    }

    void GetDimensions(thread uint& numStructs, thread uint& stride) const {
        numStructs = static_cast<uint>(size);
        stride = sizeof(T);
    }
};

template<typename T>
struct RWStructuredBuffer
{
    device T* address;
    uint64_t size;

    T Load(uint location) const {
        return address[location];
    }

    void GetDimensions(thread uint& numStructs, thread uint& stride) const {
        numStructs = static_cast<uint>(size);
        stride = sizeof(T);
    }
};

} // namespace helper

struct MyStruct
{
    uint m;
};

void test(const helper::StructuredBuffer<MyStruct> g_input, const helper::RWStructuredBuffer<MyStruct> g_output) {
    uint outInt;
    const MyStruct s1 = g_input.Load(0);
    const MyStruct s2 = g_output.Load(0);
    g_input.GetDimensions(outInt, outInt);
    g_output.GetDimensions(outInt, outInt);
}

struct ArgumentBuffer0
{
    [[id(0)]] const helper::StructuredBuffer<MyStruct> g_input;
    [[id(2)]] const helper::RWStructuredBuffer<MyStruct> g_output;
};

[[kernel]]
void ComputeShaderEntry(constant ArgumentBuffer0& set0 [[buffer(0)]]) {
    test(set0.g_input, set0.g_output);
}

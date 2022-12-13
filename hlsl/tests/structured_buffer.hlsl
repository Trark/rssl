struct MyStruct
{
    const uint m;
};

StructuredBuffer<MyStruct> g_input : register(t0);
RWStructuredBuffer<MyStruct> g_output : register(u1);

void test() {
    uint outInt;
    const MyStruct s1 = g_input.Load(0);
    const MyStruct s2 = g_output.Load(0);
    const MyStruct s3 = g_input.Load(0, outInt);
    const MyStruct s4 = g_output.Load(0, outInt);
    g_input.GetDimensions(outInt, outInt);
    g_output.GetDimensions(outInt, outInt);
    g_output[2u] = g_output[1u] = (MyStruct)g_input[1u];
}

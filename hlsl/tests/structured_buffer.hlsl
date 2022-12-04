struct MyStruct
{
    const uint m;
};

StructuredBuffer<MyStruct> g_input : register(t0);
RWStructuredBuffer<MyStruct> g_output : register(u1);

void test() {
    const MyStruct s1 = g_input.Load(0);
    const MyStruct s2 = g_output.Load(0);
    g_output[2u] = g_output[1u] = (MyStruct)g_input[1u];
}

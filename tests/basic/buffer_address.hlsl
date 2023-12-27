ByteAddressBuffer g_input : register(t0);
RWByteAddressBuffer g_output : register(u1);

struct MyStruct
{
    uint m;
};

void test() {
    const MyStruct s1 = g_input.Load<MyStruct>(0u);
    const MyStruct s2 = g_output.Load<MyStruct>(0u);
    g_output.Store<MyStruct>(4u, (MyStruct)s1);
}

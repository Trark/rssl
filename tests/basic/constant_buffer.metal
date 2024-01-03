struct MyStruct
{
    uint m;
};

void test(constant MyStruct& g_constants) {
    MyStruct s1 = (MyStruct)g_constants;
    constant MyStruct& localObject = (constant MyStruct&)g_constants;
    MyStruct s2 = (MyStruct)localObject;
}

struct ArgumentBuffer0
{
    [[id(0)]] constant MyStruct& g_constants;
};

[[kernel]]
void ComputeShaderEntry(constant ArgumentBuffer0& set0 [[buffer(0)]]) {
    test(set0.g_constants);
}

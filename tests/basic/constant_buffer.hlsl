struct MyStruct
{
    uint m;
};

ConstantBuffer<MyStruct> g_constants : register(b0);

void test() {
    MyStruct s1 = (MyStruct)g_constants;
    ConstantBuffer<MyStruct> localObject = (ConstantBuffer<MyStruct>)g_constants;
    MyStruct s2 = (MyStruct)localObject;
}

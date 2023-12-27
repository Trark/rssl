struct MyStruct
{
    uint m;
};

void test(device const uint8_t* const g_input, device uint8_t* const g_output) {
    const MyStruct s1 = *reinterpret_cast<device const MyStruct*>(g_input + 0u);
    const MyStruct s2 = *reinterpret_cast<device const MyStruct*>(g_output + 0u);
    *reinterpret_cast<device MyStruct*>(g_output + 4u) = (MyStruct)s1;
}

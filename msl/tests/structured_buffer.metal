struct MyStruct
{
    uint m;
};

void test(device const MyStruct* const g_input, device MyStruct* const g_output) {
    const MyStruct s1 = g_input[0];
    const MyStruct s2 = g_output[0];
}

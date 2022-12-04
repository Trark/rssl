[[vk::binding(0)]] extern const uint64_t g_input;
[[vk::binding(1)]] extern const uint64_t g_output;

struct MyStruct
{
    uint m;
};

void test() {
    const MyStruct s1 = vk::RawBufferLoad<MyStruct>(g_input + uint64_t(0u));
    const MyStruct s2 = vk::RawBufferLoad<MyStruct>(g_output + uint64_t(0u));
    vk::RawBufferStore<MyStruct>(g_output + uint64_t(4u), (MyStruct)s1);
}

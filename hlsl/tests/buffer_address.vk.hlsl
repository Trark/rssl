struct InlineDescriptor0
{
    [[vk::offset(0)]] uint64_t g_input;
    [[vk::offset(8)]] uint64_t g_output;
};
[[vk::binding(0)]] ConstantBuffer<InlineDescriptor0> g_inlineDescriptor0;

static const uint64_t g_input = g_inlineDescriptor0.g_input;
static const uint64_t g_output = g_inlineDescriptor0.g_output;

struct MyStruct
{
    uint m;
};

void test() {
    const MyStruct s1 = vk::RawBufferLoad<MyStruct>(g_input + uint64_t(0u));
    const MyStruct s2 = vk::RawBufferLoad<MyStruct>(g_output + uint64_t(0u));
    vk::RawBufferStore<MyStruct>(g_output + uint64_t(4u), (MyStruct)s1);
}

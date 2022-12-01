struct InlineDescriptor0
{
    [[vk::offset(0)]] uint64_t g_byteBuffer;
    [[vk::offset(8)]] uint64_t g_byteBufferOutput;
};
[[vk::binding(10)]] ConstantBuffer<InlineDescriptor0> g_inlineDescriptor0;

static uint64_t g_byteBuffer = g_inlineDescriptor0.g_byteBuffer;
static uint64_t g_byteBufferOutput = g_inlineDescriptor0.g_byteBufferOutput;
[[vk::binding(0)]] extern Buffer<float4> g_texelBuffer;
[[vk::binding(1)]] extern RWBuffer<uint3> g_texelBufferOutput;
[[vk::binding(2)]] extern Texture2D<float4> g_texture;
[[vk::binding(3)]] extern RWTexture2D<float2> g_textureOutput;
[[vk::binding(4)]] extern StructuredBuffer<uint> g_primitiveStructuredBuffer;
[[vk::binding(5)]] extern RWStructuredBuffer<uint2> g_primitiveStructuredBufferOutput;

struct S
{
    float4x4 mat;
};

[[vk::binding(6)]] extern StructuredBuffer<S> g_complexStructuredBuffer;
[[vk::binding(7)]] extern RWStructuredBuffer<S> g_complexStructuredBufferOutput;
[[vk::binding(8)]] extern ConstantBuffer<S> g_cb;

[[vk::binding(9)]] cbuffer g_cb
{
    S s;
}

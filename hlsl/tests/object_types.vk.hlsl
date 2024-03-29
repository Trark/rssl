struct InlineDescriptor0
{
    [[vk::offset(0)]] uint64_t g_byteBuffer;
    [[vk::offset(8)]] uint64_t g_byteBufferOutput;
};

[[vk::binding(11)]] ConstantBuffer<InlineDescriptor0> g_inlineDescriptor0;
static const uint64_t g_byteBuffer = g_inlineDescriptor0.g_byteBuffer;
static const uint64_t g_byteBufferOutput = g_inlineDescriptor0.g_byteBufferOutput;
[[vk::binding(0)]] Buffer<float4> g_texelBuffer;
[[vk::binding(1)]] RWBuffer<uint3> g_texelBufferOutput;
[[vk::binding(2)]] Texture2D<float4> g_texture;
[[vk::binding(3)]] Texture2D<float4> g_textureCollection[3];
[[vk::binding(0, 1)]] RWTexture2D<float2> g_textureOutput;
[[vk::binding(1, 1)]] StructuredBuffer<uint> g_primitiveStructuredBuffer;
[[vk::binding(6)]] RWStructuredBuffer<uint2> g_primitiveStructuredBufferOutput;

struct S
{
    float4x4 mat;
};

[[vk::binding(7)]] StructuredBuffer<S> g_complexStructuredBuffer;
[[vk::binding(8)]] RWStructuredBuffer<S> g_complexStructuredBufferOutput;
[[vk::binding(9)]] ConstantBuffer<S> g_cb;

[[vk::binding(10)]] cbuffer g_cb
{
    S s;
}

extern ByteAddressBuffer g_byteBuffer : register(t0);
extern RWByteAddressBuffer g_byteBufferOutput : register(u1);
extern Buffer<float4> g_texelBuffer : register(t2);
extern RWBuffer<uint3> g_texelBufferOutput : register(u3);
extern Texture2D<float4> g_texture : register(t4);
extern RWTexture2D<float2> g_textureOutput : register(u5);
extern StructuredBuffer<uint> g_primitiveStructuredBuffer : register(t6);
extern RWStructuredBuffer<uint2> g_primitiveStructuredBufferOutput : register(u7);

struct S
{
    float4x4 mat;
};

extern StructuredBuffer<S> g_complexStructuredBuffer : register(t8);
extern RWStructuredBuffer<S> g_complexStructuredBufferOutput : register(u9);
extern ConstantBuffer<S> g_cb : register(b10);

cbuffer g_cb : register(b10)
{
    S s;
}

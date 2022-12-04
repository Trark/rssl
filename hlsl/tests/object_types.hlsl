ByteAddressBuffer g_byteBuffer : register(t0);
RWByteAddressBuffer g_byteBufferOutput : register(u1);
Buffer<float4> g_texelBuffer : register(t2);
RWBuffer<uint3> g_texelBufferOutput : register(u3);
Texture2D<float4> g_texture : register(t4);
RWTexture2D<float2> g_textureOutput : register(u5);
StructuredBuffer<uint> g_primitiveStructuredBuffer : register(t6);
RWStructuredBuffer<uint2> g_primitiveStructuredBufferOutput : register(u7);

struct S
{
    float4x4 mat;
};

StructuredBuffer<S> g_complexStructuredBuffer : register(t8);
RWStructuredBuffer<S> g_complexStructuredBufferOutput : register(u9);
ConstantBuffer<S> g_cb : register(b10);

cbuffer g_cb : register(b11)
{
    S s;
}

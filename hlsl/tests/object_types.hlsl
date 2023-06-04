ByteAddressBuffer g_byteBuffer : register(t0);
RWByteAddressBuffer g_byteBufferOutput : register(u1);
Buffer<float4> g_texelBuffer : register(t2);
RWBuffer<uint3> g_texelBufferOutput : register(u3);
Texture2D<float4> g_texture : register(t4);
RWTexture2D<float2> g_textureOutput : register(u0, space1);
StructuredBuffer<uint> g_primitiveStructuredBuffer : register(t1, space1);
RWStructuredBuffer<uint2> g_primitiveStructuredBufferOutput : register(u5);

struct S
{
    float4x4 mat;
};

StructuredBuffer<S> g_complexStructuredBuffer : register(t6);
RWStructuredBuffer<S> g_complexStructuredBufferOutput : register(u7);
ConstantBuffer<S> g_cb : register(b8);

cbuffer g_cb : register(b9)
{
    S s;
}

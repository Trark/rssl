extern ByteAddressBuffer g_byteBuffer;
extern RWByteAddressBuffer g_byteBufferOutput;
extern Buffer<float4> g_texelBuffer;
extern RWBuffer<uint3> g_texelBufferOutput;
extern Texture2D<float4> g_texture;
extern RWTexture2D<float2> g_textureOutput;
extern StructuredBuffer<uint> g_primitiveStructuredBuffer;
extern RWStructuredBuffer<uint2> g_primitiveStructuredBufferOutput;
struct S
{
    float4x4 mat;
};
extern StructuredBuffer<S> g_complexStructuredBuffer;
extern RWStructuredBuffer<S> g_complexStructuredBufferOutput;
extern ConstantBuffer<S> g_cb;

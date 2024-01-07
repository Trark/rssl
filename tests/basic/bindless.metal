namespace helper {

template<typename T>
metal::vec<T, 4> Load(metal::texture2d<T> texture, int3 location) {
    return texture.read(uint2(location.x, location.y), location.z);
}

struct ByteAddressBuffer
{
    device const uint8_t* address;
    uint64_t size;

    template<typename T>
    T Load(uint offset) const {
        return offset + sizeof(T) <= static_cast<uint>(size) ? *reinterpret_cast<device const T*>(address + offset) : T {};
    }
};

} // namespace helper

void CSMAIN(constant metal::array<const metal::texture2d<float>, 1024>& g_bindlessTexture2d, constant metal::array<const helper::ByteAddressBuffer, 1024>& g_bindlessByteBuffer) {
    metal::texture2d<float> tex = (metal::texture2d<float>)g_bindlessTexture2d[0u];
    helper::Load(tex, (int3)0u);
    helper::Load(g_bindlessTexture2d[1u], (int3)0u);
    helper::ByteAddressBuffer buf = (helper::ByteAddressBuffer)(helper::ByteAddressBuffer)g_bindlessByteBuffer[0u];
    ((helper::ByteAddressBuffer)g_bindlessByteBuffer[2u]).Load<uint>(0u);
}

struct ArgumentBuffer0
{
};

struct ArgumentBuffer1
{
    [[id(0)]] metal::array<const metal::texture2d<float>, 1024> g_bindlessTexture2d;
    [[id(1024)]] metal::array<const helper::ByteAddressBuffer, 1024> g_bindlessByteBuffer;
};

struct ArgumentBuffer2
{
    [[id(0)]] const metal::sampler g_linearSampler;
};

[[kernel]]
void ComputeShaderEntry(constant ArgumentBuffer0& set0 [[buffer(0)]], constant ArgumentBuffer1& set1 [[buffer(1)]], constant ArgumentBuffer2& set2 [[buffer(2)]]) {
    CSMAIN(set1.g_bindlessTexture2d, set1.g_bindlessByteBuffer);
}

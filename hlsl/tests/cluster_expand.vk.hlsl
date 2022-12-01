struct InlineDescriptor0
{
    [[vk::offset(0)]] uint64_t g_clusterInstanceData;
    [[vk::offset(8)]] uint64_t g_clusterData;
    [[vk::offset(16)]] uint64_t g_primitiveData;
    [[vk::offset(24)]] uint64_t g_indexData;
};
[[vk::binding(2)]] ConstantBuffer<InlineDescriptor0> g_inlineDescriptor0;

static uint64_t g_clusterInstanceData = g_inlineDescriptor0.g_clusterInstanceData;
static uint64_t g_clusterData = g_inlineDescriptor0.g_clusterData;
static uint64_t g_primitiveData = g_inlineDescriptor0.g_primitiveData;
static uint64_t g_indexData = g_inlineDescriptor0.g_indexData;
[[vk::binding(0)]] extern RWByteAddressBuffer g_indirectBuffer;

struct ClusterInstanceData
{
    uint instance_id;
    uint cluster_offset;
};

struct ClusterData
{
    uint primitive_base;
    uint vertex_base;
    uint primitive_count;
    uint vertex_count;
};

struct ConstantData
{
    uint instance_cluster_count;
    uint index_buffer_size;
};

[[vk::binding(1)]] extern ConstantBuffer<ConstantData> g_constantData;

uint LoadClusterIndex(ClusterData cluster, uint primitive_index_index) {
    uint offset = cluster.primitive_base + primitive_index_index;
    uint offset_low = offset & ~3u;
    uint byte_pos = offset & 3u;
    uint word = vk::RawBufferLoad<uint>(g_primitiveData + uint64_t(offset_low));
    uint cluster_index = word >> 8u * byte_pos & 255u;
    return cluster_index;
}

[numthreads(64, 1, 1)]
void CSMAIN(uint3 dispatchThreadID : SV_DispatchThreadID) {
    if (dispatchThreadID.x >= g_constantData.instance_cluster_count)
    {
        return;
    }
    ClusterInstanceData cluster_instance_data = vk::RawBufferLoad<ClusterInstanceData>(g_clusterInstanceData + uint64_t(dispatchThreadID.x * sizeof(ClusterInstanceData)));
    ClusterData cluster_data = vk::RawBufferLoad<ClusterData>(g_clusterData + uint64_t(cluster_instance_data.cluster_offset));
    uint writeLocation = 0u;
    g_indirectBuffer.InterlockedAdd(0u, cluster_data.primitive_count * 3u, writeLocation);
    for (uint i = 0u; i < cluster_data.primitive_count * 3u; ++i)
    {
        uint cluster_index = LoadClusterIndex(cluster_data, i);
        if (writeLocation + i < g_constantData.index_buffer_size)
        {
            vk::RawBufferStore<uint>(g_indexData + uint64_t(4u * (writeLocation + i)), dispatchThreadID.x << 8u | cluster_index);
        }
    }
}

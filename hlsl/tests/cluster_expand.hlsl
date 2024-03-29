ByteAddressBuffer g_clusterInstanceData : register(t0, space1);
ByteAddressBuffer g_clusterData : register(t1, space1);
ByteAddressBuffer g_primitiveData : register(t2, space1);
RWByteAddressBuffer g_indexData : register(u3, space1);
RWByteAddressBuffer g_indirectBuffer : register(u4, space1);

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

ConstantBuffer<ConstantData> g_constantData : register(b5, space1);

uint LoadClusterIndex(ClusterData cluster, uint primitive_index_index) {
    uint offset = cluster.primitive_base + primitive_index_index;
    uint offset_low = offset & ~3u;
    uint byte_pos = offset & 3u;
    uint word = g_primitiveData.Load<uint>(offset_low);
    uint cluster_index = word >> 8u * byte_pos & 255u;
    return cluster_index;
}

[numthreads(64, 1, 1)]
void CSMAIN(uint3 dispatchThreadID : SV_DispatchThreadID) {
    if (dispatchThreadID.x >= g_constantData.instance_cluster_count)
    {
        return;
    }
    ClusterInstanceData cluster_instance_data = g_clusterInstanceData.Load<ClusterInstanceData>(dispatchThreadID.x * sizeof(ClusterInstanceData));
    ClusterData cluster_data = g_clusterData.Load<ClusterData>(cluster_instance_data.cluster_offset);
    uint writeLocation = 0u;
    g_indirectBuffer.InterlockedAdd(0u, cluster_data.primitive_count * 3u, writeLocation);
    for (uint i = 0u; i < cluster_data.primitive_count * 3u; ++i)
    {
        uint cluster_index = LoadClusterIndex(cluster_data, i);
        if (writeLocation + i < g_constantData.index_buffer_size)
        {
            g_indexData.Store<uint>(4u * (writeLocation + i), dispatchThreadID.x << 8u | cluster_index);
        }
    }
}

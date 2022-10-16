extern ByteAddressBuffer g_clusterInstanceData : register(t0);
extern ByteAddressBuffer g_clusterData : register(t1);
extern ByteAddressBuffer g_primitiveData : register(t2);
extern RWByteAddressBuffer g_indexData : register(u3);
extern RWByteAddressBuffer g_indirectBuffer : register(u4);
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
extern ConstantBuffer<ConstantData> g_constantData : register(b5);
uint LoadClusterIndex(ClusterData cluster, uint primitive_index_index) {
    uint offset = (cluster.primitive_base) + (primitive_index_index);
    uint offset_low = (offset) & (~(3u));
    uint byte_pos = (offset) & (3u);
    uint word = g_primitiveData.Load(offset_low);
    uint cluster_index = ((word) >> (((uint)(8)) * (byte_pos))) & (255u);
    return cluster_index;
}
[numthreads(64, 1, 1)]
void CSMAIN(uint3 dispatchThreadID : SV_DispatchThreadID) {
    if (((dispatchThreadID).x) >= (g_constantData.instance_cluster_count))
    {
        return;
    }
    ClusterInstanceData cluster_instance_data = g_clusterInstanceData.Load<ClusterInstanceData>(((dispatchThreadID).x) * (sizeof(ClusterInstanceData)));
    ClusterData cluster_data = g_clusterData.Load<ClusterData>(cluster_instance_data.cluster_offset);
    uint writeLocation = (uint)(0);
    g_indirectBuffer.InterlockedAdd((uint)(0), (cluster_data.primitive_count) * ((uint)(3)), writeLocation);
    for (uint i = (uint)(0); (i) < ((cluster_data.primitive_count) * ((uint)(3))); ++(i))
    {
        uint cluster_index = LoadClusterIndex(cluster_data, i);
        if (((writeLocation) + (i)) < (g_constantData.index_buffer_size))
        {
            g_indexData.Store(((uint)(4)) * ((writeLocation) + (i)), (((dispatchThreadID).x) << ((uint)(8))) | (cluster_index));
        }
    }
}

Buffer<uint> g_input : register(t0);
RWBuffer<uint> g_output : register(u1);

void test() {
    uint outInt;
    const uint load_srv = g_input.Load(0);
    const uint load_uav = g_output.Load(0);
    const uint load_srv_sparse = g_input.Load(0, outInt);
    const uint load_uav_sparse = g_output.Load(0, outInt);
    g_input.GetDimensions(outInt);
    g_output.GetDimensions(outInt);
    g_output[2u] = g_output[1u] = (uint)g_input[1u];
}

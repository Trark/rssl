struct Payload
{
    uint start_location;
};

[numthreads(64, 1, 1)]
void TaskShader(uint thread_id : SV_DispatchThreadID) {
    Payload data;
    data.start_location = thread_id.x;
    DispatchMesh(4u, 1u, 1u, data);
}

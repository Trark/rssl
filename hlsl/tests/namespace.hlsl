namespace N1 {

struct S
{
    uint x;
};

extern StructuredBuffer<N1::S> g_buffer1;
extern StructuredBuffer<N1::S> g_buffer2;

} // namespace N1

namespace N2 {

struct S
{
    uint x;
};

extern StructuredBuffer<N2::S> g_buffer1;
extern StructuredBuffer<N2::S> g_buffer2;

} // namespace N2

extern StructuredBuffer<N1::S> g_buffer1;
extern StructuredBuffer<N2::S> g_buffer2;

namespace N1 {

extern StructuredBuffer<N1::S> g_buffer3;
extern StructuredBuffer<N1::S> g_buffer4;

} // namespace N1

namespace N1 {

struct S
{
    uint x;

    void f() {
        f();
    }
};

StructuredBuffer<N1::S> g_buffer1;
StructuredBuffer<N1::S> g_buffer2;

void f() {}

} // namespace N1

namespace N2 {

struct S
{
    uint x;
};

StructuredBuffer<N2::S> g_buffer1;
StructuredBuffer<N2::S> g_buffer2;

void f() {
    N1::f();
}

} // namespace N2

StructuredBuffer<N1::S> g_buffer1;
StructuredBuffer<N2::S> g_buffer2;

namespace N1 {

StructuredBuffer<N1::S> g_buffer3;
StructuredBuffer<N1::S> g_buffer4;

void g() {
    N1::f();
    N1::S s;
    s.f();
}

} // namespace N1

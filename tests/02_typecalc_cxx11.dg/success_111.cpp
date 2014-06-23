/*
<testinfo>
test_generator=config/mercurium-cxx11
</testinfo>
*/
namespace B
{
    enum A
    {
        V = 1
    };
}

// This just tests a workaround for <g++4.10
void f()
{
    using B::A::V;
}

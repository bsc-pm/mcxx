/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/

namespace std {
    typedef unsigned long size_t;
}

template <typename T>
struct A
{
    typedef std::size_t size_t;

    void f(size_t x = static_cast<size_t>(1024));
};

void g()
{
    A<int> a;

    a.f();
}

/*
<testinfo>
test_generator="config/mercurium-cxx11"
</testinfo>
*/

template <typename T>
struct A
{
    void g1() noexcept(A::f())
    {
    }

    void g2() noexcept(A::f());

    static bool f()
    {
    }
};

void G()
{
    A<int> m;
    m.g2();
}

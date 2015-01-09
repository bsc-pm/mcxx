/*
<testinfo>
test_generator="config/mercurium-cxx11"
</testinfo>
*/

template <typename T>
struct A
{
    constexpr static bool f()
    {
        return sizeof(T) <= sizeof(int);
    }

    void g1() noexcept(A::f())
    {
    }

    void g2() noexcept(A::f());
};

void G()
{
    A<int> m;
    m.g2();
}

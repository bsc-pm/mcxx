/*
<testinfo>
test_generator=config/mercurium-cxx11
</testinfo>
*/
template <typename T>
struct A
{
    T x;
    constexpr A() : x(4) { }
};

template <typename T>
struct B1
{
    A<T> a;
    constexpr B1() = default;
};

void f()
{
    static_assert(B1<int>().a.x == 4, "Invalid expression");
}

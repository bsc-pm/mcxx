/*
<testinfo>
test_generator=config/mercurium-cxx11
</testinfo>
*/
template <typename T>
struct A
{
    int x;
    constexpr A() : x(3) { }
};

template <typename T>
struct B : A<T>
{
    int y;
    constexpr B() : y(4) { }
};

void g()
{
    static_assert(B<int>().x + B<int>().y == 7, "Invalid value");
}

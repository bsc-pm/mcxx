/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/
template <typename T1, typename S>
struct A
{
};

template <typename T2>
struct A<T2, typename T2 :: X>
{
    typedef int X;
};

template <>
struct A<int, float>
{
    typedef int Y;
};

struct B
{
    typedef int X;
};

void f()
{
    A<B, int>::X x;
    A<int, float>::Y y;
}

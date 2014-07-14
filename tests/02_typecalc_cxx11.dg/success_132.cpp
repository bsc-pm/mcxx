/*
<testinfo>
test_generator=config/mercurium-cxx11
</testinfo>
*/

template <typename T>
struct A
{
    T n;
    constexpr A(T m1, T m2) : n(m1 + m2) { }
};

template <typename T>
struct B : A<T>
{
    using A<T>::A;
};

void f()
{
    static_assert(B<int>(3, 4).n == 7, "Invalid value");
}

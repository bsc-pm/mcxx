/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/

template <typename T>
struct A;

template <>
struct A<unsigned long>
{
    void xul();
};

template <>
struct A<long>
{
    void xl();
};

template <>
struct A<unsigned long long>
{
    void xull();
};

template <>
struct A<long long>
{
    void xll();
};

template <typename T>
A<T> f(T)
{
}

void g()
{
    f(0LL).xll();
    f(0ULL).xull();

    f(0L).xl();
    f(0UL).xul();
}

/*
<testinfo>
test_generator=config/mercurium-cxx11
</testinfo>
*/

template <typename ...S>
struct A
{
};

template <>
struct A<>
{
    typedef int Type0;
};

template <typename T>
struct A<T>
{
    typedef int Type1;
};

template <typename T, typename ...S>
struct A<T, S...>
{
};

template <typename T, typename S>
struct A<T, S>
{
    typedef int Type2;
};

template <typename T, typename S, typename ...Q>
struct A<T, S, Q...>
{
};

template <typename T, typename S, typename Q>
struct A<T, S, Q>
{
    typedef int Type3;
};

template <typename T, typename S, typename K, typename ...Q>
struct A<T, S, K, Q...>
{
    typedef int Type4orMore;
};

void f()
{
    A<>::Type0 t0;
    A<int>::Type1 t1;
    A<int, int>::Type2 t2;
    A<int, int, int>::Type3 t3;
    A<int, int, int, int>::Type4orMore t4;
}


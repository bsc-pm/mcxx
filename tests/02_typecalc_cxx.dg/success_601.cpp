/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/

template <typename T>
struct A
{
    template <int I>
        struct B
        {
            typedef T(&Type)[I];
        };

    template <int I>
    typename B<I>::Type foo1();

    template <int I>
    typename A<T>::template B<I>::Type foo2();
};

template<typename T>
template<int I>
inline typename A<T>::template B<I>::Type
A<T>::foo1()
{
}

template<typename T>
template<int I>
inline typename A<T>::template B<I>::Type
A<T>::foo2()
{
}

void g()
{
    A<int> a;

    int (&c)[3] = a.foo1<3>();
    int (&d)[3] = a.foo2<3>();
}

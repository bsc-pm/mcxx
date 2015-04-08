/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/
template <typename T>
struct A
{
    struct B
    {
        T x;
    };
    static B c[];
};

template <typename T1>
typename A<T1>::B A<T1>::c[sizeof(T1)*2];

template <int N, int M> struct Eq;
template <int N> struct Eq<N, N> { typedef int Type; };

void f()
{
    A<double> a;
    Eq<sizeof(a.c), 2*sizeof(double)*sizeof(double)>::Type t;
}

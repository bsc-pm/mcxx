template <int _N1, typename _T>
struct A
{
};

template <int _N2>
struct A<_N2, int>
{
    typedef int T1;
};

template <int _N3>
struct A<_N3, const int>
{
    typedef int T2;
};

template <int _N4>
struct A<_N4, const volatile int>
{
    typedef int T3;
};

A<3, int>::T1 t1;
A<3, const int>::T2 t2;
A<3, const volatile int>::T3 t3;

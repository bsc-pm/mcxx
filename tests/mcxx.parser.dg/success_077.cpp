template <int _N1, typename _T1>
struct A
{
    typedef _T1 T1;
};

template <int _N2, typename _T2>
struct A<_N2, const _T2>
{
    typedef _T2 T2;
};

template <int _N3, typename _T3>
struct A<_N3, volatile _T3>
{
    typedef _T3 T3;
};

template <int _N4, typename _T4>
struct A<_N4, const volatile _T4>
{
    typedef _T4 T4;
};

A<3, int>::T1 t1;
A<3, const int>::T2 t2;
A<3, volatile int>::T3 t3;
A<3, const volatile int>::T4 t4;



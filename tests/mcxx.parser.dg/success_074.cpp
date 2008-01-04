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


A<3, int>::T1 T1;
A<3, const int>::T2 T2;

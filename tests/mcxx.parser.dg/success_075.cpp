template <typename _T1, int _N1>
struct A
{
    typedef _T1 *K1;
};

template <typename _T2, int _N2>
struct A<const _T2, _N2>
{
    typedef _T2 K2;
};

A<int, 3>::K1 k1;
A<const int, 3>::K2 k2;

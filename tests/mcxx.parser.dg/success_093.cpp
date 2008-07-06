template <typename _T5, typename _S5, typename _P5>
struct A
{
    typedef _P5 T;
};

template <typename _T1, typename _S1, typename _P1>
struct A;

template <typename _T2, typename _S2, typename _P2 = _S2*>
struct A;

template <typename _T3, typename _S3 = _T3*, typename _P3>
struct A;

template <typename _T4 = int, typename _S4, typename _P4>
struct A;

A<> t;

A<float> t2;

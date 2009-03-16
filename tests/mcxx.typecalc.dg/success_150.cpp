template <typename _T1, typename _T2>
struct A
{
    typedef __typeof__(_T1() + _T2()) T;

    T f();
};

template <typename _Q1, typename _Q2>
typename A<_Q1, _Q2>::T A<_Q1, _Q2>::f()
{
}

/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/
template<int W, bool S, bool C = !!W > struct A;

template<int W, bool S, bool C >
struct A
{
    A(int);

    static A ctor(int);

    template<int W1, bool S1>
        A(A<W1, S1, true>& that, int = 1);

    template<int W2, bool S2>
        A(A<W2, S2>& that, float = 2.3f);
};

A<1+8, true> f(A<1 + 8, false>& a)
{
    return a;
}

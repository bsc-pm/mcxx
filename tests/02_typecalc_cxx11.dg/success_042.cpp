/*
<testinfo>
test_generator="config/mercurium-cxx11"
</testinfo>
*/

template <bool A, typename T0, typename ...Rest0>
struct B
{
    typedef int *Type;
};

template <typename T1, typename ...Rest1>
struct B<true, T1, Rest1...>
{
    typedef float *Type;
};

template <typename T2, typename S2, typename ...Rest2>
struct B<true, T2, S2, Rest2...>
{
    typedef double *Type;
};

void g()
{
    int *pi;
    B<false, int>::Type p1;
    p1 == pi;

    float *pf;
    B<true, int>::Type p2;
    B<true, float>::Type p3;
    pf == p2;
    pf == p3;

    double *pd;
    B<true, int, int>::Type p4;
    pd == p4;
}

/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/

namespace N1
{
    template <typename T0, int N0 = sizeof(T0)>
        struct A { };

    template <typename T1>
        float* f(A<T1, 4>& a, int = 1);

    template <typename T2>
        int* f(A<T2>& a, float = 2.3f);

    void g()
    {
        A<int> a;
        float *pf = f(a);
    }
}

namespace N2
{
    template <typename T0, typename T1 = typename T0::K>
        struct A { };

    template <typename T1>
        float* f(A<T1, int>& a, int = 1);

    template <typename T2>
        int* f(A<T2>& a, float = 2.3f);

    struct B { typedef int K; };

    void g()
    {
        A<B> a;
        float* pf = f(a);
    }
}

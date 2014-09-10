/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/

namespace N2
{

    template <typename T1>
    float* f(T1, typename T1::K, int = 1);

    template <typename T2>
    int* f(T2, int,            float = 2.3f);

    struct B { typedef int K; };

    void g()
    {
        B b;
        int *p = f<B>(b, 0);
    }
}


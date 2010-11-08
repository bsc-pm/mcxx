/*
<testinfo>
test_generator=config/mercurium-omp
</testinfo>
*/
#include <stdlib.h>

template <typename Q>
struct A
{
    template <typename T>
    void foo(int n);
};

struct C
{
    int n;
    C() : n(0) { }
};
C e;

template <typename M>
template <typename S>
void A<M>::foo(int n)
{
#pragma omp parallel shared(e) firstprivate(n)
    {
#pragma omp critical
        {
            e.n = n;
        }
    }
}

int main(int argc, char *argv[])
{
    A<int> a;

    a.foo<float>(4);

    if (e.n != 4)
        abort();

    return 0;
}

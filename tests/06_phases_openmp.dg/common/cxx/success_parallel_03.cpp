/*
<testinfo>
test_generator=config/mercurium-omp
</testinfo>
*/
#include <stdlib.h>

template <typename T>
struct A
{
    void foo(int n);
};

struct C
{
    int n;
    C() : n(0) { }
};
C e;

template <typename T>
void A<T>::foo(int n)
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

    a.foo(4);

    if (e.n != 4)
        abort();

    return 0;
}

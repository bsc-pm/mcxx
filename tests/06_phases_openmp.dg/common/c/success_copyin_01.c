/*
<testinfo>
test_generator=config/mercurium-omp
</testinfo>
*/
#include <stdlib.h>
#include "omp.h"

int d;
#pragma omp threadprivate(d)

void f1(void)
{
    d = 10;
    int *p = &d;
#pragma omp parallel copyin(d) firstprivate(p)
    {
        if (p == &d
                && omp_get_thread_num() != 0)
            abort();

        if (d != 10)
            abort();
    }
}

int c[2];
#pragma omp threadprivate(c)

void f2(void)
{
    int *p = c;
    c[1] = 10;
#pragma omp parallel copyin(c) firstprivate(p)
    {
        if (p == c
                && omp_get_thread_num() != 0)
            abort();

        if (c[1] != 10)
            abort();
    }
}

int main(int argc, char *argv[])
{
    f1();
    f2();
    return 0;
}

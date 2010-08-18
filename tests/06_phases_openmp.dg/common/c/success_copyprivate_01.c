/*
<testinfo>
test_generator=config/mercurium-omp
</testinfo>
*/
#include <stdlib.h>

int d;
#pragma omp threadprivate(d)

void f1(void)
{
    d = 4;
#pragma omp parallel
    {
#pragma omp single copyprivate(d)
        {
            d = d + 3;
        }

        if (d != 7)
            abort();
    }
}

int c[2];
#pragma omp threadprivate(c)

void f2(void)
{
    c[1] = 4;

#pragma omp parallel
    {
#pragma omp single copyprivate(c)
        {
            c[1] = c[1] + 3;
        }

        if (c[1] != 7)
            abort();
    }
}

int main(int argc, char *argv[])
{
    f1();
    f2();
    return 0;
}

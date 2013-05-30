/*
<testinfo>
test_generator=config/mercurium-ompss
</testinfo>
*/

#include <stdlib.h>

int x;

void f1(void)
{
#pragma omp task inout(*x)
    void g(int *x, int c);

    x = 3;

    g(&x, 1);
}

void f2(void)
{
#pragma omp task in(*x)
    void g(int *x, int c);

    g(&x, 2);
}

void g(int *x, int c)
{
    if (c == 1)
    {
        if (*x != 3) abort();
        (*x)++;
    }
    else if (c == 2)
    {
        if (*x != 4) abort();
    }
}

int main(int argc, char *argv[])
{
    f1();
    f2();

#pragma omp taskwait

    return 0;
}

/*
<testinfo>
test_generator=config/mercurium-ompss
</testinfo>
*/

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#pragma omp target device(smp) copy_deps
#pragma omp task inout(a[0;10])
void foo1(int* a)
{
    fprintf(stderr, "foo1\n");
    for (int i = 0; i < 10; i++) 
        a[i] = i;
}

#pragma omp target device(smp) copy_deps
#pragma omp task in(a[0;10])
void foo2(int* a)
{
    fprintf(stderr, "foo2\n");
    for (int i = 0; i < 10; i++) 
    {
        if (a[i] != i) abort();
    }
}

int main (int argc, char*argv)
{
    int v[10];
    memset(v, 0, sizeof(v));

    for (int j = 0; j < 10; j++)
    {
        foo1(&v[0]);
        foo2(v);
    }

#pragma omp taskwait
    return 0;
}

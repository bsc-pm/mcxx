/*
<testinfo>
test_generator=config/mercurium-ompss
</testinfo>
*/
#include <stdio.h>
#include <stdlib.h>

#pragma omp task depend(inout: x[n])
void f(int n, int *x)
{
    x[n]++;
}

int z[1000] ;

int main(int argc, char *argv[])
{
    int i;
    for (i = 0; i < 5; i++)
        z[i] = i;

    for (i = 0; i < 5; i++)
    {
        f(i, z);
    }

#pragma omp taskwait

    for (i = 0; i < 5; i++)
    {
        if (z[i] != (i+1))
        {
            fprintf(stderr, "Invalid position z[%d], should be %d it is %d\n", i, i+1, z[i]);
            abort();
        }
    }

    return 0;
}

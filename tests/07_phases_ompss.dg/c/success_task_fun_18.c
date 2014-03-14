/*
<testinfo>
test_generator=config/mercurium-ompss
</testinfo>
*/

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#pragma omp task copy_inout([n]f)
void foo_failure(int *f, int n)
{
    int i;
    for (i = 0; i < n; i++)
    {
        f[i] = i + 1;
    }
}

int main(int argc, char *argv[])
{
    int n = 20;
    int v[n];

    memset(v, 0, sizeof(v));

    foo_failure(v, n);
#pragma omp taskwait

    int i;
    for (i = 0; i < n; i++)
    {
        if (v[i] != (i + 1))
        {
            fprintf(stderr, "v[%d] == %d but should be %d\n", i, v[i], i + 1);
            abort();
        }
    }

    return 0;
}


/*
<testinfo>
test_generator=(config/mercurium-ompss "config/mercurium-ompss-2 openmp-compatibility --variable=disable_final_clause_transformation:1")
</testinfo>
*/

#include <assert.h>
#include <stdlib.h>

void foo(int n, int *v)
{
#ifdef __NANOS6__
    #pragma oss task weakreduction(+: [n]v) final(1)
#else
    #pragma omp task reduction(+: [n]v) final(1)
#endif
    {
        #pragma omp task reduction(+: [n]v) final(1)
        {
            for(int i = 0; i < n; ++i)
                v[i]++;
        }

#ifndef __NANOS6__
        #pragma omp taskwait
#endif
    }

    #pragma omp task reduction(+: [n]v) final(1)
    {
        for(int i = 0; i < n; ++i)
            v[i]++;
    }

#ifndef __NANOS6__
    #pragma omp taskwait
#endif
}

int main()
{
    int i, *v;

    #pragma omp task out(v) final(1)
    {
        v = (int*)malloc(10*sizeof(int));
        for (int i = 0; i < 10; ++i)
        {
            v[i] = i;
        }
    }

    #pragma omp taskwait in(v)

#ifdef __NANOS6__
    #pragma oss task weakreduction(+: [10]v) final(1)
#else
    #pragma omp task reduction(+: [10]v) final(1)
#endif
    foo(10, v);

    foo(10, v);

    #pragma omp task in([10]v) final(1)
    {
        int sum = 0;
        for(i = 0; i < 10; ++i)
            sum += v[i];

        assert(sum == 85);
    }

    return 0;
}

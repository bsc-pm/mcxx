/*
<testinfo>
test_generator=(config/mercurium-ompss)
</testinfo>
*/
#include <assert.h>

int main()
{
    int x = 0;

#ifdef __NANOS6__
    #pragma oss task weakreduction(+: x) final(1)
#else
    #pragma omp task reduction(+: x) final(1)
#endif
    {
#ifdef __NANOS6__
        #pragma oss task reduction(+: x)
#endif
        x++;

        #pragma omp task reduction(+: x)
        {
            x++;
        }

        #pragma omp taskwait
    }

    #pragma omp taskwait

    assert(x == 2);
}

/*
<testinfo>
test_generator=(config/mercurium-ompss "config/mercurium-ompss-2 openmp-compatibility")
</testinfo>
*/
#include <assert.h>

int main()
{
    int x;

    #pragma omp task reduction(+: x) final(1)
    {
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

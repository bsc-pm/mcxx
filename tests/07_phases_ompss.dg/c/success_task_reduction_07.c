/*
<testinfo>
test_generator=(config/mercurium-ompss)
</testinfo>
*/
#include <assert.h>

int main()
{
    int x = 0;

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
